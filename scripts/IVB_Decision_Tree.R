############################################################
### 0. LIBRARIES
############################################################

library(tidyverse)
library(sf)
library(tidymodels)
tidymodels_prefer()

library(rpart)
library(rpart.plot)
library(randomForest)
library(tidycensus)

############################################################
### 1. LOAD DATA
############################################################

snap_raw <- read_csv("scripts/hennepin_snap_mfip_tract_reva.csv", show_col_types = FALSE)

tracts <- st_read("scripts/tl_2020_27_tract/tl_2020_27_tract.shp", quiet = TRUE)
municipalities <- st_read("scripts/tl_2020_27_place/tl_2020_27_place.shp", quiet = TRUE)

############################################################
### 2. CLEAN SNAP DATA
############################################################

snap_clean <- snap_raw |>
  filter(!is.na(GEOID)) |>
  select(GEOID, NAME, YearMonth, SNAP_people, MFIP_people, p_125_povertyE) |>
  mutate(
    GEOID = sprintf("%011.0f", as.numeric(GEOID)),
    year = YearMonth %/% 100
  ) |>
  filter(year >= 2022, year <= 2025) |>
  mutate(
    SNAP_people = na_if(SNAP_people, "Low count"),
    MFIP_people = na_if(MFIP_people, "Low count"),
    SNAP_people = na_if(SNAP_people, "<10"),
    MFIP_people = na_if(MFIP_people, "<10"),
    SNAP_people = as.numeric(SNAP_people),
    MFIP_people = as.numeric(MFIP_people),
    p_125_povertyE = as.numeric(p_125_povertyE)
  ) |>
  mutate(
    enrolled = coalesce(SNAP_people, 0) + coalesce(MFIP_people, 0),
    eligible = p_125_povertyE,
    gap_count = pmax(eligible - enrolled, 0),
    gap_rate = if_else(eligible > 0, gap_count / eligible, NA_real_)
  )

############################################################
### 3. TRACT SUMMARY
############################################################

tract_summary <- snap_clean |>
  group_by(GEOID) |>
  summarise(
    tract_name = first(na.omit(NAME)),
    enrolled = median(enrolled, na.rm = TRUE),
    eligible = median(eligible, na.rm = TRUE),
    gap_count = median(gap_count, na.rm = TRUE),
    gap_rate = median(gap_rate, na.rm = TRUE),
    .groups = "drop"
  )

############################################################
### 4. ASSIGN MUNICIPALITIES (PROJECTED FOR CLEANER CENTROIDS)
############################################################

tracts <- tracts |>
  filter(COUNTYFP == "053") |>
  mutate(GEOID = as.character(GEOID)) |>
  left_join(tract_summary, by = "GEOID")

assign_municipality <- function(sf_object, municipalities_sf) {
  
  original_sf <- sf_object
  
  sf_proj <- sf_object |> st_transform(26915)
  muni_proj <- municipalities_sf |> st_transform(26915)
  
  centroids <- st_centroid(sf_proj)
  
  joined <- st_join(
    centroids,
    muni_proj |> select(NAME),
    join = st_within
  )
  
  lookup <- joined |>
    st_drop_geometry() |>
    select(GEOID, municipality = NAME.y) |>
    distinct()
  
  original_sf |>
    left_join(lookup, by = "GEOID") |>
    mutate(
      municipality = if_else(is.na(municipality), "Unassigned", municipality)
    )
}

tracts <- assign_municipality(tracts, municipalities)

tract_master <- tracts |>
  st_drop_geometry()

############################################################
### 5. BUILD MAP-ALIGNED CORE TARGET FIELDS
############################################################

tract_master <- tract_master |>
  group_by(municipality) |>
  mutate(
    city_gap_rate = median(gap_rate, na.rm = TRUE),
    muni_n_tracts = n()
  ) |>
  ungroup() |>
  mutate(
    diff_from_city = gap_rate - city_gap_rate
  ) |>
  filter(municipality != "Unassigned")

############################################################
### 6. ACS PREDICTORS
############################################################

acs <- get_acs(
  geography = "tract",
  variables = c(
    median_income = "B19013_001",
    total_pop = "B01003_001",
    poverty = "B17001_002",
    white = "B02001_002",
    black = "B02001_003",
    hispanic = "B03003_003",
    educ_total = "B15003_001",
    bachelors = "B15003_022",
    age_18_24 = "B01001_007",
    unemployed = "B23025_005",
    labor_force = "B23025_003",
    renters = "B25003_003",
    total_housing = "B25003_001"
  ),
  state = "MN",
  county = "Hennepin",
  year = 2022,
  survey = "acs5"
)

acs_wide <- acs |>
  select(GEOID, variable, estimate) |>
  pivot_wider(names_from = variable, values_from = estimate) |>
  mutate(
    pct_poverty = if_else(total_pop > 0, poverty / total_pop, NA_real_),
    pct_black = if_else(total_pop > 0, black / total_pop, NA_real_),
    pct_hispanic = if_else(total_pop > 0, hispanic / total_pop, NA_real_),
    pct_white = if_else(total_pop > 0, white / total_pop, NA_real_),
    pct_bachelors = if_else(educ_total > 0, bachelors / educ_total, NA_real_),
    pct_age_18_24 = if_else(total_pop > 0, age_18_24 / total_pop, NA_real_),
    pct_unemployed = if_else(labor_force > 0, unemployed / labor_force, NA_real_),
    pct_renters = if_else(total_housing > 0, renters / total_housing, NA_real_)
  )

############################################################
### 7. FINAL BASE MODEL DATA
############################################################

model_base <- tract_master |>
  left_join(acs_wide, by = "GEOID") |>
  drop_na(
    diff_from_city,
    pct_poverty, pct_black, pct_hispanic,
    pct_bachelors, median_income,
    pct_age_18_24, pct_unemployed, pct_renters
  )

############################################################
### 8. SETTINGS TO TEST
############################################################

target_quantiles <- c(0.80, 0.85, 0.90, 0.95)   # top 20, 15, 10, 5
mtry_values <- c(2, 3, 4)
prob_cutoffs <- c(0.30, 0.35, 0.40)

predictor_vars <- c(
  "pct_poverty", "pct_black", "pct_hispanic",
  "pct_bachelors", "median_income",
  "pct_age_18_24", "pct_unemployed", "pct_renters"
)

model_formula <- as.formula(
  paste("target ~", paste(predictor_vars, collapse = " + "))
)

############################################################
### 9. THRESHOLD / FOREST SEARCH
############################################################

search_results <- tibble(
  target_quantile = numeric(),
  target_label = character(),
  diff_cutoff = numeric(),
  n_false = numeric(),
  n_true = numeric(),
  prevalence = numeric(),
  mtry = numeric(),
  prob_cutoff = numeric(),
  accuracy = numeric(),
  sensitivity = numeric(),
  specificity = numeric(),
  bal_accuracy = numeric()
)
for (q in target_quantiles) {
  
  cutoff_value <- quantile(model_base$diff_from_city, q, na.rm = TRUE)
  
  model_data <- model_base |>
    mutate(
      target = factor(
        diff_from_city >= cutoff_value,
        levels = c(FALSE, TRUE),
        labels = c("FALSE", "TRUE")
      )
    )
  
  set.seed(1501)
  data_split <- initial_split(model_data, prop = 0.8, strata = target)
  
  train <- training(data_split)
  test  <- testing(data_split)
  
  train_balanced <- train |>
    group_by(target) |>
    slice_sample(n = max(table(train$target)), replace = TRUE) |>
    ungroup()
  
  n_true_total <- sum(model_data$target == "TRUE")
  n_false_total <- sum(model_data$target == "FALSE")
  prevalence <- mean(model_data$target == "TRUE")
  
  for (m in mtry_values) {
    
    set.seed(1501)
    
    mod_forest <- rand_forest(
      mode = "classification",
      mtry = m,
      trees = 1000
    ) |>
      set_engine("randomForest", importance = TRUE) |>
      fit(model_formula, data = train_balanced)
    
    prob_preds <- test |>
      select(GEOID, target) |>
      bind_cols(predict(mod_forest, test, type = "prob"))
    
    for (pc in prob_cutoffs) {
      
      eval_preds <- prob_preds |>
        mutate(
          pred_class = factor(
            if_else(.pred_TRUE >= pc, "TRUE", "FALSE"),
            levels = c("FALSE", "TRUE")
          )
        )
      
      acc_val <- accuracy(eval_preds, truth = target, estimate = pred_class)$.estimate
      sens_val <- sens(eval_preds, truth = target, estimate = pred_class, event_level = "second")$.estimate
      spec_val <- spec(eval_preds, truth = target, estimate = pred_class, event_level = "second")$.estimate
      bal_acc_val <- (sens_val + spec_val) / 2
      
      search_results <- search_results |>
        add_row(
          target_quantile = q,
          target_label = paste0("top_", round((1 - q) * 100), "_pct"),
          diff_cutoff = cutoff_value,
          n_false = n_false_total,
          n_true = n_true_total,
          prevalence = prevalence,
          mtry = m,
          prob_cutoff = pc,
          accuracy = acc_val,
          sensitivity = sens_val,
          specificity = spec_val,
          bal_accuracy = bal_acc_val
        )
    }
  }
}

############################################################
### 10. REVIEW SEARCH RESULTS
############################################################

search_results |>
  arrange(desc(bal_accuracy), desc(sensitivity), desc(specificity), desc(target_quantile))

best_settings <- search_results |>
  arrange(desc(bal_accuracy), desc(sensitivity), desc(specificity), desc(target_quantile)) |>
  slice(1)

best_settings

############################################################
### 11. REFIT FINAL DATA WITH WINNING TARGET THRESHOLD
############################################################

best_q <- best_settings$target_quantile[[1]]
best_mtry <- best_settings$mtry[[1]]
best_prob_cutoff <- best_settings$prob_cutoff[[1]]
best_diff_cutoff <- best_settings$diff_cutoff[[1]]

final_data <- model_base |>
  mutate(
    target = factor(
      diff_from_city >= best_diff_cutoff,
      levels = c(FALSE, TRUE),
      labels = c("FALSE", "TRUE")
    )
  )

set.seed(1501)
final_split <- initial_split(final_data, prop = 0.8, strata = target)

final_train <- training(final_split)
final_test  <- testing(final_split)

final_train_balanced <- final_train |>
  group_by(target) |>
  slice_sample(n = max(table(final_train$target)), replace = TRUE) |>
  ungroup()

############################################################
### 12. FINAL DECISION TREE
############################################################

final_tree <- decision_tree(
  mode = "classification",
  cost_complexity = 0.005,
  tree_depth = 4,
  min_n = 10
) |>
  set_engine("rpart") |>
  fit(model_formula, data = final_train_balanced)

rpart.plot::rpart.plot(final_tree$fit, roundint = FALSE)

final_tree_preds <- final_test |>
  bind_cols(predict(final_tree, final_test)) |>
  rename(pred_class = .pred_class)

tree_accuracy <- accuracy(final_tree_preds, truth = target, estimate = pred_class)
tree_sensitivity <- sens(final_tree_preds, truth = target, estimate = pred_class, event_level = "second")
tree_specificity <- spec(final_tree_preds, truth = target, estimate = pred_class, event_level = "second")
tree_bal_accuracy <- (tree_sensitivity$.estimate + tree_specificity$.estimate) / 2

tree_conf_mat <- final_tree_preds |>
  conf_mat(truth = target, estimate = pred_class)

tree_accuracy
tree_sensitivity
tree_specificity
tree_bal_accuracy
tree_conf_mat

############################################################
### 13. FINAL RANDOM FOREST
############################################################

set.seed(1501)

final_forest <- rand_forest(
  mode = "classification",
  mtry = best_mtry,
  trees = 1000
) |>
  set_engine("randomForest", importance = TRUE) |>
  fit(model_formula, data = final_train_balanced)

final_forest_preds <- final_test |>
  select(GEOID, target) |>
  bind_cols(predict(final_forest, final_test, type = "prob")) |>
  mutate(
    pred_class = factor(
      if_else(.pred_TRUE >= best_prob_cutoff, "TRUE", "FALSE"),
      levels = c("FALSE", "TRUE")
    )
  )

forest_accuracy <- accuracy(final_forest_preds, truth = target, estimate = pred_class)
forest_sensitivity <- sens(final_forest_preds, truth = target, estimate = pred_class, event_level = "second")
forest_specificity <- spec(final_forest_preds, truth = target, estimate = pred_class, event_level = "second")
forest_bal_accuracy <- (forest_sensitivity$.estimate + forest_specificity$.estimate) / 2

forest_conf_mat <- final_forest_preds |>
  conf_mat(truth = target, estimate = pred_class)

forest_accuracy
forest_sensitivity
forest_specificity
forest_bal_accuracy
forest_conf_mat

############################################################
### 14. FINAL VARIABLE IMPORTANCE
############################################################

importance_tbl <- randomForest::importance(final_forest$fit)
importance_tbl

randomForest::varImpPlot(final_forest$fit)

############################################################
### 15. CLEAN SUMMARY TABLES
############################################################

final_model_summary <- tibble(
  model = c("Decision Tree", "Random Forest"),
  accuracy = c(tree_accuracy$.estimate, forest_accuracy$.estimate),
  sensitivity = c(tree_sensitivity$.estimate, forest_sensitivity$.estimate),
  specificity = c(tree_specificity$.estimate, forest_specificity$.estimate),
  bal_accuracy = c(tree_bal_accuracy, forest_bal_accuracy)
)

final_model_summary

best_settings

search_results |>
  group_by(target_label) |>
  arrange(desc(bal_accuracy), desc(sensitivity), desc(specificity), .by_group = TRUE) |>
  slice(1) |>
  ungroup()


############################################################
### 16. CLEAN MODEL VISUAL OUTPUTS
############################################################

cat("\n==============================\n")
cat(" FINAL DECISION TREE \n")
cat("==============================\n\n")

# Clean, readable tree
rpart.plot::rpart.plot(
  final_tree$fit,
  type = 2,                # split labels below nodes
  extra = 104,             # shows prob + % of obs
  under = TRUE,
  faclen = 0,              # don't abbreviate variable names
  varlen = 0,
  roundint = FALSE,
  box.palette = c("#2c7fb8", "#f03b20"),  # blue vs red
  shadow.col = "gray",
  nn = TRUE                # show node numbers
)

############################################################

cat("\n==============================\n")
cat(" RANDOM FOREST VARIABLE IMPORTANCE (GINI)\n")
cat("==============================\n\n")

library(ggplot2)

# Clean + format importance table
importance_clean <- importance_tbl |>
  as.data.frame() |>
  rownames_to_column("Variable") |>
  arrange(desc(MeanDecreaseGini)) |>
  mutate(
    Variable = gsub("_", " ", Variable),              # clean names
    Variable = stringr::str_to_title(Variable)        # nicer formatting
  )

# Plot
ggplot(importance_clean, aes(x = reorder(Variable, MeanDecreaseGini),
                             y = MeanDecreaseGini,
                             fill = MeanDecreaseGini)) +
  geom_col(width = 0.7) +
  coord_flip() +
  
  # Color gradient (nice visual pop)
  scale_fill_gradient(
    low = "#a6bddb",
    high = "#045a8d"
  ) +
  
  # Labels
  labs(
    title = "Random Forest Variable Importance",
    subtitle = "Measured by Mean Decrease in Gini",
    x = NULL,
    y = "Importance (Gini Decrease)"
  ) +
  
  # Clean theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.text.y = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

############################################################

cat("\n==============================\n")
cat(" RANKED VARIABLE IMPORTANCE TABLE\n")
cat("==============================\n\n")

# Clean importance table
importance_clean <- importance_tbl |>
  as.data.frame() |>
  rownames_to_column("Variable") |>
  arrange(desc(MeanDecreaseGini)) |>
  select(Variable, MeanDecreaseGini, MeanDecreaseAccuracy)

print(as_tibble(importance_clean), n = Inf)
############################################################

cat("\n==============================\n")
cat(" MODEL PERFORMANCE SUMMARY\n")
cat("==============================\n\n")

print(final_model_summary)

############################################################

cat("\n==============================\n")
cat(" BEST MODEL SETTINGS\n")
cat("==============================\n\n")

print(best_settings)
