### Libraries ###
library(tidyverse)
library(sf)
library(ggplot2)

### Load In Data ###
snap_raw <- read_csv("scripts/hennepin_snap_mfip_tract_reva.csv_tract_reva.csv")

### Remove Garbage NA Rows ###
snap_clean <- snap_raw |>
  filter(!is.na(GEOID))

### Remove Unneeded Columns ###
snap_clean <- snap_clean |>
  select(GEOID, NAME, YearMonth, SNAP_people, MFIP_people, p_125_povertyE)

### Fix tract formatting ###
snap_clean <- snap_clean |>
  mutate(GEOID = sprintf("%011.0f", GEOID))

### Create year and month variables ###
snap_clean <- snap_clean |>
  mutate(
    year = YearMonth %/% 100,
    month = YearMonth %% 100
  )

### Post-COVID Only ###
snap_clean <- snap_clean |>
  filter(year >= 2022, year <= 2025)

### Convert to numbers, and clean low counts ###
snap_clean <- snap_clean |>
  mutate(
    SNAP_people = na_if(SNAP_people, "Low count"),
    MFIP_people = na_if(MFIP_people, "Low count"),
    SNAP_people = as.numeric(SNAP_people),
    MFIP_people = as.numeric(MFIP_people)
  )

### Combine SNAP and MFIP into 1 ###
snap_clean <- snap_clean |>
  mutate(
    snap_mfip_people = coalesce(SNAP_people, 0) + coalesce(MFIP_people, 0)
  )

### Eligibility - Enrollment Gap Calculations ###
snap_clean <- snap_clean |>
  mutate(
    eligible_125 = p_125_povertyE,
    gap_count = eligible_125 - snap_mfip_people,
    gap_count = if_else(gap_count < 0, 0, gap_count),
    gap_rate  = if_else(eligible_125 > 0, gap_count / eligible_125, NA_real_)
  )

### Tract level Median from 2022-2025 ###
tract_summary <- snap_clean |>
  group_by(GEOID) |>
  summarise(
    median_gap_rate_2022_2025  = median(gap_rate, na.rm = TRUE),
    median_gap_count_2022_2025 = median(gap_count, na.rm = TRUE),
    n_months_used = sum(!is.na(gap_rate)),
    .groups = "drop"
  )

### Remove tracts with no usable months ###
tract_summary <- tract_summary |>
  filter(n_months_used > 0)

### Load municipality boundaries ###
municipalities <- st_read("scripts/tl_2020_27_place/tl_2020_27_place.shp", quiet = TRUE)

### Load census tract boundaries ###
tracts <- st_read("scripts/tl_2020_27_tract/tl_2020_27_tract.shp", quiet = TRUE)

### Keep only Hennepin County tracts ###
tracts <- tracts |>
  filter(COUNTYFP == "053")

### Attach SNAP data to tract data ###
tracts_with_data <- tracts |>
  left_join(tract_summary, by = "GEOID")

### Create tract centroids ###
tract_centroids <- tracts_with_data |>
  st_centroid()

### Match coordinate systems ###
municipalities <- st_transform(municipalities, st_crs(tract_centroids))

### Assign each tract to a municipality ###
tract_muni <- st_join(
  tract_centroids,
  municipalities |> select(PLACEFP, NAME),
  join = st_within
)

### Clean up name columns ###
tract_muni <- tract_muni |>
  rename(
    tract_name = NAME.x,
    municipality = NAME.y
  )

### Label unassigned tracts ###
tract_muni <- tract_muni |>
  mutate(
    municipality = if_else(
      is.na(municipality),
      "unassigned",
      municipality
    )
  )

### Municipality-level summary ###
municipality_summary <- tract_muni |>
  st_drop_geometry() |>
  group_by(municipality) |>
  summarise(
    n_tracts = n(),
    median_gap = median(median_gap_rate_2022_2025, na.rm = TRUE),
    mean_gap = mean(median_gap_rate_2022_2025, na.rm = TRUE),
    .groups = "drop"
  )

### Boxplots by Municipality ###
tract_muni |>
  st_drop_geometry() |>
  inner_join(municipality_summary, by = "municipality") |>
  filter(n_tracts >= 5) |>
  ggplot(aes(
    x = reorder(municipality, median_gap_rate_2022_2025, median, na.rm = TRUE),
    y = median_gap_rate_2022_2025
  )) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Distribution of SNAP Participation Gaps by Municipality",
    x = "Municipality",
    y = "Median Participation Gap (2022–2025)"
  )

### SNAP Participation Gap Map ###
ggplot(data = tracts_with_data) +
  geom_sf(aes(fill = median_gap_rate_2022_2025), color = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey90",
    name = "Gap Rate"
  ) +
  labs(
    title = "SNAP Participation Gap by Census Tract (2022–2025)",
    subtitle = "Higher values indicate more eligible residents not enrolled",
    caption = "Hennepin County"
  ) +
  theme_minimal()

