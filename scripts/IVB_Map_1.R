### Libraries ###
library(tidyverse)
library(sf)
library(leaflet)
library(htmltools)
library(scales)

### 1. Load Data

snap_raw <- read_csv("scripts/hennepin_snap_mfip_tract_reva.csv", show_col_types = FALSE)

municipalities <- st_read("scripts/tl_2020_27_place/tl_2020_27_place.shp", quiet = TRUE)
tracts <- st_read("scripts/tl_2020_27_tract/tl_2020_27_tract.shp", quiet = TRUE)

### 2. Clean SNAP Data

snap_clean <- snap_raw |>
  filter(!is.na(GEOID)) |>
  select(GEOID, NAME, YearMonth, SNAP_people, MFIP_people, p_125_povertyE) |>
  mutate(
    GEOID = sprintf("%011.0f", as.numeric(GEOID)),
    year = YearMonth %/% 100
  ) |>
  filter(year >= 2020, year <= 2025) |>
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

### 3. Tract Summary by Year

tract_summary_year <- snap_clean |>
  group_by(GEOID, year) |>
  summarise(
    tract_name = first(na.omit(NAME)),
    enrolled = median(enrolled, na.rm = TRUE),
    eligible = median(eligible, na.rm = TRUE),
    gap_count = median(gap_count, na.rm = TRUE),
    gap_rate = median(gap_rate, na.rm = TRUE),
    .groups = "drop"
  )

tract_summary_all <- snap_clean |>
  group_by(GEOID) |>
  summarise(
    tract_name = first(na.omit(NAME)),
    enrolled = median(enrolled, na.rm = TRUE),
    eligible = median(eligible, na.rm = TRUE),
    gap_count = median(gap_count, na.rm = TRUE),
    gap_rate = median(gap_rate, na.rm = TRUE),
    year_label = "All Years",
    .groups = "drop"
  )

### 4. Join to Tracts

tracts <- tracts |>
  filter(COUNTYFP == "053") |>
  mutate(GEOID = as.character(GEOID))

tracts_year <- tracts |>
  left_join(tract_summary_year, by = "GEOID") |>
  mutate(year_label = as.character(year))

tracts_all <- tracts |>
  left_join(tract_summary_all, by = "GEOID")

### 5. Assign Municipality

assign_municipality <- function(sf_object, municipalities_sf) {
  tract_centroids <- st_centroid(sf_object)
  
  tract_muni <- st_join(
    tract_centroids,
    municipalities_sf |> select(NAME),
    join = st_within
  )
  
  tract_lookup <- tract_muni |>
    st_drop_geometry() |>
    select(GEOID, municipality = NAME.y) |>
    distinct()
  
  sf_object |>
    left_join(tract_lookup, by = "GEOID") |>
    mutate(
      municipality = if_else(is.na(municipality), "Unassigned", municipality)
    )
}

tracts_year <- assign_municipality(tracts_year, municipalities)
tracts_all <- assign_municipality(tracts_all, municipalities)

### 6. Municipality Stats + High Priority Logic

add_priority_logic <- function(sf_object, by_year = FALSE) {
  if (by_year) {
    muni_stats <- sf_object |>
      st_drop_geometry() |>
      group_by(municipality, year_label) |>
      summarise(
        city_gap_rate = median(gap_rate, na.rm = TRUE),
        .groups = "drop"
      )
    
    sf_object <- sf_object |>
      left_join(muni_stats, by = c("municipality", "year_label"))
  } else {
    muni_stats <- sf_object |>
      st_drop_geometry() |>
      group_by(municipality) |>
      summarise(
        city_gap_rate = median(gap_rate, na.rm = TRUE),
        .groups = "drop"
      )
    
    sf_object <- sf_object |>
      left_join(muni_stats, by = "municipality")
  }
  
  sf_object <- sf_object |>
    mutate(diff_from_city = gap_rate - city_gap_rate)
  
  cut_5  <- quantile(sf_object$diff_from_city, 0.95, na.rm = TRUE)
  cut_10 <- quantile(sf_object$diff_from_city, 0.90, na.rm = TRUE)
  cut_15 <- quantile(sf_object$diff_from_city, 0.85, na.rm = TRUE)
  cut_20 <- quantile(sf_object$diff_from_city, 0.80, na.rm = TRUE)
  
  sf_object |>
    mutate(
      top5  = diff_from_city >= cut_5,
      top10 = diff_from_city >= cut_10,
      top15 = diff_from_city >= cut_15,
      top20 = diff_from_city >= cut_20
    )
}

tracts_year <- add_priority_logic(tracts_year, by_year = TRUE)
tracts_all <- add_priority_logic(tracts_all, by_year = FALSE)

### 7. Hover Labels

build_label <- function(df) {
  paste0(
    "<div style='font-family: Arial; font-size: 13px; line-height: 1.35;'>",
    "<b>City:</b> ", df$municipality, "<br/>",
    "<b>Census Tract:</b> ", df$tract_name, "<br/>",
    "<b>SNAP/MFIP Enrolled:</b> ", comma(round(df$enrolled)), "<br/>",
    "<b>Estimated Eligible (≤125% PL):</b> ", comma(round(df$eligible)), "<br/><br/>",
    "<b>Tract Gap Rate:</b> ", percent(df$gap_rate, accuracy = 0.1), "<br/>",
    "<b>City Gap Rate:</b> ", percent(df$city_gap_rate, accuracy = 0.1),
    "</div>"
  )
}

tracts_year$label_text <- build_label(tracts_year)
tracts_all$label_text  <- build_label(tracts_all)

### 8. Transform / Clip

tracts_year <- st_transform(tracts_year, 4326)
tracts_all <- st_transform(tracts_all, 4326)
municipalities <- st_transform(municipalities, 4326)

municipalities <- municipalities |>
  st_intersection(st_union(tracts_all)) |>
  st_collection_extract("POLYGON")

### 9. Palette

pal <- colorNumeric(
  palette = "Purples",
  domain = c(0, max(c(tracts_year$gap_rate, tracts_all$gap_rate), na.rm = TRUE)),
  na.color = "#d9d9d9"
)

### 10. Build Map

final_map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
  
  addMapPane("tracts", zIndex = 410) |>
  addMapPane("cities", zIndex = 420) |>
  addMapPane("priority", zIndex = 400) |>
  
  addProviderTiles(providers$CartoDB.Positron) |>
  
  addLegend(
    position = "bottomright",
    pal = pal,
    values = c(tracts_year$gap_rate, tracts_all$gap_rate),
    title = "Gap Rate",
    opacity = 0.8
  ) |>
  
  addControl(
    html = HTML(
      "<div style='background: white; padding: 10px 12px; border-radius: 6px; 
      box-shadow: 0 1px 5px rgba(0,0,0,0.25); font-family: Arial; font-size: 13px; max-width: 270px;'>
      
      <b>Gap Rate</b><br/>
      (Eligible - Enrolled) / Eligible<br/>
      Higher values are worse.<br/><br/>
      
      <b>High Priority Gap Areas</b><br/>
      Red tracts are the tracts with the largest differences between their gap rate and their city's average gap rate.
      
      </div>"
    ),
    position = "topright"
  ) |>
  
  addLayersControl(
    baseGroups = c("All Years", as.character(sort(unique(tracts_year$year_label)))),
    overlayGroups = c(
      "Top 5%",
      "Top 10%",
      "Top 15%",
      "Top 20%"
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  
  hideGroup(c("Top 5%", "Top 10%", "Top 15%", "Top 20%"))

### 11. Add All Years Layers

final_map <- final_map |>
  addPolygons(
    data = tracts_all,
    fillColor = ~pal(gap_rate),
    fillOpacity = 0.6,
    color = "#444",
    weight = 0.5,
    stroke = TRUE,
    smoothFactor = 0,
    options = pathOptions(pane = "tracts"),
    label = ~lapply(label_text, HTML),
    labelOptions = labelOptions(
      direction = "auto",
      style = list(
        "font-size" = "12px",
        "padding" = "4px 6px"
      )
    ),
    highlightOptions = highlightOptions(
      weight = 3,
      color = "white",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    popup = NULL,
    group = "All Years"
  ) |>
  addPolygons(
    data = tracts_all |> filter(top5),
    fillColor = "#ff2d55",
    fillOpacity = 0.8,
    color = "white",
    weight = 2,
    stroke = TRUE,
    smoothFactor = 0,
    options = pathOptions(pane = "priority"),
    label = NULL,
    popup = NULL,
    highlightOptions = highlightOptions(
      weight = 0,
      color = "#ff2d55",
      fillOpacity = 0.8,
      bringToFront = FALSE
    ),
    group = "Top 5%"
  ) |>
  addPolygons(
    data = tracts_all |> filter(top10),
    fillColor = "#ff2d55",
    fillOpacity = 0.8,
    color = "white",
    weight = 2,
    stroke = TRUE,
    smoothFactor = 0,
    options = pathOptions(pane = "priority"),
    label = NULL,
    popup = NULL,
    highlightOptions = highlightOptions(
      weight = 0,
      color = "#ff2d55",
      fillOpacity = 0.8,
      bringToFront = FALSE
    ),
    group = "Top 10%"
  ) |>
  addPolygons(
    data = tracts_all |> filter(top15),
    fillColor = "#ff2d55",
    fillOpacity = 0.8,
    color = "white",
    weight = 2,
    stroke = TRUE,
    smoothFactor = 0,
    options = pathOptions(pane = "priority"),
    label = NULL,
    popup = NULL,
    highlightOptions = highlightOptions(
      weight = 0,
      color = "#ff2d55",
      fillOpacity = 0.8,
      bringToFront = FALSE
    ),
    group = "Top 15%"
  ) |>
  addPolygons(
    data = tracts_all |> filter(top20),
    fillColor = "#ff2d55",
    fillOpacity = 0.8,
    color = "white",
    weight = 2,
    stroke = TRUE,
    smoothFactor = 0,
    options = pathOptions(pane = "priority"),
    label = NULL,
    popup = NULL,
    highlightOptions = highlightOptions(
      weight = 0,
      color = "#ff2d55",
      fillOpacity = 0.8,
      bringToFront = FALSE
    ),
    group = "Top 20%"
  ) |>
  addPolygons(
    data = municipalities,
    fill = FALSE,
    color = "#00bcd4",
    weight = 2,
    opacity = 0.8,
    options = pathOptions(pane = "cities"),
    label = NULL,
    popup = NULL,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#00bcd4",
      bringToFront = FALSE
    ),
    group = "All Years"
  )

### 12. Add Year-Specific Layers

for (yr in sort(unique(tracts_year$year_label))) {
  year_data  <- tracts_year |> filter(year_label == yr)
  year_top5  <- year_data |> filter(top5)
  year_top10 <- year_data |> filter(top10)
  year_top15 <- year_data |> filter(top15)
  year_top20 <- year_data |> filter(top20)
  
  final_map <- final_map |>
    addPolygons(
      data = year_data,
      fillColor = ~pal(gap_rate),
      fillOpacity = 0.6,
      color = "#444",
      weight = 0.5,
      stroke = TRUE,
      smoothFactor = 0,
      options = pathOptions(pane = "tracts"),
      label = ~lapply(label_text, HTML),
      labelOptions = labelOptions(
        direction = "auto",
        style = list(
          "font-size" = "12px",
          "padding" = "4px 6px"
        )
      ),
      highlightOptions = highlightOptions(
        weight = 3,
        color = "white",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      popup = NULL,
      group = yr
    ) |>
    addPolygons(
      data = year_top5,
      fillColor = "#ff2d55",
      fillOpacity = 0,
      color = "#ff0000",
      weight = 4,
      dashArray = "6,4",
      stroke = TRUE,
      smoothFactor = 0,
      options = pathOptions(pane = "priority"),
      label = NULL,
      popup = NULL,
      highlightOptions = highlightOptions(
        weight = 0,
        color = "#ff2d55",
        fillOpacity = 1,
        bringToFront = FALSE
      ),
      group = "Top 5%"
    ) |>
    addPolygons(
      data = year_top10,
      fillColor = "#ff2d55",
      fillOpacity = 0,
      color = "#ff0000",
      weight = 4,
      dashArray = "6,4",
      stroke = TRUE,
      smoothFactor = 0,
      options = pathOptions(pane = "priority"),
      label = NULL,
      popup = NULL,
      highlightOptions = highlightOptions(
        weight = 0,
        color = "#ff2d55",
        fillOpacity = 0.9,
        bringToFront = FALSE
      ),
      group = "Top 10%"
    ) |>
    addPolygons(
      data = year_top15,
      fillColor = "#ff2d55",
      fillOpacity = 0,
      color = "#ff0000",
      weight = 4,
      dashArray = "6,4",
      stroke = TRUE,
      smoothFactor = 0,
      options = pathOptions(pane = "priority"),
      label = NULL,
      popup = NULL,
      highlightOptions = highlightOptions(
        weight = 0,
        color = "#ff2d55",
        fillOpacity = 0.9,
        bringToFront = FALSE
      ),
      group = "Top 15%"
    ) |>
    addPolygons(
      data = year_top20,
      fillColor = "#ff2d55",
      fillOpacity = 0,
      color = "#ff0000",
      weight = 4,
      dashArray = "6,4",
      stroke = TRUE,
      smoothFactor = 0,
      options = pathOptions(pane = "priority"),
      label = NULL,
      popup = NULL,
      highlightOptions = highlightOptions(
        weight = 0,
        color = "#ff2d55",
        fillOpacity = 0.9,
        bringToFront = FALSE
      ),
      group = "Top 20%"
    ) |>
    addPolygons(
      data = municipalities,
      fill = FALSE,
      color = "#00bcd4",
      weight = 2,
      opacity = 0.8,
      options = pathOptions(pane = "cities"),
      label = NULL,
      popup = NULL,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#00bcd4",
        bringToFront = FALSE
      ),
      group = yr
    )
}

### 13. Show Only All Years by Default

for (yr in sort(unique(tracts_year$year_label))) {
  final_map <- final_map |> hideGroup(yr)
}

### 14. Print Map

final_map
