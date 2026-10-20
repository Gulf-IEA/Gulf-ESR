#### 0. Setup ####
library(IEAnalyzeR)
library(here)
library(ggplot2)
library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(sf)

# File Naming Setup
root_name <- "contaminants-and-spills"

raw_dir         <- here("data/intermediate/mpsr_raw")
csv_filename    <- here(paste0("data/formatted/formatted_csvs/", root_name, "_formatted.csv"))
object_filename <- here(paste0("data/formatted/final_objects/", root_name, "_object.rds"))
plot_filename   <- here(paste0("figures/plots/", root_name, "_plot.png"))

# Ensure destination directories exist
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(here("data/intermediate"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data/formatted/formatted_csvs"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data/formatted/final_objects"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("figures/plots"), recursive = TRUE, showWarnings = FALSE)

#----------------------------------------------------
#### 1. Unpack & Process Spatial Layers ####

# Turn off s2 spherical engine to safely handle duplicate/degenerate vertices
sf::sf_use_s2(FALSE)

# Layer ID to Year lookup dictionary
layer_year_lookup <- c(
  "51914" = "2015",
  "51913" = "2016",
  "51912" = "2017",
  "51719" = "2018",
  "50546" = "2019",
  "51909" = "2020",
  "53132" = "2021",
  "54952" = "2022",
  "56848" = "2023"
)

# Unzip any downloaded archive files
zip_files <- list.files(raw_dir, pattern = "\\.zip$", full.names = TRUE)
if (length(zip_files) > 0) {
  message("Unzipping downloaded ERMA shapefile archives...")
  purrr::walk(zip_files, function(z) {
    unzip(z, exdir = file.path(raw_dir, tools::file_path_sans_ext(basename(z))))
  })
}

# Locate all spatial files
spatial_files <- list.files(raw_dir, pattern = "\\.(shp|geojson)$", full.names = TRUE, recursive = TRUE)

if (length(spatial_files) == 0) {
  stop("No shapefiles or GeoJSON files found in data/intermediate/mpsr_raw/.")
}

# Helper function to extract Year, calculate dissolved area, and count events
process_mpsr_layer <- function(file_path) {
  fname <- basename(file_path)
  
  # Read spatial features, strip Z/M dimensions, and suppress FID warnings
  layer_sf <- suppressWarnings(
    sf::st_read(file_path, quiet = TRUE) |>
      sf::st_zm(drop = TRUE, what = "ZM")
  )
  
  # 1. Determine Year
  yr <- NA_character_
  col_match <- grep("^(year|yr|date|valid_date|rep_date)$", names(layer_sf), ignore.case = TRUE, value = TRUE)
  if (length(col_match) > 0) {
    val <- layer_sf[[col_match[1]]]
    yr_candidate <- stringr::str_extract(as.character(val[!is.na(val)][1]), "20[12][0-9]")
    if (!is.na(yr_candidate)) yr <- yr_candidate
  }
  
  if (is.na(yr)) {
    matched_id <- names(layer_year_lookup)[purrr::map_lgl(names(layer_year_lookup), ~ grepl(.x, fname))]
    if (length(matched_id) > 0) yr <- layer_year_lookup[[matched_id[1]]]
  }
  
  if (is.na(yr)) {
    yr <- stringr::str_extract(fname, "20(1[0-9]|2[0-5])")
  }
  
  if (is.na(yr)) {
    warning(paste0("Could not determine year for: ", fname))
    yr <- tools::file_path_sans_ext(fname)
  }
  
  message(paste0("File: ", fname, "  -->  Year: ", yr))
  
  # 2. Repair geometries and remove empty features
  layer_sf <- sf::st_make_valid(layer_sf)
  layer_sf <- layer_sf[!sf::st_is_empty(layer_sf), ]
  
  # 3. Count individual detections / events before dissolving
  id_cols <- grep("^(slick_id|incident_id|report_id|spill_id)$", names(layer_sf), ignore.case = TRUE, value = TRUE)
  if (length(id_cols) > 0) {
    event_count <- length(unique(na.omit(layer_sf[[id_cols[1]]])))
  } else {
    event_count <- nrow(layer_sf)
  }
  
  # 4. Project to EPSG:5070 (NAD83 / Conus Albers Equal Area)
  layer_proj <- sf::st_transform(layer_sf, crs = 5070)
  
  # 5. Union / dissolve overlapping slicks to prevent duplicate area counting
  layer_union <- sf::st_union(layer_proj)
  
  # 6. Calculate total area (m^2 -> km^2)
  area_m2  <- sf::st_area(layer_union)
  area_km2 <- as.numeric(sum(area_m2)) / 1e6
  
  tibble(
    Year = as.character(yr),
    total_area_km2 = round(area_km2, 2),
    spill_count    = as.numeric(event_count)
  )
}

# Aggregate annual records across all files
annual_mpsr_df <- purrr::map_dfr(spatial_files, process_mpsr_layer) |>
  group_by(Year) |>
  summarize(
    total_area_km2 = sum(total_area_km2, na.rm = TRUE),
    spill_count    = sum(spill_count, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(Year)

# Save intermediate summary table
write.csv(annual_mpsr_df, here("data/intermediate/mpsr_annual_summary.csv"), row.names = FALSE)
print(annual_mpsr_df)

#----------------------------------------------------
#### 2. Clean Data & Create Combined Time Series ####

# Col 1 is time (Year); remaining columns are the two metrics to stack
clean_df <- annual_mpsr_df |>
  select(Year, total_area_km2, spill_count)

# Define header metadata vectors aligned with the 2 metric columns
indicator_names <- rep("Contaminants", 2)
unit_names      <- c("Total Area (sq km)", "Number of Detections")
extent_names    <- rep("Gulf of Mexico", 2)

# Convert to standardized long-header format
contaminants_formatted <- IEAnalyzeR::convert_cleaned_data(
  clean_df,
  indicator_names,
  unit_names,
  extent_names
)

head(contaminants_formatted)

#----------------------------------------------------
#### 3. Save Formatted Data as CSV ####

write.csv(contaminants_formatted, file = csv_filename, row.names = FALSE)

#----------------------------------------------------
#### 4. Create Data_Prep Object ####

contaminants_obj <- IEAnalyzeR::data_prep(contaminants_formatted)

#----------------------------------------------------
#### 5. Save Formatted Data_Prep Object ####

saveRDS(contaminants_obj, file = object_filename)

#----------------------------------------------------
#### 6. Preview Stacked Plot ####

# sep_ylabs = TRUE automatically splits the two series into vertically stacked panels
stacked_plot <- IEAnalyzeR::plot_fn_obj(
  df_obj = contaminants_obj,
  trend = TRUE,
  sep_ylabs = TRUE,
  ylab_sublabel = c("extent", "unit")
)

print(stacked_plot)

#----------------------------------------------------
#### 7. Save Stacked Plot ####

# Increase height to 7-8 inches to accommodate both vertical subpanels
ggsave(
  filename = plot_filename,
  plot = stacked_plot,
  width = 8,
  height = 7.5,
  units = "in"
)