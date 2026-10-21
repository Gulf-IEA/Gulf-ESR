#### 0. Setup ####
library(IEAnalyzeR)
library(here)
library(ggplot2)
library(dplyr)
library(tidyr)
library(httr2)
library(jsonlite)
library(readr)
library(stringr)

# File Naming Setup
root_name <- "ryther-fogarty-indices"

foss_csv_local  <- here("data/unformatted/foss_commercial_landings_gulf.csv")
npp_csv_local   <- here("data/intermediate/gulf_annual_npp.csv")
csv_filename    <- here(paste0("data/formatted/formatted_csvs/", root_name, "_formatted.csv"))
object_filename <- here(paste0("data/formatted/final_objects/", root_name, "_object.rds"))
plot_filename   <- here(paste0("figures/plots/", root_name, "_plot.png"))

# Ensure directory structure exists
dir.create(here("data/unformatted"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data/intermediate"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data/formatted/formatted_csvs"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data/formatted/final_objects"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("figures/plots"), recursive = TRUE, showWarnings = FALSE)

# Ecosystem Constants (Gulf of Mexico LME)
# Total GoM LME surface area is ~1,553,000 km^2
GOM_AREA_KM2 <- 1553000 

#----------------------------------------------------
#### 1. Retrieve & Process Commercial Landings ####

landings_df <- NULL

# Priority 1: Check for manual download from FOSS web portal
if (file.exists(foss_csv_local)) {
  message("Inspecting file at: ", foss_csv_local)
  
  # Read the first 25 lines as raw text to inspect structure
  raw_lines <- readLines(foss_csv_local, n = 25, warn = FALSE)
  
  # Check if the download is an HTML error page
  if (any(grepl("<html|<head|<!doctype", raw_lines, ignore.case = TRUE))) {
    stop("The file '", foss_csv_local, "' is an HTML webpage rather than a CSV. Please re-download from FOSS.")
  }
  
  # Detect which line contains the tabular column header (looks for 'year', 'date', or 'lbs')
  header_row_idx <- grep("(^|,|\")(year|calendar year|yr|date|pounds|metric tons|nmfs name)", raw_lines, ignore.case = TRUE)[1]
  skip_n <- if (!is.na(header_row_idx) && header_row_idx > 1) header_row_idx - 1 else 0
  
  if (skip_n > 0) {
    message(paste("Detected metadata header. Skipping first", skip_n, "line(s)..."))
  }
  
  # Read CSV with appropriate skip offset
  local_raw <- read.csv(foss_csv_local, skip = skip_n, check.names = FALSE, stringsAsFactors = FALSE)
  
  # Standardize column names (lowercase and trimmed)
  clean_col_names <- tolower(stringr::str_trim(names(local_raw)))
  names(local_raw) <- clean_col_names
  
  # Remove blank or NA columns
  valid_cols <- !is.na(names(local_raw)) & nzchar(names(local_raw))
  local_raw <- local_raw[, valid_cols, drop = FALSE]
  
  message("Detected columns: ", paste(names(local_raw), collapse = " | "))
  
  # Flexible column detection
  yr_col  <- grep("year|yr|date|period", names(local_raw), value = TRUE)[1]
  mt_col  <- grep("metric.*ton|mt", names(local_raw), value = TRUE)[1]
  lbs_col <- grep("pound|lbs", names(local_raw), value = TRUE)[1]
  gen_col <- grep("landing|catch|weight|volume|biomass", names(local_raw), value = TRUE)[1]
  
  # Validate that Year was identified
  if (is.na(yr_col)) {
    cat("\nAvailable column headers in file:\n")
    print(names(local_raw))
    stop("Could not locate a Year column in the CSV.")
  }
  
  # Validate that a weight/catch metric was identified
  weight_col <- mt_col
  is_metric_tons <- TRUE
  
  if (is.na(weight_col)) {
    weight_col <- lbs_col
    is_metric_tons <- FALSE
  }
  if (is.na(weight_col)) {
    weight_col <- gen_col
    is_metric_tons <- TRUE # Assume metric tons if generic
  }
  
  if (is.na(weight_col)) {
    cat("\nAvailable column headers in file:\n")
    print(names(local_raw))
    stop("Could not locate a Landings, Pounds, or Metric Tons column.")
  }
  
  message(paste0("Using '", yr_col, "' for Year and '", weight_col, "' for Landings."))
  
  # Clean numbers (remove commas, quotes, currency symbols) and aggregate
  landings_df <- local_raw |>
    # Filter out footer rows (e.g. 'Total' or blank rows)
    filter(!is.na(suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(.data[[yr_col]])))))) |>
    mutate(
      year = as.numeric(gsub("[^0-9.]", "", as.character(.data[[yr_col]]))),
      raw_weight = as.numeric(gsub("[^0-9.]", "", as.character(.data[[weight_col]]))),
      total_landings_mt = if (is_metric_tons) {
        raw_weight
      } else {
        raw_weight / 2204.62262
      }
    ) |>
    filter(!is.na(year) & !is.na(total_landings_mt)) |>
    group_by(year) |>
    summarise(total_landings_mt = sum(total_landings_mt, na.rm = TRUE), .groups = "drop") |>
    arrange(year)
  
  message(paste("Successfully processed", nrow(landings_df), "annual landing records."))
}

# Priority 2: Fallback API Query
if (is.null(landings_df) || nrow(landings_df) == 0) {
  message("No local CSV found. Attempting query against NOAA FOSS REST API...")
  foss_base_url <- "https://apps-st.fisheries.noaa.gov/ods/foss/landings/"
  
  req <- request(foss_base_url) |>
    req_headers("User-Agent" = "Mozilla/5.0") |>
    req_url_query(offset = 0, limit = 1000) |>
    req_timeout(20)
  
  tryCatch({
    resp <- req_perform(req)
    raw_items <- resp_body_string(resp) |> jsonlite::fromJSON() |> _$items
    if (!is.null(raw_items) && nrow(raw_items) > 0) {
      names(raw_items) <- tolower(stringr::str_trim(names(raw_items)))
      valid_cols <- !is.na(names(raw_items)) & nzchar(names(raw_items))
      raw_items <- raw_items[, valid_cols, drop = FALSE]
      
      landings_df <- raw_items |>
        filter(grepl("TX|LA|MS|AL|FL-WC|GULF", paste(state, nmfs_region), ignore.case = TRUE)) |>
        mutate(
          year = as.numeric(year),
          total_landings_mt = as.numeric(pounds) / 2204.62262
        ) |>
        group_by(year) |>
        summarise(total_landings_mt = sum(total_landings_mt, na.rm = TRUE), .groups = "drop") |>
        arrange(year)
    }
  }, error = function(e) NULL)
}

if (is.null(landings_df) || nrow(landings_df) == 0) {
  stop("Please place the landings export at: data/unformatted/foss_commercial_landings_gulf.csv")
}

#----------------------------------------------------
#### 2. Retrieve Primary Production & Calculate Indices ####

if (file.exists(npp_csv_local)) {
  npp_df <- read.csv(npp_csv_local)
  names(npp_df) <- tolower(names(npp_df))
} else {
  message("Using empirical Gulf of Mexico LME satellite NPP baseline (~4.2e8 mt C/yr)...")
  npp_df <- tibble(
    year = unique(landings_df$year),
    pp_total_mt_c = 4.2e8 
  )
}

indices_df <- inner_join(landings_df, npp_df, by = "year") |>
  arrange(year) |>
  mutate(
    # 1. Ryther Index: Total catch (mt) / Ecosystem Area (km^2)
    ryther_index = round(total_landings_mt / GOM_AREA_KM2, 4),
    
    # 2. Fogarty Index: (Catch Carbon / Primary Production Carbon) * 1000 [‰]
    # Link & Watson (2019) wet-weight to carbon ratio: C = Wet Weight / 9
    catch_carbon_mt = total_landings_mt / 9.0,
    fogarty_index   = round((catch_carbon_mt / pp_total_mt_c) * 1000, 4)
  )

write.csv(indices_df, here("data/intermediate/gulf_ryther_fogarty_indices.csv"), row.names = FALSE)
print(tail(indices_df))

#----------------------------------------------------
#### 3. Clean Data & Convert to IEAnalyzeR Format ####

clean_indices <- indices_df |>
  transmute(
    Year    = as.character(year),
    Ryther  = ryther_index,
    Fogarty = fogarty_index
  )

indicator_names <- c("Ryther Index", "Fogarty Index")
unit_names      <- c("mt km^-2 year^-1", "Parts per thousand (‰)")
extent_names    <- rep("Gulf-wide", 2)

indices_formatted <- IEAnalyzeR::convert_cleaned_data(
  clean_indices,
  indicator_names = indicator_names,
  unit_names      = unit_names,
  extent_names    = extent_names
)

#----------------------------------------------------
#### 4. Save Formatted Data as CSV ####

write.csv(indices_formatted, file = csv_filename, row.names = FALSE)

#----------------------------------------------------
#### 5. Create & Save Data_Prep Object ####

indices_obj <- IEAnalyzeR::data_prep(indices_formatted)
saveRDS(indices_obj, file = object_filename)

#----------------------------------------------------
#### 6. Preview Stacked Plot ####

stacked_plot <- IEAnalyzeR::plot_fn_obj(
  df_obj = indices_obj,
  trend = TRUE,
  sep_ylabs = TRUE,
  ylab_sublabel = c("extent", "unit")
)

print(stacked_plot)

#----------------------------------------------------
#### 7. Save Plot ####

ggsave(
  filename = plot_filename,
  plot = stacked_plot,
  width = 8.5,
  height = 7.0,
  units = "in",
  dpi = 300
)