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
dir.create(here("data/intermediate"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data/formatted/formatted_csvs"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data/formatted/final_objects"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("figures/plots"), recursive = TRUE, showWarnings = FALSE)

# Ecosystem Constants (Gulf of Mexico LME)
# Total GoM LME area is ~1,553,000 km^2; US GoM EEZ/shelf is ~570,000 km^2.
# Set to the spatial footprint representing your management unit:
GOM_AREA_KM2 <- 1553000 

#----------------------------------------------------
#### 1. Retrieve & Process Commercial Landings ####

landings_df <- NULL

# Method A: Direct pull from NOAA Fisheries One-Stop Shop (FOSS) REST API
message("Attempting to query NOAA FOSS Landings API...")
foss_base_url <- "https://apps-st.fisheries.noaa.gov/ods/foss/landings/"

# Query parameters for Gulf commercial landings (offset/limit pagination)
fetch_foss_page <- function(offset = 0, limit = 5000) {
  # Filter for Gulf region states
  query_filter <- '{"$or":[{"state":"TX"},{"state":"LA"},{"state":"MS"},{"state":"AL"},{"state":"FL-WC"}]}'
  
  req <- request(foss_base_url) |>
    req_url_query(
      q = query_filter,
      offset = offset,
      limit = limit
    ) |>
    req_timeout(30)
  
  tryCatch({
    resp <- req_perform(req)
    raw_json <- resp_body_string(resp) |> jsonlite::fromJSON()
    return(raw_json$items)
  }, error = function(e) {
    return(NULL)
  })
}

# Run pagination loop
foss_records <- list()
offset <- 0
batch_size <- 5000
repeat {
  batch <- fetch_foss_page(offset = offset, limit = batch_size)
  if (is.null(batch) || nrow(batch) == 0) break
  foss_records[[length(foss_records) + 1]] <- batch
  if (nrow(batch) < batch_size) break
  offset <- offset + batch_size
}

if (length(foss_records) > 0) {
  landings_raw <- bind_rows(foss_records)
  message(paste("Successfully fetched", nrow(landings_raw), "records from FOSS API."))
  
  # Standardize column names (pounds or metric tons)
  names(landings_raw) <- tolower(names(landings_raw))
  
  landings_df <- landings_raw |>
    mutate(
      year   = as.numeric(year),
      pounds = as.numeric(pounds),
      # Convert pounds to metric tons (1 metric ton = 2204.62 lbs)
      metric_tons = pounds / 2204.62262
    ) |>
    group_by(year) |>
    summarise(total_landings_mt = sum(metric_tons, na.rm = TRUE), .groups = "drop")
}

# Method B: Local Fallback (if API times out or web export was placed in unformatted)
if (is.null(landings_df) || nrow(landings_df) == 0) {
  if (file.exists(foss_csv_local)) {
    message("Loading local FOSS landings export...")
    local_raw <- read.csv(foss_csv_local)
    names(local_raw) <- tolower(names(local_raw))
    
    # Locate year and pounds/metric tons columns
    yr_col  <- grep("year", names(local_raw), value = TRUE)[1]
    lbs_col <- grep("pound|lbs", names(local_raw), value = TRUE)[1]
    mt_col  <- grep("metric|tons|mt", names(local_raw), value = TRUE)[1]
    
    landings_df <- local_raw |>
      mutate(
        year = as.numeric(.data[[yr_col]]),
        total_landings_mt = if (!is.na(mt_col)) {
          as.numeric(.data[[mt_col]])
        } else {
          as.numeric(.data[[lbs_col]]) / 2204.62262
        }
      ) |>
      group_by(year) |>
      summarise(total_landings_mt = sum(total_landings_mt, na.rm = TRUE), .groups = "drop")
  } else {
    stop("Could not query FOSS API and no local export found at data/unformatted/foss_commercial_landings_gulf.csv.")
  }
}

#----------------------------------------------------
#### 2. Retrieve Primary Production & Calculate Indices ####

# Net Primary Production (NPP) in metric tons of Carbon (t C / yr)
# If satellite NPP file exists, read it; otherwise, use the empirical mean time series
if (file.exists(npp_csv_local)) {
  npp_df <- read.csv(npp_csv_local)
  names(npp_df) <- tolower(names(npp_df))
} else {
  message("Using published GoM LME satellite NPP trajectory...")
  # Mean annual NPP for Gulf LME is ~4.2e8 metric tons C/yr (~270 g C m^-2 yr^-1) Source: https://unesdoc.unesco.org/ark:/48223/pf0000259868
  npp_df <- tibble(
    year = unique(landings_df$year),
    pp_total_mt_c = 4.2e8 
  )
}

# Merge Landings and Primary Production
indices_df <- inner_join(landings_df, npp_df, by = "year") |>
  arrange(year) |>
  mutate(
    # 1. Ryther Index: Total catch (mt) / Ecosystem Area (km^2)
    ryther_index = round(total_landings_mt / GOM_AREA_KM2, 4),
    
    # 2. Fogarty Index: (Catch / Primary Production) * 1000 [parts per thousand, ‰]
    # Link & Watson (2019) wet-weight to carbon ratio: Carbon = Wet Weight / 9
    catch_carbon_mt = total_landings_mt / 9.0,
    fogarty_index   = round((catch_carbon_mt / pp_total_mt_c) * 1000, 4)
  )

# Save intermediate summary table
write.csv(indices_df, here("data/intermediate/gulf_ryther_fogarty_indices.csv"), row.names = FALSE)
print(tail(indices_df))

#----------------------------------------------------
#### 3. Clean Data & Convert to IEAnalyzeR Format ####

# Column 1 must be time (as character); remaining columns are the metrics
clean_indices <- indices_df |>
  transmute(
    Year    = as.character(year),
    Ryther  = ryther_index,
    Fogarty = fogarty_index
  )

# Define metadata vectors matching the two metrics
indicator_names <- c("Ryther Index", "Fogarty Index")
unit_names      <- c("mt km^-2 year^-1", "Parts per thousand (‰)")
extent_names    <- rep("Gulf of Mexico LME", 2)

# Convert to standardized IEA long-header format
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

# sep_ylabs = TRUE creates vertically stacked subpanels
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