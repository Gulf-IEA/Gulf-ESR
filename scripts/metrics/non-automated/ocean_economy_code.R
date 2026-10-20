#### 0. Setup ####
library(IEAnalyzeR)
library(here)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)

# File Naming Setup
root_name <- "total-marine-economy"

raw_filename    <- here("data/unformatted/OpenENOW_GulfStates_CountyLevel.csv")
csv_filename    <- here(paste0("data/formatted/formatted_csvs/", root_name, "_formatted.csv"))
object_filename <- here(paste0("data/formatted/final_objects/", root_name, "_object.rds"))
plot_filename   <- here(paste0("figures/plots/", root_name, "_plot.png"))

# Ensure destination directories exist
dir.create(here("data/formatted/formatted_csvs"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data/formatted/final_objects"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("figures/plots"), recursive = TRUE, showWarnings = FALSE)

#----------------------------------------------------
#### 1. Define Gulf-Adjacent Counties & Filter Data ####

raw_county <- read.csv(raw_filename, stringsAsFactors = FALSE)

# Build county FIPS lookup table from the dataset where names are populated
fips_lookup <- raw_county |>
  filter(!is.na(stateName) & !is.na(county_name)) |>
  distinct(fips, stateName, state, county_name)

# 1. Western Gulf States (All counties in the dataset for TX, LA, MS, AL are coastal Gulf)
western_fips <- fips_lookup |>
  filter(stateName %in% c("Texas", "Louisiana", "Mississippi", "Alabama")) |>
  pull(fips)

# 2. Florida Gulf-adjacent counties west/northwest of the Florida Keys
fl_gulf_county_names <- c(
  "Bay", "Charlotte", "Citrus", "Collier", "Dixie", "Escambia",
  "Franklin", "Gulf", "Hernando", "Hillsborough", "Jefferson",
  "Lee", "Levy", "Manatee", "Okaloosa", "Pasco", "Pinellas",
  "Santa Rosa", "Sarasota", "Taylor", "Wakulla", "Walton"
)

# Optional toggle for Monroe County (Florida Keys)
include_monroe_keys <- FALSE
if (include_monroe_keys) {
  fl_gulf_county_names <- c(fl_gulf_county_names, "Monroe")
}

fl_gulf_fips <- fips_lookup |>
  filter(stateName == "Florida" & county_name %in% fl_gulf_county_names) |>
  pull(fips)

# Combine all targeted Gulf FIPS codes
target_gulf_fips <- c(western_fips, fl_gulf_fips)
message(paste("Identified", length(target_gulf_fips), "Gulf-adjacent counties."))

# Filter for Gulf-adjacent counties and 'All Ocean Sectors'
gulf_econ_filtered <- raw_county |>
  filter(fips %in% target_gulf_fips & enowSector == "All Ocean Sectors")

# Aggregate across all Gulf-adjacent counties by year
clean_econ <- gulf_econ_filtered |>
  group_by(year) |>
  summarise(
    # Scale dollars into Billions USD to keep y-axis labels clean
    real_wages = round(sum(real_wages, na.rm = TRUE) / 1e9, 2),
    rgdp       = round(sum(rgdp, na.rm = TRUE) / 1e9, 2),
    employment = sum(employment, na.rm = TRUE),
    .groups    = "drop"
  ) |>
  arrange(year) |>
  mutate(year = as.character(year))

head(clean_econ)

#----------------------------------------------------
#### 2. Clean Data & Create Time Series CSV ####

# Define header metadata components aligned with each of the 3 columns
indicator_names <- rep("Total Marine Economy", 3)
unit_names      <- c("Real Wages (Billion USD)", "Real GDP (Billion USD)", "Employment (Jobs)")
extent_names    <- rep("Gulf-wide", 3)

# Convert to standardized long-header format required by IEAnalyzeR
econ_formatted <- IEAnalyzeR::convert_cleaned_data(
  clean_econ,
  indicator_names = indicator_names,
  unit_names      = unit_names,
  extent_names    = extent_names
)

head(econ_formatted)

#----------------------------------------------------
#### 3. Save Formatted Data as CSV ####

write.csv(econ_formatted, file = csv_filename, row.names = FALSE)

#----------------------------------------------------
#### 4. Create Data_Prep Object ####

econ_obj <- IEAnalyzeR::data_prep(econ_formatted)

#----------------------------------------------------
#### 5. Save Formatted data_prep Object ####

saveRDS(econ_obj, file = object_filename)

#----------------------------------------------------
#### 6. Preview Stacked Plot ####

# sep_ylabs = TRUE splits the 3 metrics into stacked, aligned subpanels
stacked_plot <- IEAnalyzeR::plot_fn_obj(
  df_obj = econ_obj,
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
  width = 8,
  height = 8.5,
  units = "in",
  dpi = 300
)