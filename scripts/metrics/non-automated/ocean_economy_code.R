#### 0. Setup ####
library(IEAnalyzeR)
library(here)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)

# File Naming Setup
root_name <- "total-marine-economy"

# File paths relative to the repository root
raw_filename    <- here("data/unformatted/OpenENOW_GulfRegion.csv")
csv_filename    <- here(paste0("data/formatted/formatted_csvs/", root_name, "_formatted.csv"))
object_filename <- here(paste0("data/formatted/final_objects/", root_name, "_object.rds"))
plot_filename   <- here(paste0("figures/plots/", root_name, "_plot.png"))

# Ensure destination directories exist
dir.create(here("data/formatted/formatted_csvs"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data/formatted/final_objects"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("figures/plots"), recursive = TRUE, showWarnings = FALSE)

#----------------------------------------------------
#### 1. Read & Process Raw Data ####

raw_econ <- read.csv(raw_filename)

# Filter for the entire marine economy across all sectors
total_marine_econ <- raw_econ |>
  filter(enowSector == "All Ocean Sectors") |>
  arrange(year)

# Optional scaling: Raw dollars are in the tens of billions (e.g., $19B - $154B).
# Dividing by 1e9 scales real wages and rgdp into Billions USD for cleaner plot labels.
# If you prefer raw unscaled dollars, remove the `/ 1e9` and update unit_names below.
clean_econ <- total_marine_econ |>
  transmute(
    year       = as.character(year),
    real_wages = round(real_wages / 1e9, 2), # Billions USD
    rgdp       = round(rgdp / 1e9, 2),       # Billions USD
    employment = employment                  # Total number of jobs
  )

head(clean_econ)

#----------------------------------------------------
#### 2. Clean Data & Create Time Series CSV ####

# Define header metadata components aligned with each of the 3 columns:
# Col 1: real_wages, Col 2: rgdp, Col 3: employment
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

# sep_ylabs = TRUE splits the 3 metrics into stacked, aligned panels
stacked_plot <- IEAnalyzeR::plot_fn_obj(
  df_obj = econ_obj,
  trend = TRUE,
  sep_ylabs = TRUE,
  ylab_sublabel = c("extent", "unit")
)

print(stacked_plot)

#----------------------------------------------------
#### 7. Save Plot ####

# Height is set to 8.5 inches to comfortably fit all 3 stacked subpanels
ggsave(
  filename = plot_filename,
  plot = stacked_plot,
  width = 8,
  height = 8.5,
  units = "in"
)