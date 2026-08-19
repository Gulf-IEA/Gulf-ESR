# File created on 2026-08-19

#### 0. Setup ####
# Add any packages that are needed for analysis here.
library(IEAnalyzeR)
library(here)
library(ggplot2)

# File Naming Setup.
# !! Auto generated-Do Not Change !!
root_name <- "Coral_EFGB"

csv_filename <- here(paste0("data/formatted/formatted_csvs/", root_name, "_formatted.csv"))
object_filename <- here(paste0("data/formatted/final_objects/", root_name, "_object.rds"))
plot_filename <- here(paste0("figures/plots/", root_name, "_plot.png"))

# ----------------------------------------------------
# ### 1. Read Data ####
# Pull data from its source:
# Manual data: data/unformatted data
# Automated data: Add script for data call (API, package, etc.)
# Confidential data: Store locally in the confidential data folder
#   - This folder is excluded using gitignore and will not push to the GitHub repo
# If intermediate data (shapefiles etc.) are needed, please put them in data>intermediate
#   - Filename should use the syntax rootname_descriptivename

# Read the raw benthic cover data
raw_data <- read.csv(here("data/unformatted/2025EFGBRT.csv"))

# Keep only the variables needed for the time series
raw_data <- raw_data[, c("Year", "Benthic", "Cover")]

# Reshape from stacked format to one column per benthic category
your_data <- tidyr::pivot_wider(
  raw_data,
  names_from = Benthic,
  values_from = Cover
)


# ----------------------------------------------------
# ### 2. Clean data and create time series csv ####

# Transform the data to fit the IEA data format.
# For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
# Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.

# Define header components for the data rows (ignore year). Fill in the blanks here.
# indicator_names = c("")
# unit_names = c("")
# extent_names = c("")

indicator_names = c(
  "Coral",
  "Macroalgae",
  "Sponge",
  "Colonizable Substrate",
  "CCA",
  "Sand",
  "Hydrocoral"
)

unit_names = c(
  "% area",
  "% area",
  "% area",
  "% area",
  "% area",
  "% area",
  "% area"
)

extent_names = c(
  "East Flower Garden Banks",
  "East Flower Garden Banks",
  "East Flower Garden Banks",
  "East Flower Garden Banks",
  "East Flower Garden Banks",
  "East Flower Garden Banks",
  "East Flower Garden Banks"
)


formatted_data = IEAnalyzeR::convert_cleaned_data(your_data, indicator_names, unit_names, extent_names)


# ----------------------------------------------------
# ### 3. Save Formatted data as csv ####

# This will save your data to the appropriate folder.

write.csv(formatted_data, file = csv_filename, row.names = F)

# ----------------------------------------------------
# ### 4. Create Data_Prep object ####

# Please use your formatted csv to create a "data_prep" object.
# For more info on the data_prep function see the vignette linked above.

data_obj <- IEAnalyzeR::data_prep(csv_filename)


# ----------------------------------------------------
# ### 5. Save Formatted data_prep object ####

# This will save your data to the appropriate folder.

saveRDS(data_obj, file = object_filename)


# ----------------------------------------------------
# ### 6. Preview Plot ####
# Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# For more info on the plot_fn_obj function go HERE

plot <- IEAnalyzeR::plot_fn_obj(df_obj = data_obj,trend = TRUE)
# Override the title
plot <- plot + ggplot2::labs(title = "East Flower Garden Banks") + ggplot2::facet_wrap(~indicator, ncol = 1, scales = "free_y")
plot

# ----------------------------------------------------
# ### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

ggsave(
  filename = plot_filename,
  plot = plot,
  width = 6,
  height = 14,
  units = "in"
)
