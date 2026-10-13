# File created on 2026-10-13

#### 0. Setup ####
# Add any packages that are needed for analysis here.
library(IEAnalyzeR)
library(here)
library(ggplot2)

# File Naming Setup.
# !! Auto generated-Do Not Change !!
root_name <- "Mean_trophic_level"

csv_filename <- here(paste0("data/formatted/formatted_csvs/", root_name, "_formatted.csv"))
object_filename <- here(paste0("data/formatted/final_objects/", root_name, "_object.rds"))
plot_filename <- here(paste0("figures/plots/", root_name, "_plot.png"))

#----------------------------------------------------
#### 1. Read Data ####
# Pull data from its source:
# Manual data: data/unformatted data
# Automated data: Add script for data call (API, package, etc.)
# Confidential data: Store locally in the confidential data folder
#   - This folder is excluded using gitignore and will not push to the GitHub repo
# If intermediate data (shapefiles etc.) are needed, please put them in data>intermediate
#   - Filename should use the syntax rootname_descriptivename

managed <- read.csv(here("data/unformatted/managed_species_landings_with_trophic.csv"))
unmanaged <- read.csv(here("data/unformatted/unmanaged_species_landings_with_trophic.csv"))

#----------------------------------------------------
#### 2. Clean data and create time series csv ####

#Transform the data to fit the IEA data format.
#For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
#Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.

#Define header components for the data rows (ignore year). Fill in the blanks here.
# Calculate annual catch-weighted mean trophic level.
calculate_mtl <- function(data) {

  years <- sort(unique(data$LAND_YEAR))

  mtl <- sapply(years, function(y) {

    d <- data[data$LAND_YEAR == y, ]

    # Use only positive landings with known trophic levels
    valid <- !is.na(d$`TOTAL_LIVE_POUNDS`) &
      is.finite(d$`TOTAL_LIVE_POUNDS`) &
      d$`TOTAL_LIVE_POUNDS` > 0 &
      !is.na(d$trophic) &
      is.finite(d$trophic)

    d <- d[valid, ]

    if (nrow(d) == 0) {
      return(NA_real_)
    }

    weighted.mean(
      d$trophic,
      d$`TOTAL_LIVE_POUNDS`
    )
  })

  data.frame(year = years, MTL = as.numeric(mtl))
}

managed_mtl <- calculate_mtl(managed)
unmanaged_mtl <- calculate_mtl(unmanaged)

# Combine the two annual time series
your_data <- merge(
  managed_mtl,
  unmanaged_mtl,
  by = "year",
  all = TRUE,
  suffixes = c("_managed", "_unmanaged")
)

# Arrange columns in the required format
names(your_data) <- c(
  "year",
  "Managed species",
  "Unmanaged species"
)

your_data <- your_data[order(your_data$year), ]

# Define header components for the data rows (ignore year).
indicator_names <- c(
  "Managed species",
  "Unmanaged Species"
)

unit_names <- c(
  "Trophic level",
  "Trophic level"
)

extent_names <- c(
  "Managed species",
  "Unmanaged species"
)

formatted_data <- IEAnalyzeR::convert_cleaned_data(
  your_data,
  indicator_names,
  unit_names,
  extent_names
)


#----------------------------------------------------
#### 3. Save Formatted data as csv ####
 
# This will save your data to the appropriate folder.

write.csv(formatted_data, file = csv_filename, row.names = F)

#----------------------------------------------------
#### 4. Create Data_Prep object ####
  
#Please use your formatted csv to create a "data_prep" object.
#For more info on the data_prep function see the vignette linked above.

data_obj <- IEAnalyzeR::data_prep(csv_filename)


#----------------------------------------------------
#### 5. Save Formatted data_prep object ####

#This will save your data to the appropriate folder.
  
saveRDS(data_obj, file = object_filename)


#----------------------------------------------------
#### 6. Preview Plot ####
# Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# For more info on the plot_fn_obj function go HERE

#IEAnalyzeR::plot_fn_obj(df_obj = data_obj, trend = TRUE)
p <- IEAnalyzeR::plot_fn_obj(df_obj = data_obj, trend = TRUE) +
  ggplot2::labs(title = "Catch Weighted Mean Trophic Level")

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

ggsave(filename = plot_filename, plot=p)
