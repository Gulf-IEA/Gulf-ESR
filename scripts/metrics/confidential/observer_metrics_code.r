# File created on 2026-10-21 by B. Turley

#### 0. Setup ####
# Add any packages that are needed for analysis here.
library(IEAnalyzeR)
library(here)
library(ggplot2)
library(readxl)
library(dplyr)

# File Naming Setup.
# !! Auto generated-Do Not Change !!
root_name <- "observer_metrics"

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

rf_obs <- read_xlsx(here('data/unformatted/confidential/Reef Fish Observer Program Coverage Rates by Year, Region, and Trip Gear (2007-2025).xlsx'))
shrimp <- read.csv(here("data/unformatted/confidential/gulf_shrimp_observer.csv"))

#----------------------------------------------------
#### 2. Clean data and create time series csv ####

#Transform the data to fit the IEA data format.
#For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
#Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.

### subset for GOM, keep year and proporition observed
rf_obs_sub <- subset(rf_obs, REGION == "GOM") |>
  select(YEAR, GEAR, PROP_TRIPS, PROP_VESSELS, PROP_SEADAYS) |>
  type.convert(as.is = TRUE) |> 
  as.data.frame() |> 
  reshape(idvar = "YEAR", timevar = "GEAR", direction = "wide")

### add row in shrimp to align
shrimp[19,] <- c(2025, NA, NA, NA)


#Define header components for the data rows (ignore year). Fill in the blanks here.
indicator_names = rep('Observer Effort',3)
unit_names = c("Proporition Seadays Observed","Proporition Seadays Observed","Proporition Hours Observed")
extent_names = c('Bottom Longline', 'Vertical Line', 'Shrimp')

formatted_data = IEAnalyzeR::convert_cleaned_data(cbind(rf_obs_sub[,c(1,4,7)], shrimp[,4]), 
                                                  indicator_names, unit_names, extent_names)


#----------------------------------------------------
#### 3. Save Formatted data as csv ####

# This will save your data to the appropriate folder.

write.csv(formatted_data, file = csv_filename, row.names = F)

#----------------------------------------------------
#### 4. Create Data_Prep object ####

#Please use your formatted csv to create a "data_prep" object.
#For more info on the data_prep function see the vignette linked above.

data_obj <- IEAnalyzeR::data_prep(formatted_data)

#----------------------------------------------------
#### 5. Save Formatted data_prep object ####

#This will save your data to the appropriate folder.

saveRDS(data_obj, file = object_filename)

#----------------------------------------------------
#### 6. Preview Plot ####
# Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# For more info on the plot_fn_obj function go HERE

IEAnalyzeR::plot_fn_obj(df_obj = data_obj, trend = TRUE, 
                        manual_title = 'Fisheries Observer Effort',
                        ylab_sublabel = c('extent','unit'),
                        sep_ylabs = T,
                        ncol = 2, fig.width = 10, pts = T, pt_size = 2) +
  theme(plot.title = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 12, face = 'plain'),
        axis.text = element_text(size = 11, face = 'plain'),
        strip.text = element_text(size = 12, face = 'plain'))

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

ggsave(filename = plot_filename, height = 5, width = 10, unit = "in")
