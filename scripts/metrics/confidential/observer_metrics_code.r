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
# object_filename <- here(paste0("data/formatted/final_objects/", root_name, "_object.rds"))
# plot_filename <- here(paste0("figures/plots/", root_name, "_plot.png"))

#----------------------------------------------------
#### 1. Read Data ####
# Pull data from its source:
# Manual data: data/unformatted data
# Automated data: Add script for data call (API, package, etc.)
# Confidential data: Store locally in the confidential data folder
#   - This folder is excluded using gitignore and will not push to the GitHub repo
# If intermediate data (shapefiles etc.) are needed, please put them in data>intermediate
#   - Filename should use the syntax rootname_descriptivename

rf_obs <- read_xlsx('Reef Fish Observer Program Coverage Rates by Year, Region, and Trip Gear (2007-2025).xlsx')

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

#Define header components for the data rows (ignore year). Fill in the blanks here.
indicator_names = c("BLL Trips observed",
                    "BLL Vessels observed",
                    "BLL Seadays Observed",
                    "VL Trips observed",
                    "VL Vessels observed",
                    "VL Seadays Observed",
                    "Total Trips observed",
                    "Total Vessels observed",
                    "Total Seadays Observed")
unit_names = rep("Proporition Observed", length(indicator_names))
extent_names = c(rep('Bottom Longline', 3), rep('Vertical Line', 3), rep('Vertical Line + Bottom Longline', 3))

formatted_data = IEAnalyzeR::convert_cleaned_data(rf_obs_sub, indicator_names, unit_names, extent_names)


#----------------------------------------------------
#### 3. Save Formatted data as csv ####

# This will save your data to the appropriate folder.

write.csv(formatted_data, file = csv_filename, row.names = F)

#----------------------------------------------------
#### 4. Create Data_Prep object ####

#Please use your formatted csv to create a "data_prep" object.
#For more info on the data_prep function see the vignette linked above.

bll_obj <- IEAnalyzeR::data_prep(formatted_data[,c(1,2:4)])
vl_obj <- IEAnalyzeR::data_prep(formatted_data[,c(1,5:7)])
total_obj <- IEAnalyzeR::data_prep(formatted_data[,c(1,8:10)])

#----------------------------------------------------
#### 5. Save Formatted data_prep object ####

#This will save your data to the appropriate folder.

object_filename1 <- here(paste0("data/formatted/final_objects/", root_name, "_object1.rds"))
object_filename2 <- here(paste0("data/formatted/final_objects/", root_name, "_object2.rds"))
object_filename3 <- here(paste0("data/formatted/final_objects/", root_name, "_object3.rds"))

saveRDS(bll_obj, file = object_filename1)
saveRDS(vl_obj, file = object_filename2)
saveRDS(total_obj, file = object_filename3)


#----------------------------------------------------
#### 6. Preview Plot ####
# Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# For more info on the plot_fn_obj function go HERE

IEAnalyzeR::plot_fn_obj(df_obj = bll_obj, trend = TRUE, 
                        manual_title = 'Bottom Longline Trips Observed',
                        ncol = 2, fig.width = 9, pts = T, pt_size = 2)

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

plot_filename1 <- here(paste0("figures/plots/", root_name, "_plot1.png"))

ggsave(filename = plot_filename1, height = 5, width = 9, unit = "in")


#----------------------------------------------------
#### 6. Preview Plot ####
# Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# For more info on the plot_fn_obj function go HERE

IEAnalyzeR::plot_fn_obj(df_obj = vl_obj, trend = TRUE,
                        manual_title = 'Vertical Line Trips Observed',
                        ncol = 2, fig.width = 9, pts = T, pt_size = 2)

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

plot_filename2 <- here(paste0("figures/plots/", root_name, "_plot2.png"))

ggsave(filename = plot_filename2, height = 5, width = 9, unit = "in")


#----------------------------------------------------
#### 6. Preview Plot ####
# Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# For more info on the plot_fn_obj function go HERE

IEAnalyzeR::plot_fn_obj(df_obj = total_obj, trend = TRUE,
                        manual_title = 'Total Reef Trips Observed', 
                        ncol = 2, fig.width = 9, pts = T, pt_size = 2)

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

plot_filename3 <- here(paste0("figures/plots/", root_name, "_plot3.png"))

ggsave(filename = plot_filename3, height = 5, width = 9, unit = "in")
