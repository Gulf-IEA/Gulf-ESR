
# File created on 2026-03-16 by B. Turley

#### 0. Setup ####
library(IEAnalyzeR)
library(here)
library(ggplot2)

# File Naming Setup.
root_name <- "Storminess"

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

setwd(here('data/intermediate'))
dat <- read.csv('offshore-winds-chart.csv', skip = 5)

#----------------------------------------------------
#### 2. Clean data and create time series csv ####

#Transform the data to fit the IEA data format.
#For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
#Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.

#Define header components for the data rows (ignore year). Fill in the blanks here.
indicator_names = c('Annual Near Gale (≥28kts) Events with ≥25% Coverage',
                    'Annual Gale (≥34kts) Events with ≥25% Coverage')
unit_names = rep('Event count',2)
extent_names = rep('Gulf-wide', 2)

formatted_data = IEAnalyzeR::convert_cleaned_data(dat[,c(1,3,5)], indicator_names, unit_names, extent_names)


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

IEAnalyzeR::plot_fn_obj(df_obj = data_obj, 
                        trend = TRUE,
                        manual_title = 'Storminess',
                        fig.width = 10,
                        pts = T, pt_size = 2)

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

ggsave(filename = plot_filename)

