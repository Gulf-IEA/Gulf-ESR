# File created on 2026-08-14

#### 0. Setup ####
# Add any packages that are needed for analysis here.
library(IEAnalyzeR)
library(here)
library(ggplot2)
library(dplyr)

# File Naming Setup.
# !! Auto generated-Do Not Change !!
root_name <- "Days-at-sea"

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

### confidential Coastal Fisheries Logbook Data request an NDA on file
setwd('~/CMP/data/cflp/')
cflp <- readRDS('CFLPBlake.rds')
names(cflp)
cflp_gom <- subset(cflp, cflp$REGION == "GOM") # select only Gulf

#----------------------------------------------------
#### 2. Clean data and create time series csv ####

#Transform the data to fit the IEA data format.
#For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
#Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.

### find unique trips and sum days at sea by year; SCHEDULE_NUMBER is unique ID for trips; multiple rows per trip so just select the first and sum per year
tmp <- cflp_gom[!duplicated(cflp_gom$SCHEDULE_NUMBER),] 
days_year <- aggregate(DAYS_AWAY ~ LAND_YEAR, data = tmp, sum, na.rm = T)
### gut check
plot(days_year$LAND_YEAR, days_year$DAYS_AWAY, 
     type = "b", xlab = "Year", ylab = "Days at Sea", main = "Days at Sea by Year",
     ylim = c(0, max(days_year$DAYS_AWAY, na.rm = T)))

### mandatory reporting for all coastal fisheries 1993 and mandatory for king mackerel landings 1999
days_year99 <- subset(days_year, LAND_YEAR>=1999)


#Define header components for the data rows (ignore year). Fill in the blanks here.
indicator_names = c('Days at sea')
unit_names = c('Days')
extent_names = c('Gulf-wide')

formatted_data = IEAnalyzeR::convert_cleaned_data(days_year99, indicator_names, unit_names, extent_names)


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

IEAnalyzeR::plot_fn_obj(df_obj = data_obj, trend = T, 
                        pts = T, pt_size = 1, fig.width = 7)  #+
  # scale_y_continuous(limits = c(0, 6e4))

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height = , width = , unit = "in") if needed.

ggsave(filename = plot_filename, height = 4, width = 7, unit = "in")
