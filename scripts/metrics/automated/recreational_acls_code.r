
# File created on 2026-04-09 by B. Turley

#### 0. Setup ####
# Add any packages that are needed for analysis here.
library(IEAnalyzeR)
library(here)
library(ggplot2)
library(dplyr)
library(lubridate)

# File Naming Setup.
root_name<- "recreational_acls"

# csv_filename <- here(paste0("data/formatted/formatted_csvs/", root_name, "_formatted.csv"))
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

# Load the MH Data Log

################################################
###### data come from a SEFSC github repo ###### 
##### planned migration to Oracle Database #####
##### need to be revisited next ESR update #####
################################################

# mh <- readRDS('https://github.com/SEFSC/SEFSC-ODM-Management-History/blob/main/ODM-MH-Data_log/data/results/MH_DL_2025Sep10.RDS')
mh <- readRDS(here(paste0("data/intermediate/MH_DL_2025Sep10.rds")))


#### Define species, region, and sector of interest ####
species = c('SNAPPER, CUBERA', 'AMBERJACK, GREATER', 'AMBERJACK, LESSER', 'JACK, ALMACO', 'SNAPPER, SILK', 'SNAPPER, QUEEN', 'SNAPPER, BLACKFIN', 'WENCHMAN',
            'DRUM, RED', 'COBIA', 'GROUPER, GAG', 'SNAPPER, GRAY', 'MACKEREL, KING', 'GROUPER, YELLOWEDGE', 'SNAPPER, LANE', 'GROUPER, RED', 'SNAPPER, RED', 
            'SCAMP', 'MACKEREL, SPANISH','HOGFISH', 'SNAPPER, MUTTON', 'SNAPPER, YELLOWTAIL', 'SNAPPER, VERMILION', 'TRIGGERFISH, GRAY', 'TRIGGERFISH, QUEEN')
region = 'GULF OF MEXICO'
sector = 'RECREATIONAL'
management_type_use<- c('ACL','ACT', 'TAC', 'QUOTA')

acls <- mh %>%
  filter(COMMON_NAME_USE %in% species,
         REGION %in% region,
         DETAILED == "YES",
         NEVER_IMPLEMENTED %in% c(0, NA),
         REG_REMOVED == 0,
         # MANAGEMENT_TYPE_USE == "ACL",
         (MANAGEMENT_TYPE_USE %in% management_type_use),
         # MANAGEMENT_TYPE == "ACL",
         # SPP_TYPE == 'COMMON_NAME', ### removing this gets yellowedge groupers and scamp
         VALUE_UNITS == 'POUNDS') %>%
  arrange(CLUSTER, START_DATE2) |> type.convert(as.is = T) |>
  filter(SUBSECTOR=='ALL')

acl_slice <- acls |>
  arrange(COMMON_NAME_USE, START_YEAR, SECTOR, EFFECTIVE_DATE) |>
  group_by(COMMON_NAME_USE, START_YEAR, SECTOR) |>
  slice_max(order_by = EFFECTIVE_DATE, n = 1, with_ties = F) |>
  select(COMMON_NAME_USE, SPP_TYPE, SPP_NAME, MANAGEMENT_TYPE_USE, START_YEAR, 
         SECTOR, SUBSECTOR, VALUE, VALUE_UNITS, VALUE_TYPE)

setwd(here(paste0("data/intermediate/")))
write.csv(acl_slice, file = 'acl_tac_quota.csv', row.names = F)


##############################################################################
### the outputs are for use in other indicators and no plots were intended ###
##############################################################################


# #----------------------------------------------------
# #### 2. Clean data and create time series csv ####
# 
# #Transform the data to fit the IEA data format.
# #For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
# #Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.
# 
# #Define header components for the data rows (ignore year). Fill in the blanks here.
# indicator_names = c("")
# unit_names = c("")
# extent_names = c("")
# 
# formatted_data = IEAnalyzeR::convert_cleaned_data(your_data, indicator_names, unit_names, extent_names)
# 
# 
# #----------------------------------------------------
# #### 3. Save Formatted data as csv ####
#  
# # This will save your data to the appropriate folder.
# 
# write.csv(formatted_data, file = csv_filename, row.names = F)
# 
# #----------------------------------------------------
# #### 4. Create Data_Prep object ####
#   
# #Please use your formatted csv to create a "data_prep" object.
# #For more info on the data_prep function see the vignette linked above.
# 
# data_obj<-IEAnalyzeR::data_prep(csv_filename)
# 
# 
# #----------------------------------------------------
# #### 5. Save Formatted data_prep object ####
# 
# #This will save your data to the appropriate folder.
#   
# saveRDS(data_obj, file = object_filename)
# 
# 
# #----------------------------------------------------
# #### 6. Preview Plot ####
# # Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# # For more info on the plot_fn_obj function go HERE
# 
# IEAnalyzeR::plot_fn_obj(df_obj = data_obj, trends = TRUE)
# 
# #----------------------------------------------------
# #### 7. Save plot ####
# # This will save the plot to the correct folder.
# # Adjust height & width using (height=, width=, unit="in") if needed.
# 
# ggsave(filename = plot_filename)
