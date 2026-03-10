
# File created on 2026-03-10 by B. Turley

#### 0. Setup ####
library(IEAnalyzeR)
library(here)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)

# File Naming Setup.
root_name <- "ACL_Open-Close-dates"

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

### for ACLs
#### Load data ####
# Load the MH Data Log
mh <- readRDS(here('data/intermediate/MH_DL_2025Sep10.RDS'))


#### Define species, region, and sector of interest ####
species = c('SNAPPER, CUBERA', 'AMBERJACK, GREATER', 'AMBERJACK, LESSER', 'JACK, ALMACO', 'SNAPPER, SILK', 'SNAPPER, QUEEN', 'SNAPPER, BLACKFIN', 'WENCHMAN',
            'DRUM, RED', 'COBIA', 'GROUPER, GAG', 'SNAPPER, GRAY', 'MACKEREL, KING', 'GROUPER, YELLOWEDGE', 'SNAPPER, LANE', 'GROUPER, RED', 'SNAPPER, RED', 
            'SCAMP', 'MACKEREL, SPANISH','HOGFISH', 'SNAPPER, MUTTON', 'SNAPPER, YELLOWTAIL', 'SNAPPER, VERMILION', 'TRIGGERFISH, GRAY', 'TRIGGERFISH, QUEEN')
region = 'GULF OF MEXICO'
sector = 'RECREATIONAL'
# sector = 'COMMERCIAL'
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
  filter(SUBSECTOR=='ALL') #|>
# filter(SUBSECTOR=='ALL' & SECTOR=='ALL')

# yllw_scmp <- mh %>%
#   filter(COMMON_NAME_USE %in% c('GROUPER, YELLOWEDGE', 'SCAMP'),
#          REGION %in% region,
#          DETAILED == "YES",
#          NEVER_IMPLEMENTED %in% c(0, NA),
#          REG_REMOVED == 0,
#          MANAGEMENT_TYPE_USE == "ACL",
#          # MANAGEMENT_TYPE == "ACL",
#          VALUE_UNITS == 'POUNDS') |>
#   arrange(CLUSTER, START_DATE2) |> type.convert() |>
#   filter(SUBSECTOR=='ALL') #|>
# # filter(SUBSECTOR=='ALL' & SECTOR=='ALL')
# 
# acls <- rbind(yllw_scmp, acls)

acl_slice <- acls |>
  arrange(COMMON_NAME_USE, START_YEAR, SECTOR, EFFECTIVE_DATE) |>
  group_by(COMMON_NAME_USE, START_YEAR, SECTOR) |>
  slice_max(order_by = EFFECTIVE_DATE, n = 1, with_ties = F) |>
  select(COMMON_NAME_USE, SPP_TYPE, SPP_NAME, MANAGEMENT_TYPE_USE, START_YEAR, 
         SECTOR, SUBSECTOR, VALUE, VALUE_UNITS, VALUE_TYPE)

### check species
spp <- unique(acl_slice$COMMON_NAME_USE)
spp


setwd(here('data/intermediate/'))
write.csv(acl_slice, 'acl_tac_quota.csv', row.names = F)



### for open and close dates
#### Load data ####
# Load the MH Data Log
# mh <- readRDS(here("ODM-MH-Data_log", "data", "results", "MH_DL_2025Sep10.RDS"))
mh <- readRDS(here('data/intermediate/MH_DL_2025Sep10.RDS'))

# Function to expand dates based on management status
source(here("scripts/helper_functions/func_expand_status.R"))


#### Define species, region, and sector of interest ####
species = c('SNAPPER, CUBERA', 'AMBERJACK, GREATER', 'AMBERJACK, LESSER', 'JACK, ALMACO', 'SNAPPER, SILK', 'SNAPPER, QUEEN', 'SNAPPER, BLACKFIN', 'WENCHMAN',
            'DRUM, RED', 'COBIA', 'GROUPER, GAG', 'SNAPPER, GRAY', 'MACKEREL, KING', 'GROUPER, YELLOWEDGE', 'SNAPPER, LANE', 'GROUPER, RED', 'SNAPPER, RED', 
            'SCAMP', 'MACKEREL, SPANISH','HOGFISH', 'SNAPPER, MUTTON', 'SNAPPER, YELLOWTAIL', 'SNAPPER, VERMILION', 'TRIGGERFISH, GRAY', 'TRIGGERFISH, QUEEN')
region = 'GULF OF MEXICO'

management_type_use <- 'CLOSURE'

review_code <- T # this loop can take awhile; set to F to rerun the loop

if(review_code==F){
  
  output <- c()
  
  for(i in species){
    
    print(i)
    
    # Filter and reclassify closure-related records ####
    # Retain only species and region of interest and management types that impact the status of the fishery
    # Provide management types that impact fishery status with a VALUE of OPEN or CLOSE
    # FISHING SEASON/FISHING YEAR implies that the fishery is OPEN during the defined window
    # PROHIBITED SALE AND PURCHASE 
    #    - If FLAG = YES -> OPEN (sale is allowed)
    #    - If FLAG = NO -> CLOSE (sale is not allowed)
    # PROHIBITED SPECIES implies the fishery is closed as species cannot be retained
    # CLOSURE VALUE already indicates fishery status
    mh_spp_closure <- mh %>%
      filter(COMMON_NAME_USE == i,
             REGION == region,
             MANAGEMENT_TYPE_USE %in% c('CLOSURE', 'FISHING SEASON', 'FISHING YEAR',
                                        'PROHIBITED SALE AND PURCHASE',
                                        'PROHIBITED SPECIES')) %>%
      mutate(VALUE = case_when(MANAGEMENT_TYPE_USE %in% c('FISHING SEASON', 'FISHING YEAR') ~ 'OPEN',
                               MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'YES' ~ 'OPEN',
                               MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'NO' ~ 'CLOSE',
                               MANAGEMENT_TYPE_USE == 'PROHIBITED SPECIES' ~ 'CLOSE',
                               TRUE ~ VALUE)) %>%
      arrange(SECTOR_USE, START_DATE2)
    
    # Check to see which MANAGEMENT_TYPE_USE are present after filtering
    mtu <- unique(mh_spp_closure$MANAGEMENT_TYPE_USE)
    
    cat('\n', i, '\n' ,mtu, '\n')
    
    spp_year <- expand_status(mh_spp_closure, "FISHING YEAR")
    spp_closures <- expand_status(mh_spp_closure, "CLOSURE")
    
    # Combine all management types that refer to closures
    # For now just comment out mtypes that do not apply
    spp_closure_story <- spp_year %>%
      rename(FR_CITATION_year = "FR_CITATION",
             VALUE_year = "VALUE") %>%
      select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_year, VALUE_year) %>%
      # full_join(spp_season %>%
      #             rename(FR_CITATION_season = "FR_CITATION",
      #                    VALUE_season = "VALUE") %>%
      #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_season, VALUE_season),
      #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
      full_join(spp_closures %>%
                  rename(FR_CITATION_close = "FR_CITATION",
                         VALUE_close = "VALUE") %>%
                  select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_close, VALUE_close),
                by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
      # full_join(spp_prohibited_sale %>%
      #            rename(FR_CITATION_sale = "FR_CITATION",
      #                    VALUE_sale = "VALUE") %>%
      #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_sale, VALUE_sale),
      #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
      # full_join(spp_prohibited_spp %>%
      #             rename(FR_CITATION_spp = "FR_CITATION",
      #                    VALUE_spp = "VALUE") %>%
      #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_spp, VALUE_spp),
      #             by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence)) %>%
      arrange(SECTOR_USE, SUBSECTOR_USE, ZONE_USE, date_sequence) %>%
      # Select the most recent FR CITATION
      mutate(FR_CITATION = pmax(FR_CITATION_year, 
                                #FR_CITATION_season, 
                                FR_CITATION_close, 
                                #                            FR_CITATION_sale, 
                                #                            FR_CITATION_spp,
                                na.rm = T)) %>%
      # Select the fishery status (open/closed) that applies to the most recent FR
      mutate(VALUE = case_when(FR_CITATION == FR_CITATION_close ~ VALUE_close,
                               #                           FR_CITATION == FR_CITATION_sale ~ VALUE_sale,
                               #                          FR_CITATION == FR_CITATION_spp ~ VALUE_spp,
                               #FR_CITATION == FR_CITATION_season ~ VALUE_season,
                               FR_CITATION == FR_CITATION_year ~ VALUE_year)) %>%
      select(-FR_CITATION_year, -VALUE_year, -FR_CITATION_close, -VALUE_close) #, -FR_CITATION_sale, -VALUE_sale, -FR_CITATION_season, -VALUE_season)
    
    # Summarize the open and closed periods for each year
    summ_spp_closures <- spp_closure_story %>%
      mutate(YEAR = year(date_sequence)) %>%
      group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>% 
      arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence) %>%
      mutate(change = VALUE != lag(VALUE, default = first(VALUE)) |
               YEAR != lag(YEAR, default = first(YEAR))) %>%
      mutate(group = cumsum(change)) %>%
      group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE, group) %>%
      summarize(ndays = n(),
                start = min(date_sequence),
                end = max(date_sequence),
                .groups = 'drop') %>%
      mutate(VALUE = case_when(is.na(VALUE) ~ "OPEN",
                               TRUE ~ VALUE)) %>%
      arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, start)
    
    results <- subset(summ_spp_closures, SUBSECTOR_USE=='ALL' & ZONE_USE=='ALL', 
                      select = c('COMMON_NAME_USE','SECTOR_USE','YEAR','VALUE','start','end')) |>
      arrange(YEAR, start, end, SECTOR_USE, VALUE)
    
    if(i == species[1]){
      output <- results
    } else {
      output <- bind_rows(output, results)
    }
    
    setwd(here("data/intermediate"))
    save(output, file='spp_open_close_output.RData')
    
  }
  
} else {
  
  setwd(here("data/intermediate"))
  load('spp_open_close_output.RData')
  
}


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
# data_obj <- IEAnalyzeR::data_prep(csv_filename)
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
# IEAnalyzeR::plot_fn_obj(df_obj = data_obj, trend = TRUE)
# 
# #----------------------------------------------------
# #### 7. Save plot ####
# # This will save the plot to the correct folder.
# # Adjust height & width using (height=, width=, unit="in") if needed.
# 
# ggsave(filename = plot_filename)
