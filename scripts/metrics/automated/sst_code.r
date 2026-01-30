
# File created on 2026-01-30 by B. Turley

#### 0. Setup ####
library(dplyr)
library(lubridate)
library(IEAnalyzeR)
library(here)
library(ggplot2)
library(rerddap)
library(sf)
library(terra)

# File Naming Setup.
root_name <- "sst"

csv_filename <- here(paste0("data/formatted/formatted_csvs/", root_name, "_formatted.csv"))
object_filename <- here(paste0("data/formatted/final_objects/", root_name, "_object.rds"))
plot_filename <- here(paste0("figures/plots/", root_name, "_plot.png"))

#----------------------------------------------------
#### 1. Read Data ####

# define years  --------------------------------
styear <- 1982
enyear <- 2025

# define spatial domain  --------------------------------
min_lon <- -98
max_lon <- -80
min_lat <- 18
max_lat <- 31

# load shapefile to subset  --------------------------------
### shapefiles downloaded from marineregions.org (future goal implement mregions2 R package for shapefile)
setwd(here(paste0("data/intermediate/gulf_eez")))
eez <- vect('eez.shp') |> makeValid()

setwd(here(paste0("data/intermediate/gulf_iho")))
iho <- vect('iho.shp') |> makeValid()

gulf_eez <- terra::intersect(eez, iho) |> st_as_sf() |> st_transform(crs = st_crs(4326))
gulf_iho <- iho |> st_as_sf() |> st_transform(crs = st_crs(4326))

rm(eez, iho); gc()

# get ERDDAP info  --------------------------------
sst <- info('ncdcOisst21Agg_LonPM180') # this may work better

# empty data  -------------------------------------------------
dat_gulf <- c()
dat_eez <- c()

# download by year to avoid timeout errors --------------------

######################################################
#### don't run while reviewing code; takes awhile ####
#### load saved intermediate files below loop ########
######################################################

setwd(here(paste0("data/intermediate")))
system.time(
  for (yr in styear:enyear) { 
    
    cat('\n', yr, '\n')
    
    sst_grab <- griddap(sst, fields = c('anom','sst'), 
                        time = c(paste0(yr,'-01-01'), paste0(yr,'-12-31')),
                        longitude = c(min_lon, max_lon), 
                        latitude = c(min_lat, max_lat), 
                        fmt = 'csv')
    
    ### whole Gulf / IHO
    sst_iho_sf <- st_as_sf(sst_grab, coords = c("longitude", "latitude"), crs = 4326) |>
      st_intersection(gulf_iho)
    
    sst_gulf <- sst_iho_sf |>
      st_drop_geometry() |> # dplyr is slow
      group_by(time) |>
      summarize(sst_degC = mean(sst, na.rm = T),
                anom_degC = mean(anom, na.rm = T))
    
    ### US EEZ
    sst_eez_sf <- st_as_sf(sst_grab, coords = c("longitude", "latitude"), crs = 4326) |>
      st_intersection(gulf_eez)
    
    sst_eez <- sst_eez_sf |>
      st_drop_geometry() |> # dplyr is slow
      group_by(time) |>
      summarize(sst_degC = mean(sst, na.rm = T),
                anom_degC = mean(anom, na.rm = T))
    
    if (yr == styear) { 
      dat_gulf <- sst_gulf
      dat_eez <- sst_eez
    } 
    else {
      dat_gulf <- rbind(dat_gulf, sst_gulf)
      dat_eez <- rbind(dat_eez, sst_eez)
    }
  }
)

setwd(here(paste0("data/intermediate")))
# save(dat_eez, dat_gulf, file = 'sst_comb_temp2.RData')
load('sst_comb_temp2.RData')

### convert to dates
dat_gulf$time <- as.Date(dat_gulf$time)
dat_eez$time <- as.Date(dat_eez$time)

#----------------------------------------------------
#### 2. Clean data and create time series csv ####

#Transform the data to fit the IEA data format.
#For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
#Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.

#Define header components for the data rows (ignore year). Fill in the blanks here.
indicator_names = c("")
unit_names = c("")
extent_names = c("")

formatted_data = IEAnalyzeR::convert_cleaned_data(your_data, indicator_names, unit_names, extent_names)


#----------------------------------------------------
#### 3. Save Formatted data as csv ####
 
# This will save your data to the appropriate folder.

write.csv(formatted_data, file = csv_filename, row.names = F)

#----------------------------------------------------
#### 4. Create Data_Prep object ####
  
#Please use your formatted csv to create a "data_prep" object.
#For more info on the data_prep function see the vignette linked above.

data_obj<-IEAnalyzeR::data_prep(csv_filename)


#----------------------------------------------------
#### 5. Save Formatted data_prep object ####

#This will save your data to the appropriate folder.
  
saveRDS(data_obj, file = object_filename)


#----------------------------------------------------
#### 6. Preview Plot ####
# Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# For more info on the plot_fn_obj function go HERE

IEAnalyzeR::plot_fn_obj(df_obj = data_obj, trends = TRUE)

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

ggsave(filename = plot_filename)
