# File created on 2026-05-08 by B. Turley

#### 0. Setup ####
# Add any packages that are needed for analysis here.
library(IEAnalyzeR)
library(here)
library(ggplot2)
library(abind)
library(dplyr)
library(lubridate)
library(ncdf4)
library(terra)
library(sf)
library(reticulate)
library(heatwave3)

# File Naming Setup.
# !! Auto generated-Do Not Change !!
root_name <- "bottom-temperature"

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


### area of interest
min_lon <- -98
max_lon <- -80
min_lat <- 18
max_lat <- 31

# load shapefile to subset  --------------------------------
### shapefiles downloaded from marineregions.org (future goal implement mregions2 R package for shapefile)
setwd("~/data/shapefiles/gulf_eez")
eez <- vect('eez.shp') |> makeValid()

setwd("~/data/shapefiles/gulf_iho")
iho <- vect('iho.shp') |> makeValid()

gulf_eez <- terra::intersect(eez, iho)


######################################################
#### don't run while reviewing code; takes awhile ####
#### load saved intermediate files below loop ########
######################################################

review_code <- T ### set to F to rerun download loop

if(review_code == F){
  
# virtualenv_create(envname = "CopernicusMarineR", packages = c("copernicusmarine"))
# Activate the virtual environment (must be done before importing any Python module)
use_virtualenv("CopernicusMarineR", required = TRUE)

# Optional sanity check: confirm which Python reticulate is using
py_config()

# Import the Python module
copernicusmarine <- import("copernicusmarine")

# The adapted command
result <- copernicusmarine$subset(
  dataset_id = "cmems_mod_glo_phy_my_0.083deg_P1M-m",
  dataset_version="202311",
  variables = list("bottomT"),  # Use list() so reticulate passes a proper Python list
  minimum_longitude = min_lon,
  maximum_longitude = max_lon,
  minimum_latitude = min_lat,
  maximum_latitude = max_lat,
  start_datetime = "1993-01-01T00:00:00",
  end_datetime   = "2025-12-01T00:00:00",
  output_directory = "C:/Users/brendan.turley/Documents/data/copernicusmarine/sbt"
)

# The adapted command
result2 <- copernicusmarine$subset(
  dataset_id = "cmems_mod_glo_phy_anfc_0.083deg_static",
  dataset_version="202211",
  variables = list("deptho"),  # Use list() so reticulate passes a proper Python list
  minimum_longitude = min_lon,
  maximum_longitude = max_lon,
  minimum_latitude = min_lat,
  maximum_latitude = max_lat,
  output_directory = "C:/Users/brendan.turley/Documents/data/copernicusmarine"
)

setwd('C:/Users/brendan.turley/Documents/data/copernicusmarine')
deptho2 <- rast('cmems_mod_glo_phy_anfc_0.083deg_static_deptho_98.00W-80.00W_18.00N-31.00N.nc')
msk <- ifel(deptho2 > 100, NA, 1) ### create a mask for only the continential shelf

setwd('C:/Users/brendan.turley/Documents/data/copernicusmarine/sbt')
sbt <- rast('cmems_mod_glo_phy_my_0.083deg_P1M-m_bottomT_98.00W-80.00W_18.00N-31.00N_1993-01-01-2025-12-01.nc') |>
  mask(msk)
# plot(sbt)

### US EEZ
sbt_eez <- crop(sbt, gulf_eez) |> mask(gulf_eez)
sbt_eez_ts <- global(sbt_eez, fun = c('mean',"range"), na.rm = TRUE, weighted = T)
sbt_eez_ts <- data.frame(
  time = time(sbt_eez), # Convert index back to Date
  yr_mon = seq(1993,2025.999,1/12),
  sbt = sbt_eez_ts$mean,
  min = sbt_eez_ts$min,
  max = sbt_eez_ts$max
)

setwd(here('data/intermediate'))
save(sbt_eez, sbt_eez_ts, file = 'sbt_tmp.RData')

} else {
  
  setwd(here('data/intermediate'))
  load('sbt_tmp.RData')
  
}

sbt_eez_ts$anom <- (matrix(sbt_eez_ts$sbt, 12, 33) - apply(matrix(sbt_eez_ts$sbt, 12, 33),1,mean)) |> 
   as.vector()

sbt_eez_ts$yrmon <- paste(sbt_eez_ts$time |> year(),
                       sprintf("%02.f", sbt_eez_ts$time |> month()),
                       sep = '-')


plot(sbt_eez_ts$time, sbt_eez_ts$sbt, typ = 'o', pch = 16,
     panel.first = grid())

plot(sbt_eez_ts$yr_mon, sbt_eez_ts$anom, typ = 'o', pch = 16,
  panel.first = list(grid(), abline(h=0,lty=5)))

plot(sbt_eez_ts$yr_mon, sbt_eez_ts$anom, typ = 'h')


sbt_ann <- aggregate(cbind(sbt, anom) ~ year(time), data = sbt_eez_ts, mean, na.rm = T) |>
  setNames(c('year','sbt','anom'))

plot(sbt_ann$year, sbt_ann$sbt, typ = 'o', pch = 16)
plot(sbt_ann$year, sbt_ann$anom, typ = 'o', pch = 16)

#----------------------------------------------------
#### 2. Clean data and create time series csv ####

#Transform the data to fit the IEA data format.
#For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
#Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.

#Define header components for the data rows (ignore year). Fill in the blanks here.
indicator_names = "Sea Bottom Temperature"
unit_names = "Anomaly (°C)"
extent_names = 'US Gulf EEZ'

formatted_data = IEAnalyzeR::convert_cleaned_data(cbind(sbt_eez_ts$yrmon,sbt_eez_ts$anom), 
                                                  indicator_names, unit_names, extent_names)


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

IEAnalyzeR::plot_fn_obj(df_obj = data_obj, trend = TRUE)

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

ggsave(filename = plot_filename, height = 4, width = 7, unit="in")




#----------------------------------------------------
#### 8. making Seasonal plots (sensu NEFSC SOE) ####


### helper functions

### this adds a fahrenheit axis on the right of the plot by converting the celcius default
ax_convert_c2f <- function(vals, side = 4, n = 5, las = 1, ...){ ### ... used to pass other parameters for interior fxns
  tick_val <- pretty(vals*(9/5)+32, n = n, ...)
  axis(side, (tick_val-32)*(5/9), tick_val, las = las, ...)
}


### add seasons
sbt_eez_ts$jday <- yday(sbt_eez_ts$time)

sbt_eez_ts <- sbt_eez_ts |>
  mutate(season = case_when(
    month(time)==12 | month(time)<3 ~ 'win',
    month(time)>2 & month(time)<6 ~ 'spr',
    month(time)>5 & month(time)<9 ~ 'sum',
    month(time)>8 & month(time)<12 ~ 'aut'
  )) |>
  arrange(time)

### create season_yr and adjust to make december n-1 part of winter n
sbt_eez_ts$season_yr <- ifelse(month(sbt_eez_ts$time)==12, 
                               year(sbt_eez_ts$time)+1, 
                               year(sbt_eez_ts$time))
sbt_eez_ts$season_yr[which(sbt_eez_ts$season_yr==2026)] <- NA

### seasonal means
eez_win <- aggregate(sbt ~ season_yr, data = subset(sbt_eez_ts, season=='win'),
                     mean, na.rm = T)
eez_spr <- aggregate(sbt ~ season_yr, data = subset(sbt_eez_ts, season=='spr'),
                     mean, na.rm = T)
eez_sum <- aggregate(sbt ~ season_yr, data = subset(sbt_eez_ts, season=='sum'),
                     mean, na.rm = T)
eez_aut <- aggregate(sbt ~ season_yr, data = subset(sbt_eez_ts, season=='aut'),
                     mean, na.rm = T)

# png(here('figures/plots/sst-seasonal-plot.png'), wi### add seasons
sbt_eez_ts$jday <- yday(sbt_eez_ts$time)

sbt_eez_ts <- sbt_eez_ts |>
  mutate(season = case_when(
    month(time)==12 | month(time)<3 ~ 'win',
    month(time)>2 & month(time)<6 ~ 'spr',
    month(time)>5 & month(time)<9 ~ 'sum',
    month(time)>8 & month(time)<12 ~ 'aut'
  )) |>
  arrange(time)

### create season_yr and adjust to make december n-1 part of winter n
sbt_eez_ts$season_yr <- ifelse(month(sbt_eez_ts$time)==12, 
                            year(sbt_eez_ts$time)+1, 
                            year(sbt_eez_ts$time))
sbt_eez_ts$season_yr[which(sbt_eez_ts$season_yr==2026)] <- NA

### seasonal means
eez_win <- aggregate(sbt ~ season_yr, data = subset(sbt_eez_ts, season=='win'),
                     mean, na.rm = T)
eez_spr <- aggregate(sbt ~ season_yr, data = subset(sbt_eez_ts, season=='spr'),
                     mean, na.rm = T)
eez_sum <- aggregate(sbt ~ season_yr, data = subset(sbt_eez_ts, season=='sum'),
                     mean, na.rm = T)
eez_aut <- aggregate(sbt ~ season_yr, data = subset(sbt_eez_ts, season=='aut'),
                     mean, na.rm = T)


png(here('figures/plots/bottom-temperature-seasonal-plot.png'), width = 9, height = 6, units = 'in', res = 300)
par(mfrow = c(2,2), mar = c(3,5,2,3),
    oma = c(0,0,3,0))

plot(eez_win$season_yr, eez_win$sbt, 
     typ = 'o', pch = 16, las = 1,
     panel.first = list(abline(lm(sbt ~ season_yr, data = eez_win), lwd = 4, col = 'orange'),
                        abline(h = mean(eez_win$sbt), col = 'gray', lwd = 2),
                        grid()),
     xlab = '', ylab = 'SBT', main = 'Winter - DJF')
mtext('(°C)', side = 3, adj = -.1, line = .5)
mtext('(°F)', side = 3, adj = 1.1, line = .5)
ax_convert_c2f(eez_win$sbt, n = 4)

plot(eez_spr$season_yr, eez_spr$sbt, 
     typ = 'o', pch = 16, las = 1,
     panel.first = list(abline(lm(sbt ~ season_yr, data = eez_spr), lwd = 4, col = 'orange'),
                        abline(h = mean(eez_spr$sbt), col = 'gray', lwd = 2),
                        grid()),
     xlab = '', ylab = 'SBT', main = 'Spring - MAM')
mtext('(°C)', side = 3, adj = -.1, line = .5)
mtext('(°F)', side = 3, adj = 1.1, line = .5)
ax_convert_c2f(eez_spr$sbt, n = 4)

plot(eez_sum$season_yr, eez_sum$sbt, 
     typ = 'o', pch = 16, las = 1,
     panel.first = list(abline(lm(sbt ~ season_yr, data = eez_sum), lwd = 4, col = 'orange'),
                        abline(h = mean(eez_sum$sbt), col = 'gray', lwd = 2),
                        grid()),
     xlab = '', ylab = 'SBT', main = 'Summer - JJA')
mtext('(°C)', side = 3, adj = -.1, line = .5)
mtext('(°F)', side = 3, adj = 1.1, line = .5)
ax_convert_c2f(eez_sum$sbt, n = 4)

plot(eez_aut$season_yr, eez_aut$sbt,
     typ = 'o', pch = 16, las = 1,
     panel.first = list(abline(lm(sbt ~ season_yr, data = eez_aut), lwd = 4, col = 'orange'),
                        abline(h = mean(eez_aut$sbt), col = 'gray', lwd = 2),
                        grid()),
     xlab = '', ylab = 'SBT', main = 'Fall - SON')
mtext('(°C)', side = 3, adj = -.1, line = .5)
mtext('(°F)', side = 3, adj = 1.1, line = .5)
ax_convert_c2f(eez_aut$sbt, n = 4)

mtext('US Gulf EEZ Bottom Temperatures', side = 3, outer = TRUE, cex = 5/4, font = 2, line = 5/4)
dev.off()

