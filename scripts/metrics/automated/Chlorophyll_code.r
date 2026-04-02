
# File created on 2026-02-19, adapted from file created 2026-02-06 by Carissa Gervasi

#### 0. Setup ####
# Add any packages that are needed for analysis here.
library(abind)
library(dplyr)
library(lubridate)
library(IEAnalyzeR)
library(here)
library(ggplot2)
library(rerddap)
library(sf)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(ncdf4)
library(cmocean)
library(fields)

# File Naming Setup.
root_name <- "Chlorophyll"

csv_filename <- here(paste0("data/formatted/formatted_csvs/", root_name, "_formatted.csv"))
object_filename <- here(paste0("data/formatted/final_objects/", root_name, "_object.rds"))
plot_filename <- here(paste0("figures/plots/", root_name, "_plot.png"))

#----------------------------------------------------
#### 1. Read Data ####

### The ESA CCI V6 (https://www.oceancolour.org/) was chosen over MODIS for several reasons:
# 1. AQUA/MODIS is nearing the end of its fuel and it is expected to deorbit soon (see status https://aqua.nasa.gov/announcements/aqua-status)
# 2. The ESA CCI blended chlorophyll compares well to AQUA/MODIS data; see separate code for comparisons; correlation is 0.85 between standardized monthly anomalies
# 3. One of the goals of this ESR (Gulf 2025) is to set the framework for annual updates; thus using a stable data source is preferable


# ERDDAP file: pmlEsaCCI60OceanColorMonthly
# https://coastwatch.pfeg.noaa.gov/erddap/info/pmlEsaCCI60OceanColorMonthly/index.html


# define spatial domain  --------------------------------
min_lon <- -98
max_lon <- -80
min_lat <- 18
max_lat <- 31

# load shapefile to subset  --------------------------------
### shapefiles downloaded from marineregions.org (future goal implement mregions2 R package for shapefile)
setwd(here("data/intermediate/gulf_eez"))
setwd("C:/Users/brendan.turley/Documents/data/shapefiles/gulf_eez") ### remove when final pull request issued
eez <- vect('eez.shp') |> makeValid()

setwd(here("data/intermediate/gulf_iho"))
setwd("C:/Users/brendan.turley/Documents/data/shapefiles/gulf_iho") ### remove when final pull request issued
iho <- vect('iho.shp') |> makeValid()

gulf_eez <- terra::intersect(eez, iho) |>
  st_as_sf() |> 
  st_transform(crs = st_crs(4326))
gulf_iho <- iho |> 
  st_as_sf() |> 
  st_transform(crs = st_crs(4326))

rm(eez, iho); gc()


# download by year to avoid timeout errors --------------------

######################################################
#### don't run while reviewing code; takes awhile ####
#### load saved intermediate files below loop ########
######################################################

review_code <- T ### set to F to rerun download loop

if(review_code == F){
  
  # get ERDDAP info  --------------------------------
  # pmlEsaCCI50OceanColorMonthly
  chl <- info('pmlEsaCCI60OceanColorMonthly', url = 'https://coastwatch.pfeg.noaa.gov/erddap')
  
  
  # define years  --------------------------------
  styear <- 1997 # 1997 is partial year
  enyear <- 2025 # only goes to Dec 2021
  
  # empty data  -------------------------------------------------
  cci_erddap_eez <- c()
  
  ### only retain the geometry
  gulf_eez <- gulf_eez$geometry
  
  setwd(here("data/intermediate"))
  system.time(
    for (yr in styear:enyear) {
      
      cat('\n', yr, '\n')
      
      if(yr == 1997){
        begin_date <- '1997-09-04T00:00:00Z'
      } else {
        begin_date <- paste0(yr,'-01-01T00:00:00Z')
      }
      
      chl_grab <- griddap(chl, fields = 'chlor_a',
                          time = c(begin_date, paste0(yr,'-12-01T00:00:00Z')),
                          longitude = c(min_lon, max_lon),
                          latitude = c(min_lat, max_lat),
                          fmt = 'csv')
      
      ### log for geometric mean
      chl_grab$log10_chlor <- log10(chl_grab$chlor_a + .0001)
      
      ### US EEZ
      chl_eez_sf <- st_as_sf(chl_grab, coords = c("longitude", "latitude"), crs = 4326) |>
        st_intersection(gulf_eez)
      
      
      chl_eez <- chl_eez_sf |>
        st_drop_geometry() |>
        group_by(time) |>
        summarize(chl_mgm3 = mean(chlor_a, na.rm = T),
                  chl_mgm3_geo = 10^mean(log10_chlor, na.rm = T))
      
      if (yr == styear) {
        cci_erddap_eez <- chl_eez
      }
      else {
        cci_erddap_eez <- rbind(cci_erddap_eez, chl_eez)
      }
    }
  )
  
  setwd(here("data/intermediate"))
  save(cci_erddap_eez, file = 'chl_cci_temp.RData')
  
} else {
  
  setwd(here("data/intermediate"))
  load('chl_cci_temp.RData')
  
}

dat_eez2 <- c(rep(NA,8), cci_erddap_eez$chl_mgm3_geo)
dat_sq <- matrix(dat_eez2, 12, 29) 
dat_anom_cci <- ((dat_sq - apply(dat_sq, 1, mean, na.rm = T))/apply(dat_sq, 1, sd, na.rm = T)) |> as.vector()
time1 <- c(seq(as.Date('1997-01-01'), as.Date('1997-08-01'), by = 'month'),
           as.Date(cci_erddap_eez$time))

### sanity check
plot(time1, dat_anom_cci, typ = 'l', lwd = 2, col = 3)
abline(h = c(-1,0,1), lty = 5)
abline(v = seq(ymd('1997-01-01'),ymd('2026-01-01'),by = 'year'), lty = 3, col = 'gray')


#----------------------------------------------------
#### 2. Clean data and create time series csv ####

#Transform the data to fit the IEA data format.
#For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
#Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.

yrmon <- paste(time1 |> year(),
               sprintf("%02.f", time1 |> month()),
               sep = '-')


#Define header components for the data rows (ignore year). Fill in the blanks here.
indicator_names = c('Chlorophyll a')
unit_names = c('Standardized Monthly Anomaly')
extent_names = c("US Gulf EEZ")

formatted_data = IEAnalyzeR::convert_cleaned_data(cbind(yrmon[9:length(yrmon)], 
                                                        dat_anom_cci[9:length(dat_anom_cci)]), # NAs added for anomaly calc; then remmoved here
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

IEAnalyzeR::plot_fn_obj(df_obj = data_obj, trend = TRUE, lwd = .5)

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

ggsave(filename = plot_filename)


#----------------------------------------------------
#### 8. making Seasonal plots (sensu NEFSC SOE) ####


### helper functions

### perform spatial trend analyses on a matirx or an array
array_lm <- function (x){
  if(is.na(all(x))){
    NA
  } else {
    y <- 1:length(x)
    coef(lm(x~y))[2]
  }
}

### replace min/max for spatial plots; superfluous using terra
replace_min_max <- function(dat, min, max){
  dat[which(dat < min)] <- min
  dat[which(dat > max)] <- max
  dat
}

### load from previously saved spot
setwd(here("data/intermediate"))
load('chl_cci_temp.RData')


### convert to dates
cci_erddap_eez$time <- as.Date(cci_erddap_eez$time)

# add yearmonth column --------------------------
cci_erddap_eez$yrmon <- paste(cci_erddap_eez$time |> year(),
                              sprintf("%02.f", cci_erddap_eez$time |> month()),
                              sep = '-')

### add seasons
cci_erddap_eez$jday <- yday(cci_erddap_eez$time)

cci_erddap_eez <- cci_erddap_eez |>
  mutate(season = case_when(
    month(time)==12 | month(time)<3 ~ 'win',
    month(time)>2 & month(time)<6 ~ 'spr',
    month(time)>5 & month(time)<9 ~ 'sum',
    month(time)>8 & month(time)<12 ~ 'aut'
  )) |>
  arrange(time)

### create season_yr and adjust to make december n-1 part of winter n
cci_erddap_eez$season_yr <- ifelse(month(cci_erddap_eez$time)==12, 
                                   year(cci_erddap_eez$time)+1, 
                                   year(cci_erddap_eez$time))
cci_erddap_eez$season_yr[which(cci_erddap_eez$season_yr==2026)] <- NA

### alternative to redefine seasons as jfm, amj, jas, ond
# dat_eez <- dat_eez |>
#   mutate(season = case_when(
#     month(time)<4 ~ 'win',
#     month(time)>3 & month(time)<7 ~ 'spr',
#     month(time)>4 & month(time)<10 ~ 'sum',
#     month(time)>9 ~ 'aut'
#   )) |>
#   arrange(time)
# dat_eez$season_yr <- year(dat_eez$time)

### seasonal means
eez_win <- aggregate(chl_mgm3_geo ~ season_yr, data = subset(cci_erddap_eez, season=='win'),
                     # mean, na.rm = T)
                     function(x) 10^mean(log10(x + .0001),na.rm=T))
eez_spr <- aggregate(chl_mgm3_geo ~ season_yr, data = subset(cci_erddap_eez, season=='spr'),
                     # mean, na.rm = T)
                     function(x) 10^mean(log10(x + .0001),na.rm=T))
eez_sum <- aggregate(chl_mgm3_geo ~ season_yr, data = subset(cci_erddap_eez, season=='sum'),
                     # mean, na.rm = T)
                     function(x) 10^mean(log10(x + .0001),na.rm=T))
eez_aut <- aggregate(chl_mgm3_geo ~ season_yr, data = subset(cci_erddap_eez, season=='aut'),
                     # mean, na.rm = T)
                     function(x) 10^mean(log10(x + .0001),na.rm=T))

png(here('figures/plots/chl-seasonal-plot.png'), width = 9, height = 6, units = 'in', res = 300)
par(mfrow = c(2,2), mar = c(3,5,2,3),
    oma = c(0,0,3,0))

plot(eez_win$season_yr, eez_win$chl_mgm3_geo, 
     typ = 'o', pch = 16, las = 1,
     panel.first = list(abline(lm(chl_mgm3_geo ~ season_yr, data = eez_win), lwd = 4, col = 'orange'),
                        abline(h = mean(eez_win$chl_mgm3_geo), col = 'gray', lwd = 2),
                        grid()),
     xlab = '', ylab = expression(paste('Chlorophyll ', alpha, ' (mg m',-3,')')), main = 'Winter - DJF')

plot(eez_spr$season_yr, eez_spr$chl_mgm3_geo, 
     typ = 'o', pch = 16, las = 1,
     panel.first = list(abline(lm(chl_mgm3_geo ~ season_yr, data = eez_spr), lwd = 4, col = 'orange'),
                        abline(h = mean(eez_spr$chl_mgm3_geo), col = 'gray', lwd = 2),
                        grid()),
     xlab = '', ylab = expression(paste('Chlorophyll ', alpha, ' (mg m',-3,')')), main = 'Spring - MAM')

plot(eez_sum$season_yr, eez_sum$chl_mgm3_geo, 
     typ = 'o', pch = 16, las = 1,
     panel.first = list(abline(lm(chl_mgm3_geo ~ season_yr, data = eez_sum), lwd = 4, col = 'orange'),
                        abline(h = mean(eez_sum$chl_mgm3_geo), col = 'gray', lwd = 2),
                        grid()),
     xlab = '', ylab = expression(paste('Chlorophyll ', alpha, ' (mg m',-3,')')), main = 'Summer - JJA')

plot(eez_aut$season_yr, eez_aut$chl_mgm3_geo,
     typ = 'o', pch = 16, las = 1,
     panel.first = list(abline(lm(chl_mgm3_geo ~ season_yr, data = eez_aut), lwd = 4, col = 'orange'),
                        abline(h = mean(eez_aut$chl_mgm3_geo), col = 'gray', lwd = 2),
                        grid()),
     xlab = '', ylab = expression(paste('Chlorophyll ', alpha, ' (mg m',-3,')')), main = 'Fall - SON')

mtext(expression(paste('US Gulf EEZ Chorophyll ',alpha , ' Seasonality')), side = 3, outer = TRUE, cex = 5/4, font = 2, line = 5/4)
dev.off()




### spatial plots


### alternative download from Plymouth Marine Lab (from the horse's mouth); https://www.oceancolour.org/
### this using the OPeNDAP entry point

# define spatial domain  --------------------------------
min_lon <- -98
max_lon <- -80
min_lat <- 18
max_lat <- 31

review_code <- T ### set to F to rerun download loop

if(review_code == F){
  
  dat <- nc_open('https://www.oceancolour.org/thredds/dodsC/cci/v6.0-release/geographic/monthly/chlor_a/1997/ESACCI-OC-L3S-CHLOR_A-MERGED-1M_MONTHLY_4km_GEO_PML_OCx-199709-fv6.0.nc',
                 readunlim = F, return_on_error = T, suppress_dimvals = T)
  lon <- ncvar_get(dat, 'lon')
  lat <- ncvar_get(dat, 'lat')
  chl_fill <- ncatt_get(dat, 'chlor_a', "_FillValue")
  nc_close(dat)
  rm(dat)
  
  ### get lon/lat indices for download
  ilon <- which(lon>min_lon & lon<max_lon)
  ilat <- which(lat>min_lat & lat<max_lat)
  
  ### create year-month datatframe to paste into OPeNDAP request
  yr_mon <- expand.grid(year = 2021:2025, 
                        month = sprintf('%02d', 1:12)) |>
    arrange(year, month)
  
  ### chunk downloads for sanity
  dat_eez <- array(NA, c(length(ilon), length(ilat), dim(yr_mon)[1]))
  time_dat <- rep(NA, dim(yr_mon)[1])
  
  pb <- txtProgressBar(min = 0, max = length(time_dat), style = 3)
  for(i in 1:dim(yr_mon)[1]){
    
    nc_url <- paste0('https://www.oceancolour.org/thredds/dodsC/cci/v6.0-release/geographic/monthly/chlor_a/',
                     yr_mon$year[i],
                     '/ESACCI-OC-L3S-CHLOR_A-MERGED-1M_MONTHLY_4km_GEO_PML_OCx-',
                     yr_mon$year[i], yr_mon$month[i],
                     '-fv6.0.nc')
    
    dat <- nc_open(nc_url, readunlim = F, return_on_error = T, suppress_dimvals = T)
    
    while(dat$error==T){ ### sometimes the server times out; this seems to work
      Sys.sleep(5) 
      dat <- nc_open(nc_url, readunlim = F, return_on_error = T, suppress_dimvals = T)
    }
    
    chl_pull <- try(ncvar_get(dat, 'chlor_a', start = c(ilon[1], ilat[1], 1),
                              count = c(length(ilon), length(ilat), 1)))
    time_pull <- try(ncvar_get(dat, 'time'))
    
    while(any(class(chl_pull) == 'try-error')){ ### sometimes the server times out; this seems to work
      Sys.sleep(5) 
      chl_pull <- try(ncvar_get(dat, 'chlor_a', start = c(ilon[1], ilat[1], 1),
                                count = c(length(ilon), length(ilat), 1)))
      time_pull <- try(ncvar_get(dat, 'time'))
    }
    
    dat_eez[,,i] <- chl_pull
    time_dat[i] <- time_pull
    
    setwd(here("data/intermediate"))
    save(dat_eez, time_dat, file = 'chl_spatial.RData')
    
    nc_close(dat)
    rm(chl_pull, time_pull, dat)
    setTxtProgressBar(pb, i)
  }
  
  
} else {
  
  setwd(here("data/intermediate"))
  load('chl_spatial.RData')
  
}

# setwd(here("data/intermediate"))
# load('chl_comb_temp2.RData')
# 
# time_dat <- as.Date(time_dat, origin = '1970-01-01')
# ind <- which(year(time_dat)>2020)
# 
# time_dat <- time_dat[ind]
# dat_eez <- dat_eez[,,ind]
# 
# setwd(here("data/intermediate"))
# save(dat_eez, time_dat, file = 'chl_spatial.RData')


chl_l <- list(chl = dat_eez,
              time = time_dat)
chl_l$yrmon <- paste0(year(chl_l$time),
                      sprintf("%02d",month(chl_l$time)))
### mean monthly anomalies for spatial trend to match aggregation from timeseries plots
chl_a <- array(NA, dim = c(432,312,60))
for(i in unique(chl_l$yrmon)){
  chl_a[,,which(i==unique(chl_l$yrmon))] <- chl_l$chl[,,which(chl_l$yrmon==i)] |>
    apply(c(1,2), function(x) 10^mean(log10(x + .0001), na.rm=T))
  # apply(c(1,2), function(x) mean(log10(x + .0001), na.rm=T))
}

yr5_trend <- apply(chl_a, c(1,2), array_lm)
hist(yr5_trend)
range(yr5_trend, na.rm = T)
quantile(yr5_trend, na.rm = T)
imagePlot(yr5_trend)

ann_25 <- apply(chl_l$chl[,,which(year(chl_l$time)>2024)],
                c(1,2), function(x) 10^mean(log10(x + .0001), na.rm=T))
hist(log10(ann_25))
range(log10(ann_25), na.rm = T)
quantile(log10(ann_25), na.rm = T)
imagePlot(log10(ann_25))


### colors and breaks for plotting
t_brks <- seq(-.01,.01,.0005)
t_cols <- cmocean('delta')(length(t_brks)-1)
a_brks <- seq(-1.2,1.2,.05)
a_cols <- cmocean('speed')(length(a_brks)-1)


### shapefile for plotting
world <- ne_download(scale = 10, type = "countries", 
                     returnclass = 'sv') |>
  crop(ext(min_lon,max_lon,min_lat,max_lat))

### put into raster
fyt_rast <- rast(t(yr5_trend[,ncol(yr5_trend):1]))
fyt_rast <- rast(t(yr5_trend))
ext(fyt_rast) <- c(min_lon,max_lon,min_lat,max_lat)
crs(fyt_rast) <- "EPSG:4326"

ann_25_rast <- rast(t(log10(ann_25)))
ext(ann_25_rast) <- c(min_lon,max_lon,min_lat,max_lat)
crs(ann_25_rast) <- "EPSG:4326"


png(here('figures/plots/chl-spatial-plot.png'), width = 4, height = 6, units = 'in', res = 300)
par(mfrow=c(2,1))
plot(fyt_rast,
     col = t_cols, range = c(-.01,.01),
     # col = t_cols, range = c(-.36,.36),
     plg = list(tick = 'out', format='g'),
     main = expression(paste('2021-2025 Chl-',alpha,' Trend (mg ',m^-3,' ',month^-1,')')),
     cex.main = 1)
plot(world, add= T, col = 'gray')
plot(gulf_eez['geometry'], add = T)

plot(ann_25_rast, 
     col = a_cols, range = c(-1.2,1.2),
     plg = list(tick = 'out', format='g',
                at = c(-1,0,1),
                labels = c(0.1, 1, 10)),
     main = expression(paste('2025 Mean Chl-',alpha,' log(mg ',m^-3,')')),
     cex.main = 1)
plot(world, add= T, col = 'gray')
plot(gulf_eez['geometry'], add = T)
dev.off()
