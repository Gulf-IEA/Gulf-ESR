
### this code compares the ESA CCI and Aqua MODIS chlorophyll products

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

# define spatial domain  --------------------------------
min_lon <- -98
max_lon <- -80
min_lat <- 18
max_lat <- 31

# load shapefile to subset  --------------------------------
### run if need to download data

review_code <- T ### set to F to rerun download loop

if(review_code == F){
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
}


# ESA CCI from ERDDAP --------------------

######################################################
#### don't run while reviewing code; takes awhile ####
#### load saved intermediate files below loop ########
######################################################

review_code #<- T ### set to F to rerun download loop; previously set

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


# AQUA/MODIS from ERDDAP --------------------

######################################################
#### don't run while reviewing code; takes awhile ####
#### load saved intermediate files below loop ########
######################################################

review_code #<- T ### set to F to rerun download loop; previously set

if(review_code == F){
  
  ### alternatively there is AQUA/MODIS
  # erdMH1chlamday_R2022NRT or erdMH1chlamday_R2022SQ
  chl <- info('erdMH1chlamday_R2022SQ', url = 'https://coastwatch.pfeg.noaa.gov/erddap')
  
  # define years  --------------------------------
  styear <- 2003
  enyear <- 2025
  
  # empty data  -------------------------------------------------
  modis_erddap_eez <- c()
  
  ### only retain the geometry
  gulf_eez <- gulf_eez$geometry
  st_crs(gulf_eez)
  
  setwd(here("data/intermediate"))
  system.time(
    for (yr in styear:enyear) { 
      
      cat('\n', yr, '\n')
      
      if(yr<2024){
        variable <- 'chlor_a'
      }
      if(yr>=2024){
        chl <- info('erdMH1chlamday_R2022NRT', url = 'https://coastwatch.pfeg.noaa.gov/erddap')
        variable <- 'chlorophyll'
      }
      
      chl_grab <- griddap(chl, fields = variable, 
                          time = c(paste0(yr,'-01-16T00:00:00Z'), paste0(yr,'-12-16T00:00:00Z')),
                          longitude = c(min_lon, max_lon), 
                          latitude = c(min_lat, max_lat), 
                          fmt = 'csv')
      
      if(names(chl_grab)[4]=='chlor_a'){
        names(chl_grab)[4] <- 'chlorophyll'
      }
      
      ### log for geometric mean
      chl_grab$log10_chlor <- log10(chl_grab$chlorophyll + .0001)
      
      ### US EEZ
      chl_eez_sf <- st_as_sf(chl_grab, coords = c("longitude", "latitude"), crs = 4326) |>
        st_intersection(gulf_eez)
      
      
      chl_eez <- chl_eez_sf |>
        st_drop_geometry() |>
        group_by(time) |>
        summarize(chl_mgm3 = mean(chlorophyll, na.rm = T),
                  chl_mgm3_geo = 10^mean(log10_chlor, na.rm = T))
      
      if (yr == styear) { 
        modis_erddap_eez <- chl_eez
      } 
      else {
        modis_erddap_eez <- rbind(modis_erddap_eez, chl_eez)
      }
    }
  )
  
  setwd(here("data/intermediate"))
  save(modis_erddap_eez, file = 'chl_modis_temp.RData')
  
} else {
  
  setwd(here("data/intermediate"))
  load('chl_modis_temp.RData')
  
}

dat_eez1 <- cci_erddap_eez$chl_mgm3_geo
dat_sq1 <- matrix(dat_eez1[5:length(dat_eez1)], 12, 28) 
dat_anom_cci <- ((dat_sq1 - apply(dat_sq1, 1, mean, na.rm = T))/apply(dat_sq1, 1, sd, na.rm = T)) |> as.vector()
time1 <- as.Date(cci_erddap_eez$time)[5:length(dat_eez1)]

dat_eez2 <- modis_erddap_eez$chl_mgm3_geo
dat_sq2 <- matrix(dat_eez2, 12, 23) 
dat_anom_modis <- ((dat_sq2 - apply(dat_sq2, 1, mean, na.rm = T))/apply(dat_sq2, 1, sd, na.rm = T)) |> as.vector()
time2 <- as.Date(modis_erddap_eez$time)

plot(time1, dat_anom_cci, typ = 'l', lwd = 2, col = 3)
points(time2, dat_anom_modis, typ = 'l', lwd = 2, col = 4)
abline(h = c(-1,0,1), lty = 5)
abline(v = seq(as.Date('1998-01-01'),as.Date('2025-12-01'),by = 'year'), lty = 2, col = 'gray')
legend('topleft', c('ESA-CCI', 'MODIS'), lty = 1, lwd = 2, col = c(3,4), bty = 'n')

time2.1 <- time2-15
cci_com <- dat_anom_cci[which(time1==time2.1[1]):length(time1)]

plot(cci_com, dat_anom_modis, asp = 1,
     pch = 16, col = alpha(1,.3), cex = 2,
     panel.first = abline(0,1, lty = 5))
cor.test(cci_com, dat_anom_modis,
         method = c("pear"), exact = T)



# Alternative download for ESA CCI ----------------------------------------


### alternative download from Plymouth Marine Lab (from the horse's mouth); https://www.oceancolour.org/
### this using the OPeNDAP entry point


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
  yr_mon <- rbind(data.frame(year = rep(1997,4),
                             month = sprintf('%02d', 9:12)),
                  expand.grid(year = 1998:2025, 
                              month = sprintf('%02d', 1:12))) |>
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
    save(dat_eez, time_dat, file = 'chl_comb_temp2.RData')
    
    nc_close(dat)
    rm(chl_pull, time_pull, dat)
    setTxtProgressBar(pb, i)
  }
  
} else {
  
  setwd(here("data/intermediate"))
  load('chl_comb_temp2.RData') ### this has been removed to local drive; it is too large for github
  
}


# dat_eez[dat_eez==chl_fill$value] <- NA
# dat_eez2 <- dat_eez

# sanity check
apply(dat_eez, 3, function(x)all(is.na(x)))
n_na <- apply(dat_eez, 3, function(x)sum(is.na(x)))


time_dat
time_dat <- as.Date(time_dat, origin = '1970-01-01')
# data.frame(yr_mon, date = as.Date(time_dat, origin = '1970-01-01'))

### log data prior ro global summary statistics then unlog per Product User Guide for v6.0 Dataset page 18
# https://climate.esa.int/documents/3154/D4.2_-_OC-CCI_Product_User_Guide_PUG_v6.5b_for_website.pdf
# dat_eez_log <- apply(log10(dat_eez), 3, mean , na.rm = T)
dat_eez_log <- log10(dat_eez) |> apply(3, mean , na.rm = T)
dat_eez_mean <- apply(dat_eez, 3, mean , na.rm = T) # median is more stable and equivalent between methods
dat_eez_med <- apply(dat_eez, 3, median , na.rm = T) # median is more stable and equivalent between methods

par(mfrow = c(2,1))
plot(time_dat, 10^dat_eez_log, typ = 'l')
plot(time_dat, dat_eez_med, typ = 'l')
# plot(time_dat, dat_eez_log, typ = 'l')
plot(time_dat, dat_eez_mean, typ = 'l')
plot(time_dat, n_na, typ = 'l')

dat_sq <- matrix(10^dat_eez_log[5:340], 12, 28) 
dat_anom_cci <- ((dat_sq - apply(dat_sq, 1, mean))/apply(dat_sq, 1, sd)) |> as.vector()
time_dat2 <- time_dat[5:340]


plot(time_dat2, dat_anom_cci, typ = 'l', lwd = 2, col = 3)
abline(h = c(-1,0,1), lty = 5)
abline(v = seq(as.Date('1998-01-01'),as.Date('2025-12-01'),by = 'year'), lty = 2, col = 'gray')


image(log10(apply(dat_eez, c(1,2), mean, na.rm = T)))

hist(log10(apply(dat_eez, c(1,2), mean, na.rm = T)))


test <- aperm(dat_eez, c(2,1,3))


chl_stack <- rast(aperm(dat_eez, c(2,1,3)),
                  ext = c(lon[ilon[1]], lon[ilon[length(ilon)]], 
                          lat[ilat[length(ilat)]], lat[ilat[1]]), 
                  # crs = 'EPSG:32662') # or pc_crs
                  # crs = pc_crs)
                  crs = 'EPSG:4326')

if (crs(chl_stack) != crs(gulf_eez)) {
  chl_stack <- project(chl_stack, crs(gulf_eez))
}

# 1. Crop the raster stack to the shapefile's extent
cropped_stack <- crop(chl_stack, gulf_eez)

# 2. Mask the cropped stack to the shapefile's exact boundaries
# This operation sets all cells outside the polygon to NA
# The cropped_stack is used here to make the masking faster, as it operates on a smaller area
subset_stack <- mask(cropped_stack, gulf_eez)

# View the result
plot(subset_stack)
plot(gulf_eez, add = TRUE, border = "red", lwd = 2) # Overlay the shapefile to visualize the subset

chl_mean <- global(subset_stack, mean, na.rm = T)
chl_mean_geo <- global(log10(subset_stack), mean, na.rm = T)

plot(time_dat, chl_mean$mean, typ = 'l', lwd = 2)
plot(time_dat, 10^(chl_mean_geo$mean), typ = 'l', lwd = 2)


dat_sq <- matrix(10^(chl_mean_geo$mean[5:340]), 12, 28) 
dat_anom_cci2 <- (dat_sq - apply(dat_sq, 1, mean))/apply(dat_sq, 1, sd) |> as.vector()

plot(time_dat2, dat_anom_cci2, typ = 'l', lwd = 2, col = 3)
abline(h = 0, lty = 5)
abline(v = seq(as.Date('1998-01-01'),as.Date('2025-12-01'),by = 'year'), lty = 2)

plot(dat_anom_cci, dat_anom_cci2, asp = 1)
abline(a = 0, b = 1, lty = 2, col = 2, lwd = 2)



