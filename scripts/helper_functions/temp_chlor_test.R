
library(ncdf4)
library(terra)

# define years  --------------------------------
styear <- 1998
enyear <- 2025

# define spatial domain  --------------------------------
min_lon <- -98
max_lon <- -80
min_lat <- 18
max_lat <- 31

system.time(dat <- nc_open('https://www.oceancolour.org/thredds/dodsC/CCI_ALL-v6.0-1km-MONTHLY'))

ncvar_get(dat, 'crs') ### PlateCarree 54001; EPSG:32662
# Define Plate Carrée CRS
pc_crs <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

time <- ncvar_get(dat, 'time') |> 
  as.Date(origin = '1970-01-01')
lon <- ncvar_get(dat, 'lon')
lat <- ncvar_get(dat, 'lat')

ilon <- which(lon>min_lon & lon<max_lon)
ilat <- which(lat>min_lat & lat<max_lat)

system.time(test <- ncvar_get(dat, 'chlor_a', start = c(ilon[1], ilat[1], 1),
                  count = c(length(ilon), length(ilat), 1)))

image(log10(test))

r_stack <- rast(t(test),
                ext = c(lon[ilon[1]], lon[ilon[length(ilon)]], 
                        lat[ilat[length(ilat)]], lat[ilat[1]]), 
                # crs = 'EPSG:32662') # or pc_crs
                # crs = pc_crs)
                crs = 'EPSG:4326') 
plot(log10(r_stack))
# Path to your shapefile (replace with your actual file path)
# Example of reading a shapefile

setwd("C:/Users/brendan.turley/Documents/data/shapefiles/gulf_eez")
eez <- vect('eez.shp') |> makeValid()

# Ensure both the raster stack and shapefile have the same CRS
# If they have different CRSs, you should reproject the shapefile (or the raster)
# Example of reprojecting a SpatVector
# if (crs(eez) != crs(r_stack)) {
#   eez <- project(eez, crs(r_stack))
# }

if (crs(r_stack) != crs(eez)) {
  r_stack <- project(r_stack, crs(eez))
}

# 1. Crop the raster stack to the shapefile's extent
cropped_stack <- crop(r_stack, eez)

# 2. Mask the cropped stack to the shapefile's exact boundaries
# This operation sets all cells outside the polygon to NA
# The cropped_stack is used here to make the masking faster, as it operates on a smaller area
subset_stack <- mask(cropped_stack, eez)

# View the result
plot(subset_stack)
plot(eez, add = TRUE, border = "red", lwd = 2) # Overlay the shapefile to visualize the subset

global(r_stack, mean, na.rm = T)


