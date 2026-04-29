
### this code is not used in production but a test of NPP metric

library(abind)
library(lubridate)
library(ncdf4)
library(terra)
library(sf)
library(reticulate)

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


# download --------------------

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
    dataset_id = "cmems_obs-oc_glo_bgc-pp_my_l4-multi-4km_P1M",
    dataset_version="202603",
    variables = list("PP"),  # Use list() so reticulate passes a proper Python list
    minimum_longitude = min_lon,
    maximum_longitude = max_lon,
    minimum_latitude = min_lat,
    maximum_latitude = max_lat,
    start_datetime = "1997-09-01T00:00:00",
    end_datetime   = "2025-12-01T00:00:00",
    output_directory = "C:/Users/brendan.turley/Documents/data/copernicusmarine/pp"
  )
  
  
} else {
  
  setwd('C:/Users/brendan.turley/Documents/data/copernicusmarine/pp')
  dat <- nc_open('cmems_obs-oc_glo_bgc-pp_my_l4-multi-4km_P1M_PP_97.98W-80.02W_18.02N-30.98N_1997-09-01-2025-12-01.nc')
  
}


pp <- ncvar_get(dat, 'PP')
pp <- pp[,,5:340]
time <- ncvar_get(dat, 'time') |>
  as.Date(origin = '1900-01-01')
time <- time[5:340]
nc_close(dat)

pp <- aperm(pp, c(2,1,3))
pp_r <- rast(pp[dim(pp)[1]:1,,], crs="EPSG:4326")
ext(pp_r) <- c(min_lon, max_lon, min_lat, max_lat)
time(pp_r) <- as.Date(time)


### annual
ann_gwide <- crop(pp_r, gulf_eez) |> mask(gulf_eez)
cell_area <- cellSize(ann_gwide, unit = "m")

# Calculate days in each month (handles leap years)
days_in_month <- days_in_month(time(ann_gwide))

# 1. Multiply daily rate by days in the month (Raster Algebra)
npp_monthly_rate <- ann_gwide * days_in_month
# 2. Multiply by cell area to get total mg C per grid cell
npp_mg_per_cell <- npp_monthly_rate * cell_area

# Sum all cells for each of the 240 layers (basin-wide monthly totals)
monthly_basin_mg <- global(npp_mg_per_cell, fun = "sum", na.rm = TRUE)

# Convert the results into a data frame for easier aggregation
df <- data.frame(
  year = year(time(ann_gwide)),
  mg_C = monthly_basin_mg$sum
)

# Sum monthly totals into annual totals
annual_npp_mg <- aggregate(mg_C ~ year, data = df, FUN = sum)

# Convert mg to Metric Tonnes
annual_npp_mg$tonnes_C <- annual_npp_mg$mg_C / 1e9
annual_npp_mg$Mtonnes_C <- annual_npp_mg$tonnes_C / 1e6

plot(annual_npp_mg$year, annual_npp_mg$Mtonnes_C, typ = 'b')



### monthly anomaly
monthly_df <- data.frame(
  date = time(ann_gwide),
  mg_C = monthly_basin_mg$sum
)

npp_sq <- t(matrix(monthly_df$mg_C, 12, 28)) |> log10()
npp_sq_a <- (npp_sq - apply(npp_sq, 2, mean)) / apply(npp_sq, 2, sd)
npp_sq_a <- (npp_sq - apply(npp_sq, 2, mean))

plot(monthly_npp_mg$date,as.vector(t(npp_sq_a)), typ = 'h')

