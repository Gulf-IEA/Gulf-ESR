
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


# download by year to avoid timeout errors --------------------

######################################################
#### don't run while reviewing code; takes awhile ####
#### load saved intermediate files below loop ########
######################################################

review_code <- T ### set to F to rerun download loop

if(review_code == F){
  
  # get ERDDAP info  --------------------------------
  sst <- info('ncdcOisst21Agg_LonPM180') # this may work better
  
  # empty data  -------------------------------------------------
  dat_gulf <- c()
  dat_eez <- c()
  
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
        st_drop_geometry() |>
        group_by(time) |>
        summarize(sst_degC = mean(sst, na.rm = T),
                  anom_degC = mean(anom, na.rm = T))
      
      ### US EEZ
      sst_eez_sf <- st_as_sf(sst_grab, coords = c("longitude", "latitude"), crs = 4326) |>
        st_intersection(gulf_eez)
      
      sst_eez <- sst_eez_sf |>
        st_drop_geometry() |>
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
  
  setwd(here("data/intermediate"))
  save(dat_eez, dat_gulf, file = 'sst_comb_temp2.RData')
  
} else {
  
  setwd(here("data/intermediate"))
  load('sst_comb_temp2.RData')
  
}


### convert to dates
dat_gulf$time <- as.Date(dat_gulf$time)
dat_eez$time <- as.Date(dat_eez$time)

# add yearmonth column --------------------------
dat_gulf$yrmon <- paste(dat_gulf$time |> year(),
                        sprintf("%02.f", dat_gulf$time |> month()),
                        sep = '-')
dat_eez$yrmon <- paste(dat_eez$time |> year(),
                       sprintf("%02.f", dat_eez$time |> month()),
                       sep = '-')

#----------------------------------------------------
#### 2. Clean data and create time series csv ####

#Transform the data to fit the IEA data format.
#For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
#Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.

gulf_yrmon <- dat_gulf |>
  group_by(yrmon) |>
  summarise(gulf_sst_degC = mean(sst_degC, na.rm = T),
            gulf_anom_degC = mean(anom_degC, na.rm = T))

eez_yrmon <- dat_eez |>
  group_by(yrmon) |>
  summarise(eez_sst_degC = mean(sst_degC, na.rm = T),
            eez_anom_degC = mean(anom_degC, na.rm = T))

sst_merge <- merge(gulf_yrmon, eez_yrmon)

#Define header components for the data rows (ignore year). Fill in the blanks here.
indicator_names = rep("Sea Surface Temperature", 2)
unit_names = rep("Anomaly (°C)", 2)
extent_names = c("Gulf-wide", 'US Gulf EEZ')

formatted_data = select(sst_merge,
                        yrmon,
                        gulf_anom_degC,
                        eez_anom_degC) |>
  IEAnalyzeR::convert_cleaned_data(indicator_names, unit_names, extent_names)


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

ggsave(filename = plot_filename)


#----------------------------------------------------
#### 8. making Seasonal plots (sensu NEFSC SOE) ####


### helper functions
ax_convert_c2f <- function(vals, side = 4, n = 5, las = 1, ...){ ### ... used to pass other parameters for interior fxns
  tick_val <- pretty(vals*(9/5)+32, n = n, ...)
  axis(side, (tick_val-32)*(5/9), tick_val, las = las, ...)
}


### load from previously saved spot
setwd(here("data/intermediate"))
load('sst_comb_temp2.RData')

### convert to dates
dat_eez$time <- as.Date(dat_eez$time)

# add yearmonth column --------------------------
dat_eez$yrmon <- paste(dat_eez$time |> year(),
                       sprintf("%02.f", dat_eez$time |> month()),
                       sep = '-')

### add seasons
dat_eez$jday <- yday(dat_eez$time)

dat_eez <- dat_eez |>
  mutate(season = case_when(
    month(time)==12 | month(time)<3 ~ 'win',
    month(time)>2 & month(time)<6 ~ 'spr',
    month(time)>5 & month(time)<9 ~ 'sum',
    month(time)>8 & month(time)<12 ~ 'aut'
  )) |>
  arrange(time)

### create season_yr and adjust to make december n-1 part of winter n
dat_eez$season_yr <- ifelse(month(dat_eez$time)==12, year(dat_eez$time)+1, year(dat_eez$time))
dat_eez$season_yr[which(dat_eez$season_yr==2026)] <- NA


### seasonal means
eez_win <- aggregate(sst_degC ~ season_yr, data = subset(dat_eez, season=='win'),
                     mean, na.rm = T)
eez_spr <- aggregate(sst_degC ~ season_yr, data = subset(dat_eez, season=='spr'),
                     mean, na.rm = T)
eez_sum <- aggregate(sst_degC ~ season_yr, data = subset(dat_eez, season=='sum'),
                     mean, na.rm = T)
eez_aut <- aggregate(sst_degC ~ season_yr, data = subset(dat_eez, season=='aut'),
                     mean, na.rm = T)

png(here(paste0('figures/plots/sst-seasonal-plot.png')), width = 8, height = 6, units = 'in', res = 300)
par(mfrow = c(2,2), mar = c(3,5,2,3))
plot(eez_win$season_yr, eez_win$sst_degC, 
     typ = 'o', pch = 16, las = 1,
     panel.first = list(abline(lm(sst_degC ~ season_yr, data = eez_win), lwd = 4, col = 'orange'),
                        abline(h = mean(eez_win$sst_degC), col = 'gray', lwd = 2),
                        grid()),
     xlab = '', ylab = 'SST', main = 'Winter - DJF')
mtext('(°C)', side = 3, adj = -.15)
mtext('(°F)', side = 3, adj = 1.15)
ax_convert_c2f(eez_win$sst_degC, n = 4)

plot(eez_spr$season_yr, eez_spr$sst_degC, 
     typ = 'o', pch = 16, las = 1,
     panel.first = list(abline(lm(sst_degC ~ season_yr, data = eez_spr), lwd = 4, col = 'orange'),
                        abline(h = mean(eez_spr$sst_degC), col = 'gray', lwd = 2),
                        grid()),
     xlab = '', ylab = 'SST', main = 'Spring - MAM')
mtext('(°C)', side = 3, adj = -.15)
mtext('(°F)', side = 3, adj = 1.15)
ax_convert_c2f(eez_spr$sst_degC, n = 4)

plot(eez_sum$season_yr, eez_sum$sst_degC, 
     typ = 'o', pch = 16, las = 1,
     panel.first = list(abline(lm(sst_degC ~ season_yr, data = eez_sum), lwd = 4, col = 'orange'),
                        abline(h = mean(eez_sum$sst_degC), col = 'gray', lwd = 2),
                        grid()),
     xlab = '', ylab = 'SST', main = 'Summer - JJA')
mtext('(°C)', side = 3, adj = -.15)
mtext('(°F)', side = 3, adj = 1.15)
ax_convert_c2f(eez_sum$sst_degC, n = 4)

plot(eez_aut$season_yr, eez_aut$sst_degC,
     typ = 'o', pch = 16, las = 1,
     panel.first = list(abline(lm(sst_degC ~ season_yr, data = eez_aut), lwd = 4, col = 'orange'),
                        abline(h = mean(eez_aut$sst_degC), col = 'gray', lwd = 2),
                        grid()),
     xlab = '', ylab = 'SST', main = 'Fall - SON')
mtext('(°C)', side = 3, adj = -.15)
mtext('(°F)', side = 3, adj = 1.15)
ax_convert_c2f(eez_aut$sst_degC, n = 4)
dev.off()



### spatial plots

setwd("C:/Users/brendan.turley/Documents/R_projects/ESR-indicator-scratch/data/intermediate_files")
anom_2021 <- readRDS('anom_2021')
anom_2022 <- readRDS('anom_2022')
anom_2023 <- readRDS('anom_2023')
anom_2024 <- readRDS('anom_2024')
anom_2025 <- readRDS('anom_2025')


anom_a <- array(NA, dim = c(72,52,5))
anom_a[,,1] <- apply(anom_2021$anom, c(1,2), mean, na.rm=T)
anom_a[,,2] <- apply(anom_2022$anom, c(1,2), mean, na.rm=T)
anom_a[,,3] <- apply(anom_2023$anom, c(1,2), mean, na.rm=T)
anom_a[,,4] <- apply(anom_2024$anom, c(1,2), mean, na.rm=T)
anom_a[,,5] <- apply(anom_2025$anom, c(1,2), mean, na.rm=T)

anom_a <- array(NA, dim = c(72,52,1826))
anom_a[,,1:365] <- anom_2021$anom
anom_a[,,366:730] <- anom_2022$anom
anom_a[,,731:1095] <- anom_2023$anom
anom_a[,,1096:1461] <- anom_2024$anom
anom_a[,,1462:1826] <- anom_2025$anom

y <- 1:dim(anom_a)[3]

array_lm <- function (x){
  if(is.na(all(x))){
    NA
  } else {
    coef(lm(x~y))[2]
  }
}

yr5_trend <- apply(anom_a, c(1,2), array_lm)
hist(yr5_trend)
range(yr5_trend, na.rm = T)

imagePlot(yr5_trend)

a_brks <- seq(-3,3,.1)
a_cols <- cmocean('balance')(length(a_brks)-1)
t_brks <- seq(-.342,.342,.02)
t_cols <- cmocean('balance')(length(t_brks)-1)

par(mfrow=c(2,3))
imagePlot(-360+lons,lats,anom_a[,,1],breaks = a_brks, col = a_cols)
imagePlot(-360+lons,lats,anom_a[,,2],breaks = a_brks, col = a_cols)
imagePlot(-360+lons,lats,anom_a[,,3],breaks = a_brks, col = a_cols)
imagePlot(-360+lons,lats,anom_a[,,4],breaks = a_brks, col = a_cols)
imagePlot(-360+lons,lats,anom_a[,,5],breaks = a_brks, col = a_cols)
imagePlot(-360+lons,lats, yr5_trend, breaks = t_brks, col = t_cols)



imagePlot(-360+lons,lats,
          apply(anom_2025$anom, c(1,2), mean, na.rm=T),
          breaks = a_brks, col = a_cols)


t_brks <- seq(-.001,.001, .00005)
t_cols <- cmocean('balance')(length(t_brks)-1)
imagePlot(-360+lons,lats, yr5_trend, breaks = t_brks, col = t_cols)

### make seasonal maps

anom_2024$time_df <- data.frame(time = anom_2024$time)
anom_2025$time_df <- data.frame(time = anom_2025$time)

anom_2024$time_df <- anom_2024$time_df |>
  mutate(season = case_when(
    month(time)==12 | month(time)<3 ~ 'win',
    month(time)>2 & month(time)<6 ~ 'spr',
    month(time)>5 & month(time)<9 ~ 'sum',
    month(time)>8 & month(time)<12 ~ 'aut'
  )) |>
  arrange(time)

anom_2025$time_df <- anom_2025$time_df |>
  mutate(season = case_when(
    month(time)==12 | month(time)<3 ~ 'win',
    month(time)>2 & month(time)<6 ~ 'spr',
    month(time)>5 & month(time)<9 ~ 'sum',
    month(time)>8 & month(time)<12 ~ 'aut'
  )) |>
  arrange(time)

win_2025 <- anom_2025$anom[,,which(month(anom_2025$time_df$time)<3)] |>
  abind(anom_2024$anom[,,which(month(anom_2024$time_df$time)>11)], along = 3) |>
  apply(c(1,2), mean, na.rm = T)
spr_2025 <- anom_2025$anom[,,which(anom_2025$time_df$season=='spr')] |>
  apply(c(1,2), mean, na.rm = T)
sum_2025 <- anom_2025$anom[,,which(anom_2025$time_df$season=='sum')] |>
  apply(c(1,2), mean, na.rm = T)
aut_2025 <- anom_2025$anom[,,which(anom_2025$time_df$season=='aut')] |>
  apply(c(1,2), mean, na.rm = T)

replace_min_max <- function(dat, min, max){
  dat[which(dat < min)] <- min
  dat[which(dat > max)] <- max
  dat
}

tmin <- -3.5
tmax <- 3
win_2025 <- replace_min_max(win_2025, tmin, tmax)
spr_2025 <- replace_min_max(spr_2025, tmin, tmax)
sum_2025 <- replace_min_max(sum_2025, tmin, tmax)
aut_2025 <- replace_min_max(aut_2025, tmin, tmax)

a_brks <- seq(tmin, tmax, .1)
a_cols <- cmocean('balance')(length(a_brks)-1)

par(mfrow=c(2,2))
imagePlot(-360+lons,lats,
          win_2025, 
          breaks = a_brks, col = a_cols, asp = 1,
          xlab = 'Longitude (°W)', ylab = 'Latitude (°N)')
plot(gulf_eez['geometry'], add = T, fill=NA)
imagePlot(-360+lons,lats,
          spr_2025, 
          breaks = a_brks, col = a_cols, asp = 1,
          xlab = 'Longitude (°W)', ylab = 'Latitude (°N)')
plot(gulf_eez['geometry'], add = T, fill=NA)
imagePlot(-360+lons,lats,
          sum_2025, 
          breaks = a_brks, col = a_cols, asp = 1,
          xlab = 'Longitude (°W)', ylab = 'Latitude (°N)')
plot(gulf_eez['geometry'], add = T, fill=NA)
imagePlot(-360+lons,lats,
          aut_2025, 
          breaks = a_brks, col = a_cols, asp = 1,
          xlab = 'Longitude (°W)', ylab = 'Latitude (°N)')
plot(gulf_eez['geometry'], add = T, fill=NA)
