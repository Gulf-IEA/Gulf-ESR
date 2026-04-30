
# File created on 2026-04-29 by  B. Turley

#### 0. Setup ####
# Add any packages that are needed for analysis here.
library(IEAnalyzeR)
library(here)
library(ggplot2)
library(terra)
library(lubridate)
library(purrr)
library(rnaturalearth)
library(rnaturalearthdata)

# File Naming Setup.
root_name <- "artificial-structure"

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

### SEFSC-PEM structure dataset
### data available on shared Gulf-IEA drive under ~/Ecosystem Status Reports/2025 Gulf ESR/02. Data / Resources/Artificial Structures Data (PEM) 
setwd("C:/Users/brendan.turley/Documents/data/PEM_artificial_structures/ARP FY26")

struc <- read.csv('ARP_update_Nov25.csv')
struc$year_built <- NA
struc$year_built[which(nchar(struc$Year)==4)] <- as.numeric(struc$Year[which(nchar(struc$Year)==4)])
struc$year_built[which(nchar(struc$Year)>4)] <- struc$Year[which(nchar(struc$Year)>4)] |> mdy() |> year()

struc_shp <- vect('ARPDPP_112525_final.shp')
struc_rast <- rast(ext(struc_shp), resolution = 0.1, crs=crs(struc_shp))


### BOEM structures
### download data from BOEM.gov
temp_file <- tempfile()
temp_dir <- tempdir()
download.file("https://www.data.boem.gov/Platform/Files/PlatStrucRawData.zip", temp_file, mode = "wb")
unzip(temp_file, exdir = temp_dir)

extracted_files <- list.files(temp_dir, full.names = TRUE)
plat <- read.csv(file.path(temp_dir,'PlatStrucRawData', "mv_platstruc_structures.txt"))

platforms <- plat[which(!is.na(plat$LONGITUDE)), ]

table(platforms$STRUC_TYPE_CODE)
# "CAIS"  "CT"    "FIXED" "FPSO"  "MOPU"  "MTLP"  "SEMI"  "SPAR"  "TLP"   "WP"
plt_typ <- c("CAIS","CT","FIXED")
platforms <- subset(platforms, STRUC_TYPE_CODE %in% plt_typ)
platforms$INSTALL_DATE <- mdy(platforms$INSTALL_DATE)
platforms$REMOVAL_DATE <- mdy(platforms$REMOVAL_DATE)


### platforms that exist in 2025
plt_2025 <- subset(platforms, year(INSTALL_DATE)==2025 | 
                     year(INSTALL_DATE)<2025) |>
  subset(year(REMOVAL_DATE)>2025 |
           is.na(REMOVAL_DATE))

### removals 2015-2025
plt_rm <- subset(platforms, year(REMOVAL_DATE)>=2015)

#----------------------------------------------------
#### 2. Clean data and create time series csv ####

#Transform the data to fit the IEA data format.
#For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
#Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.

### BOEM platfrom time series
yrs <- seq(min(year(platforms$INSTALL_DATE)),2025)
plt_yr <- list()
n <- 1
for(i in yrs){
  plt_i <- subset(platforms, year(INSTALL_DATE)==i | 
                    year(INSTALL_DATE)<i) |>
    subset(year(REMOVAL_DATE)>i |
             is.na(REMOVAL_DATE)) |> 
    nrow()
  plt_yr[[n]] <- data.frame(year = i, 
                            nplt = plt_i)
  n <- n + 1
}
plt_yr <- list_rbind(plt_yr)


### combine 2025 data with PEM data for plotting

ong <- c('OilCaisson','OilPlatformMaterial','OilRig')
# ong <- c('OilCaisson','OilRig')
struc_no <- struc[which(!is.element(struc$GEOFORM, ong)), ]

struc_no <- subset(struc_no, select = c('Longitude', 'Latitude', 'year_built'))

plat_2025 <- subset(plt_2025, select = c('LONGITUDE', 'LATITUDE', 'INSTALL_DATE')) |>
  setNames(c('Longitude', 'Latitude', 'year_built'))
plat_2025$year_built <- year(plat_2025$year_built)



str_yr <- aggregate(Longitude ~ year_built, data = struc_no, function(x) length(unique(x))) |>
  setNames(c('year','num_struc')) |>
  merge(data.frame(year = min(plt_yr$year):2025), all=T)
str_yr$num_struc[which(is.na(str_yr$num_struc))] <- 0
str_yr$cummul <- cumsum(str_yr$num_struc)

all_struc <- merge(plt_yr, str_yr, all = T)
all_struc$tot <- all_struc$cummul + all_struc$nplt

plot(all_struc$year, all_struc$tot)
points(all_struc$year, all_struc$cummul,col = 2)
points(all_struc$year, all_struc$nplt, col = 3)


#Define header components for the data rows (ignore year). Fill in the blanks here.
indicator_names = c('ONG rigs','artificial reefs','Total artificial Structures')
unit_names = rep('Number of structures',3)
extent_names =rep('Gulf-wide',3)

formatted_data = IEAnalyzeR::convert_cleaned_data(all_struc[,-3], indicator_names, unit_names, extent_names)


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

IEAnalyzeR::plot_fn_obj(df_obj = data_obj, trend = TRUE, facet_scales = 'fixed',
                        manual_title = NA)

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

ggsave(filename = plot_filename, height = 8, width = 7, unit="in")




#----------------------------------------------------
### combined plot

png(here('figures/plots/artificial_structure-combined-plot.png'), width = 7, height = 4, units = 'in', res = 300)
par(mar = c(4,4,1,1))

plot(formatted_data$indicator[3:nrow(formatted_data)],
     formatted_data$`Total artificial Structures`[3:nrow(formatted_data)],
     typ = 'l', lwd = 2, las = 1,
     xlab = 'Year', ylab = 'Number of Structures')
points(formatted_data$indicator[3:nrow(formatted_data)],
       formatted_data$`Oil and Gas rigs`[3:nrow(formatted_data)],
       typ = 'l', lwd = 2, col = 'orangered')
points(formatted_data$indicator[3:nrow(formatted_data)],
       formatted_data$`artificial reefs`[3:nrow(formatted_data)],
       typ = 'l', lwd = 2, col = 'purple3')
legend('topleft', c('Total Structures','ONG Rigs','artificial Reefs'),
       lty = 1, lwd = 2, col = c(1, 'orangered','purple3'),
       bty = 'n')

dev.off()

### maps

# define spatial domain  --------------------------------
min_lon <- -98
max_lon <- -80
min_lat <- 18
max_lat <- 31

### shapefile for plotting
world <- ne_download(scale = 10, type = "countries", 
                     returnclass = 'sv') |>
  crop(ext(min_lon,max_lon,min_lat,max_lat))



setwd("C:/Users/brendan.turley/Documents/data/PEM_artificial_structures/ARP FY26")

struc_shp <- vect('ARPDPP_112525_final.shp')
struc_rast <- rast(ext(struc_shp), resolution = 0.1, crs=crs(struc_shp))

struc_comb <- rbind(struc_no, plat_2025) |>
  vect(geom = c('Longitude','Latitude'), crs = 'EPSG:4326')

numb <- rasterize(struc_comb, struc_rast, fun = 'count')
# values(numb)[which(values(numb)>100)] <- 100
# area_numb <- cellSize(numb, unit='km')
# values(numb) <- values(numb)/values(area_numb)

yr_blt <- rasterize(struc_comb, struc_rast, 
                    field = 'year_built', function(x) round(median(x,na.rm=T),-1))


### removals

plt_rmv <- vect(plt_rm, geom = c('LONGITUDE','LATITUDE'), crs = 'EPSG:4326')
plt_rmv$yr_rm <- year(plt_rmv$REMOVAL_DATE)
plt_rmr <- rasterize(plt_rmv, struc_rast, 
                     field = 'yr_rm', function(x) round(median(x,na.rm=T),0))


png(here('figures/plots/artificial_structure-spatial-plot.png'),
    width = 5, height = 6, units = 'in', res = 300)
par(mfrow=c(3,1),
    mar = c(2,2,1,4))

plot(numb, col = rev(map.pal('plasma',100)),
     # range = c(0,100),
     plg = list(tick = 'out'),
     main = expression(paste('artificial Structures (number / 10 km'^2,')')),
     ext = c(-98, -80, 24.5, 30.5),
     mar = c(1,3,1,4),
     box = T, zebra = F)
plot(world, add= T, col = 'gray')

plot(yr_blt, col = map.pal('plasma',8),
     main = 'Mean Decade of Construction',
     ext = c(-98, -80, 24.5, 30.5),
     mar = c(1,3,1,4),
     box = T, zebra = F)
plot(world, add= T, col = 'gray')

plot(plt_rmr, col = map.pal('plasma',11),
     main = 'Mean Year of Removal (past decade)',
     type='classes',
     ext = c(-98, -80, 24.5, 30.5),
     mar = c(1,3,1,4),
     box = T, zebra = F)
plot(world, add= T, col = 'gray')

dev.off()

