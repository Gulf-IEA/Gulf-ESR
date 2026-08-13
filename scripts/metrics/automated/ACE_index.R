# File created on 2026-06-15

#### 0. Setup ####
# Add any packages that are needed for analysis here.
library(IEAnalyzeR)
library(here)
library(ggplot2)
library(maps)

# File Naming Setup.
# !! Auto generated-Do Not Change !!
root_name <- "ACE"

csv_filename <- here(paste0("data/formatted/formatted_csvs/", root_name, "_formatted.csv"))
object_filename <- here(paste0("data/formatted/final_objects/", root_name, "_object.rds"))
plot_filename <- here(paste0("figures/plots/", root_name, "_plot.png"))

################  GULF OF MEXICO ACE INDEX DATA EXTRACTOR  ####################
# 1. Spatial Bounds (Gulf of Mexico)
min_lon <- -99; max_lon <- -80; min_lat <- 18; max_lat <- 31
styear  <- 1961; enyear <- 2025 

# 2. Download Live NOAA Tracking Data (v04r01 ensures 2023-2025 is populated)
options(download.file.method="libcurl")
url <- "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r01/access/csv/ibtracs.NA.list.v04r01.csv"

print("Downloading live hurricane database from NOAA...")
temp_dest <- here("data/unformatted/ibtracs_temp.csv")
download.file(url = url, destfile = temp_dest)

dat  <- read.csv(temp_dest, skip = 2, header = F)
datn <- read.csv(temp_dest, skip = 0, header = T)
names(dat) <- names(datn)
file.remove(temp_dest) 

# 3. Filter for the Gulf of Mexico Spatial Envelope
dat$incl <- NA
dat$incl[which(dat$LON < (max_lon + 1) & dat$LON > (min_lon - 1) & 
                 dat$LAT < (max_lat + 1) & dat$LAT > (min_lat - 1))] <- 1

# Isolate Tropical Storm tracking records inside the box
d <- dat[which(dat$incl == 1 & dat$NATURE == "TS"), ]

# 4. Legacy Synoptic Time Filtering (The Missing Step)
# Convert time fields and isolate standard 6-hour reporting periods
d$tim  <- strptime(d$ISO_TIME, format="%Y-%m-%d %H:%M:%S")
d$tim2 <- format(d$tim, "%H:%M")

# Keep ONLY observations recorded at 00:00, 06:00, 12:00, and 18:00
d2 <- d[d$tim2 %in% c("00:00", "06:00", "12:00", "18:00"), ]

# Filter down to your specific study period
d2 <- d2[d2$SEASON >= styear & d2$SEASON <= enyear, ]

# 5. Run Reconciled ACE Index Math
# (Using USA_WIND to replicate original HURDAT data input standards)
wind_speeds <- as.numeric(d2$USA_WIND)
d2$sqvel    <- wind_speeds * wind_speeds
d2$Season   <- factor(d2$SEASON, levels = styear:enyear)

# Calculate annual cumulative kinetic energy index (scaled by 10^-4)
storm_index <- tapply(d2$sqvel, d2$Season, sum, na.rm = T) * 10 ^ (-4)
storm_index[is.na(storm_index)] <- 0

# 6. SPIT OUT THE CLEAN CONVENTIONAL CSV
raw_ace_df <- data.frame(
  Year = names(storm_index),
  ACE_current = as.numeric(storm_index)
)

output_path <- here("data/unformatted/ACE_index_raw.csv")
dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
write.csv(raw_ace_df, file = output_path, row.names = FALSE)

print(paste0("Success! Clean, reconciled data written to: ", output_path))

#----------------------------------------------------
#### 1. Read Data ####
# Pull data from its source:
# Manual data: data/unformatted data
# Automated data: Add script for data call (API, package, etc.)
# Confidential data: Store locally in the confidential data folder
#   - This folder is excluded using gitignore and will not push to the GitHub repo
# If intermediate data (shapefiles etc.) are needed, please put them in data>intermediate
#   - Filename should use the syntax rootname_descriptivename
# ============================================================
# CONFIGURATION
# ============================================================

your_data <- read.csv("data/unformatted/ACE_index_raw.csv")

#----------------------------------------------------
#### 2. Clean data and create time series csv ####

#Transform the data to fit the IEA data format.
#For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
#Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.

#Define header components for the data rows (ignore year). Fill in the blanks here.
indicator_names = c("Accumulated Cyclone Energy (ACE) index")
unit_names = c("10^4 kt^2")
extent_names = c("Gulf of America")

formatted_data = IEAnalyzeR::convert_cleaned_data(your_data, indicator_names, unit_names, extent_names)


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

ggsave(
  filename = plot_filename, 
  width = 8, 
  height = 3, 
  units = "in", 
  dpi = 300
)
