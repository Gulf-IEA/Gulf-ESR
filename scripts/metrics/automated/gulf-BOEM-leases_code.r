
# File created on 2026-01-27 by B. Turley; drafted 2026-01-27

#### 0. Setup ####
library(IEAnalyzeR)
library(ggplot2)
library(here)
library(lubridate)

# File Naming Setup.
root_name <- "Gulf-BOEM-Leases"

csv_filename <- here(paste0("data/formatted/formatted_csvs/", root_name, "_formatted.csv"))
object_filename <- here(paste0("data/formatted/final_objects/", root_name, "_object.rds"))
plot_filename <- here(paste0("figures/plots/", root_name, "_plot.png"))

#----------------------------------------------------
#### 1. Read Data ####

### download data from BOEM.gov
temp_file <- tempfile()
temp_dir <- tempdir()
download.file("https://www.data.boem.gov/Leasing/Files/LABRawData.zip", temp_file, mode = "wb")
unzip(temp_file, exdir = temp_dir)

extracted_files <- list.files(temp_dir, full.names = TRUE)
leases <- read.csv(file.path(temp_dir,'LABRawData', "mv_lease_area_block.txt"))

leases$LEASE_EFF_DATE <- mdy(leases$LEASE_EFF_DATE)
leases$LEASE_EXPIR_DATE <- mdy(leases$LEASE_EXPIR_DATE)


#----------------------------------------------------
#### 2. Clean data and create time series csv ####

#Transform the data to fit the IEA data format.
#For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
#Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.

### sum number of leases per year
yrs <- seq(min(year(leases$LEASE_EFF_DATE),na.rm=T),2025)
lease_yr <- list()
n <- 1
for(i in yrs){
  lease_i <- subset(leases, year(LEASE_EFF_DATE)==i | 
                      year(LEASE_EFF_DATE)<i) |>
    subset(year(LEASE_EXPIR_DATE)>i |
             is.na(LEASE_EXPIR_DATE)) |> 
    nrow()
  lease_yr[[n]] <- data.frame(year = i, 
                              nlease = lease_i)
  n <- n + 1
}
lease_yr <- unlist(lease_yr) |> 
  matrix(90,2,byrow=T) |> 
  as.data.frame() |> 
  setNames(c('year','nlease'))

### plot to check output
plot(lease_yr$year, lease_yr$nlease, typ = 'l', lwd = 2)

# setwd("~/R_projects/ESR-indicator-scratch/data/processed")
# write.csv(lease_yr, 'leases_yr.csv', row.names = F)
# lease_yr <- read.csv('leases_yr.csv')

#Define header components for the data rows (ignore year). Fill in the blanks here.
indicator_names = c('BOEM ONG Leases')
unit_names = c('Leases')
extent_names = c('')

formatted_data = IEAnalyzeR::convert_cleaned_data(lease_yr, indicator_names, unit_names, extent_names)


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

IEAnalyzeR::plot_fn_obj(df_obj = data_obj, trend = TRUE)

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

ggsave(filename = plot_filename)
