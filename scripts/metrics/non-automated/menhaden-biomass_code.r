# File created on 2026-05-05 by B.Turley

#### 0. Setup ####
# Add any packages that are needed for analysis here.
library(IEAnalyzeR)
library(here)
library(ggplot2)

# File Naming Setup.
root_name <- "menhaden-biomass"

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

setwd(here('data/unformatted'))
dat <- dget('gm001.rdat')
yr <- dat$t.series$year
bp1 <- dat$t.series$B
bpa <- dat$B.age
rec <- dat$t.series$recruits
rec.dev <- dat$t.series$logR.dev

plot(yr[-48], bp1[-48], typ = 'l')
plot(yr[-48], rec[-48], typ = 'l')
plot(yr[-48], rec.dev[-48], typ = 'l')

plot(yr[-48], bpa[-48,2],
     typ = 'b', ylim = range(bpa))
for(i in 3:5){
  points(yr[-48], bpa[-48,i],
         typ = 'b')
}

#----------------------------------------------------
#### 2. Clean data and create time series csv ####

#Transform the data to fit the IEA data format.
#For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
#Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.

#Define header components for the data rows (ignore year). Fill in the blanks here.
indicator_names = rep("SEDAR97 Menhaden",2)
# unit_names = c("Biomass",'Recruitment')
# extent_names = c('(1000s metric tons)','(Log Deviations)')
unit_names = c("Biomass (1000s metric tons)",'Recruitment (Log Deviations)')
extent_names = c('','')

indicator_names = c("Biomass (1000s metric tons)",'Recruitment (Log Deviations)')
unit_names = c("1000s metric tons",'Log Deviations')
extent_names = rep("SEDAR97 Menhaden",2)

formatted_data = IEAnalyzeR::convert_cleaned_data(cbind(yr[-48],bp1[-48],rec.dev[-48]),
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

IEAnalyzeR::plot_fn_obj(df_obj = data_obj, trend = TRUE, pts = T,
                        sep_ylabs = T, manual_title = 'SEDAR97 Menhaden')
                        # sep_ylabs = T, ylab_sublabel = c('unit','extent')

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

ggsave(filename = plot_filename, width = 7, height = 6, unit = 'in')
