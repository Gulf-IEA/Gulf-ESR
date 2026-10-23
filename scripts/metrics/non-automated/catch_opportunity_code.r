# File created on 2026-10-23 by B. Turley

#### 0. Setup ####
# Add any packages that are needed for analysis here.
library(IEAnalyzeR)
library(here)
library(ggplot2)

# File Naming Setup.
# !! Auto generated-Do Not Change !!
root_name <- "catch_opportunity"

csv_filename <- here(paste0("data/formatted/formatted_csvs/", root_name, "_formatted.csv"))
object_filename <- here(paste0("data/formatted/final_objects/", root_name, "_object.rds"))
plot_filename <- here(paste0("figures/plots/", root_name, "_plot.png"))

root_name2 <- "season_length"

csv_filename2 <- here(paste0("data/formatted/formatted_csvs/", root_name2, "_formatted.csv"))
object_filename2 <- here(paste0("data/formatted/final_objects/", root_name2, "_object.rds"))
plot_filename2 <- here(paste0("figures/plots/", root_name2, "_plot.png"))

#----------------------------------------------------
#### 1. Read Data ####
# Pull data from its source:
# Manual data: data/unformatted data
# Automated data: Add script for data call (API, package, etc.)
# Confidential data: Store locally in the confidential data folder
#   - This folder is excluded using gitignore and will not push to the GitHub repo
# If intermediate data (shapefiles etc.) are needed, please put them in data>intermediate
#   - Filename should use the syntax rootname_descriptivename


### the original "raw" data comes from a curated database created by SEFSC https://github.com/SEFSC/SEFSC-ODM-Management-History
### this data is then turned into an output by the R script ~/Gulf-ESR/scripts/automated/ACL_Open-Close-dates_code.R
### that output is ingested and transformed by R script ~/Gulf-ESR/scripts/other/create_fishery_open_close_matrices.R

setwd(here('data/intermediate/fishery_open_close/'))

### read recreational data
rec_comps <- data.frame(year = 1990:2025)
for(i in 1990:2025){
  tmp <- read.csv(paste0('recreational_matrix_',i,'.csv'))
  rec_comps$cols[i-1989] <- mean(colSums(tmp[,-1])) ### mean num spp open per day per year
  rec_comps$rows[i-1989] <- mean(rowSums(tmp[,-1])) ### mean season length per year
}

### read commmercial data
com_comps <- data.frame(year = 1990:2025)
for(i in 1990:2025){
  tmp <- read.csv(paste0('commercial_matrix_',i,'.csv'))
  com_comps$cols[i-1989] <- mean(colSums(tmp[,-1])) ### mean num spp open per day per year
  com_comps$rows[i-1989] <- mean(rowSums(tmp[,-1])) ### mean season length per year
}


#----------------------------------------------------
#### 2. Clean data and create time series csv ####

#Transform the data to fit the IEA data format.
#For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
#Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.

root_name <- "catch_opportunity"
#Define header components for the data rows (ignore year). Fill in the blanks here.
indicator_names = rep("Catch Opportunity",2)
unit_names = rep("Mean species Open per Day",2)
extent_names = c("Commercial", 'Recreational')

formatted_data = IEAnalyzeR::convert_cleaned_data(cbind(com_comps[,1:2], rec_comps$cols),
                                                  indicator_names, unit_names, extent_names)

root_name2 <- "season_length"
#Define header components for the data rows (ignore year). Fill in the blanks here.
indicator_names2 = rep("Season Length",2)
unit_names2 = rep("Mean Season Length",2)
extent_names2 = c("Commercial", 'Recreational')

formatted_data2 = IEAnalyzeR::convert_cleaned_data(cbind(com_comps[,c(1,3)], rec_comps$rows),
                                                  indicator_names2, unit_names2, extent_names2)

#----------------------------------------------------
#### 3. Save Formatted data as csv ####
 
# This will save your data to the appropriate folder.

write.csv(formatted_data, file = csv_filename, row.names = F)
write.csv(formatted_data2, file = csv_filename2, row.names = F)

#----------------------------------------------------
#### 4. Create Data_Prep object ####
  
#Please use your formatted csv to create a "data_prep" object.
#For more info on the data_prep function see the vignette linked above.

catopp_obj <- IEAnalyzeR::data_prep(csv_filename)
sealth_obj <- IEAnalyzeR::data_prep(csv_filename2)


#----------------------------------------------------
#### 5. Save Formatted data_prep object ####

#This will save your data to the appropriate folder.
  
saveRDS(catopp_obj, file = object_filename)
saveRDS(sealth_obj, file = object_filename2)


#----------------------------------------------------
#### 6. Preview Plot ####
# Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# For more info on the plot_fn_obj function go HERE

IEAnalyzeR::plot_fn_obj(df_obj = catopp_obj, trend = TRUE, 
                        sep_ylabs = T, ylab_sublabel = c("unit",'extent'),
                        pts = T, pt_size = 1,
                        fig.width = 8,
                        lwd = .5) +
  theme(plot.title = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 12, face = 'plain'),
        axis.text = element_text(size = 11, face = 'plain'))

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

ggsave(filename = plot_filename, height = 7, width = 8, unit = "in")


#----------------------------------------------------
#### 6. Preview Plot ####
# Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# For more info on the plot_fn_obj function go HERE

IEAnalyzeR::plot_fn_obj(df_obj = sealth_obj, trend = TRUE,
                        sep_ylabs = T, ylab_sublabel = c("unit",'extent'),
                        pts = T, pt_size = 1,
                        fig.width = 8,
                        lwd = .5) +
  theme(plot.title = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 12, face = 'plain'),
        axis.text = element_text(size = 11, face = 'plain'))

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

ggsave(filename = plot_filename2, height = 7, width = 8, unit = "in")
