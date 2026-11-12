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


pct_change_na <- function(x) {
  n <- length(x)
  r <- c(NA, (x[2:n] - x[1:(n - 1)]) / x[1:(n - 1)] * 100) |> abs()
  return(r)
}


### the original "raw" data comes from a curated database created by SEFSC https://github.com/SEFSC/SEFSC-ODM-Management-History
### this data is then turned into an output by the R script ~/Gulf-ESR/scripts/automated/ACL_Open-Close-dates_code.R
### that output is ingested and transformed by R script ~/Gulf-ESR/scripts/other/create_fishery_open_close_matrices.R

setwd(here('data/intermediate/fishery_open_close/'))

### read recreational data
spp_rm <- c('DRUM, RED', 'JACK, ALMACO', 'TRIGGERFISH, QUEEN')
tmp <- read.csv(paste0('recreational_matrix_',1990,'.csv'))
tmp <- tmp[!is.element(tmp$species, spp_rm), ]

rec_comps <- data.frame(year = 1990:2025)
rec_spp <- data.frame(spp = tmp$species)
for(i in 1990:2025){
  tmp <- read.csv(paste0('recreational_matrix_',i,'.csv'))
  tmp <- tmp[!is.element(tmp$species, spp_rm), ]
  rec_comps$cols[i-1989] <- mean(colSums(tmp[,-1])) ### mean num spp open per day per year
  rec_comps$rows[i-1989] <- mean(rowSums(tmp[,-1])) ### mean season length per year
  rec_comps$rows_sd[i-1989] <- sd(rowSums(tmp[,-1])) ### sd season length per year
  
  ### percent change per year
  rs_tmp <- data.frame(spp = tmp$species, i = rowSums(tmp[,-1])) |>
    setNames(c('spp', paste0(i)))
  rec_spp <- merge(rec_spp, rs_tmp, by = 'spp', all = T)
}

rec_spp[rec_spp==366] <- 365
rec_spp[rec_spp==0] <- NA ## gray TF closed 2017 skewing the plot
rec_pct <- t(apply(rec_spp[,-1], 1, pct_change_na))
rec_pct_ind <- apply(rec_pct, 2, function (x) rbind(mean(x, na.rm = T), sd(x, na.rm = T)))
rec_poly <- c(rec_pct_ind[1,] + rec_pct_ind[2,], rev(rec_pct_ind[1,] - rec_pct_ind[2,]))
rec_poly <- ifelse(rec_poly < 0, 0, rec_poly)

plot(1990:2025, rec_pct_ind[1,],typ='o',
     ylim = range(rec_poly,na.rm=T),
     xlab = 'Year', ylab = 'Percent Change in Season Length', 
     main = 'Recreational Percent Change in Season Length')
polygon(c(1990:2025,2025:1990),rec_poly, col = rgb(0,0,1,.2), border = NA)

### read commmercial data
spp_rm <- c('DRUM, RED', 'JACK, ALMACO', 'TRIGGERFISH, QUEEN')
tmp <- read.csv(paste0('commercial_matrix_',1990,'.csv'))
tmp <- tmp[!is.element(tmp$species, spp_rm), ]

com_comps <- data.frame(year = 1990:2025)
com_spp <- data.frame(spp = tmp$species)
for(i in 1990:2025){
  tmp <- read.csv(paste0('commercial_matrix_',i,'.csv'))
  tmp <- tmp[!is.element(tmp$species, spp_rm), ]
  com_comps$cols[i-1989] <- mean(colSums(tmp[,-1])) ### mean num spp open per day per year
  com_comps$rows[i-1989] <- mean(rowSums(tmp[,-1])) ### mean season length per year
  com_comps$rows_sd[i-1989] <- sd(rowSums(tmp[,-1])) ### sd season length per year
  
  ### percent change per year
  rs_tmp <- data.frame(spp = tmp$species, i = rowSums(tmp[,-1])) |>
    setNames(c('spp', paste0(i)))
  com_spp <- merge(com_spp, rs_tmp, by = 'spp', all = T)
}

com_spp[com_spp==366] <- 365
com_spp[com_spp==0] <- NA ## gray TF closed 2017 skewing the plot
com_pct <- t(apply(com_spp[,-1], 1, pct_change_na))
com_pct_ind <- apply(com_pct, 2, function (x) rbind(mean(x, na.rm = T), sd(x, na.rm = T)))
com_poly <- c(com_pct_ind[1,] + com_pct_ind[2,], rev(com_pct_ind[1,] - com_pct_ind[2,]))
com_poly <- ifelse(com_poly < 0, 0, com_poly)

plot(1990:2025, com_pct_ind[1,],typ='o',
     ylim = range(com_poly, na.rm=T),
     xlab = 'Year', ylab = 'Percent Change in Season Length', 
     main = 'Commercial Percent Change in Season Length')
polygon(c(1990:2025,2025:1990),com_poly, col = rgb(0,0,1,.2), border = NA)

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
indicator_names2 = c(rep("Season Length",2),
                     rep("Year-to-year Changes to Season Length",2))
unit_names2 = c(rep("Mean Season Length",2),
                rep("Percent Change",2))
extent_names2 = rep(c("Commercial", 'Recreational'),2)

formatted_data2 = IEAnalyzeR::convert_cleaned_data(cbind(com_comps[,c(1,3)], 
                                                         rec_comps$rows,
                                                         com_pct_ind[1,],
                                                         rec_pct_ind[1,]),
                                                   indicator_names2, unit_names2, extent_names2)

lower_ci <- cbind(1990:2025, 
                  rep(NA, length(1990:2025)),
                  rep(NA, length(1990:2025)),
                  com_pct_ind[1,] - com_pct_ind[2,],
                  rec_pct_ind[1,] - rec_pct_ind[2,])
lower_ci <- ifelse(lower_ci < 0, 0, lower_ci)


ci_sealth <- convert_ci_data(lower_ci,
                             cbind(1990:2025, 
                                   rep(NA, length(1990:2025)),
                                   rep(NA, length(1990:2025)),
                                   com_pct_ind[1,] + com_pct_ind[2,],
                                   rec_pct_ind[1,] + rec_pct_ind[2,]),
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
sealth_obj <- IEAnalyzeR::data_prep(csv_filename2, ci_df = ci_sealth)


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
                        sep_ylabs = T, ylab_sublabel = c('extent','unit'),
                        pts = T, pt_size = 1,
                        fig.width = 10,
                        lwd = .5) +
  theme(plot.title = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 12, face = 'plain'),
        axis.text = element_text(size = 11, face = 'plain'))


#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

ggsave(filename = plot_filename2, height = 6, width = 10, unit = "in")

