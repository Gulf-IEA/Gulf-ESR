# File created on 2026-08-28 by B. Turley

#### 0. Setup ####
# Add any packages that are needed for analysis here.
library(IEAnalyzeR)
library(here)
library(ggplot2)
library(readxl)
library(dplyr)

# File Naming Setup.
# !! Auto generated-Do Not Change !!
root_name <- "ifq_metrics"

csv_filename <- here(paste0("data/formatted/formatted_csvs/", root_name, "_formatted.csv"))
# object_filename <- here(paste0("data/formatted/final_objects/", root_name, "_object.rds"))
# plot_filename <- here(paste0("figures/plots/", root_name, "_plot.png"))

#----------------------------------------------------
#### 1. Read Data ####
# Pull data from its source:
# Manual data: data/unformatted data
# Automated data: Add script for data call (API, package, etc.)
# Confidential data: Store locally in the confidential data folder
#   - This folder is excluded using gitignore and will not push to the GitHub repo
# If intermediate data (shapefiles etc.) are needed, please put them in data>intermediate
#   - Filename should use the syntax rootname_descriptivename

### this spread sheet came from Juan Agar at SEFSC and housed at in our shared GDrive at:
### NOAA Gulf-IEA > Ecosystem Status Reports > 2025 Gulf ESR > 02. Data / Resources
setwd(here("data/unformatted/confidential"))
dat <- read_xlsx('sefsc_G&T_FY24 fof 2026.xlsx', sheet = "G&T CS", n_max = 26)
rs_dat <- read_xlsx('sefsc_Red_Snapper_FY24 for 2026.xlsx', sheet = 'RS CS', skip = 1, n_max = 22)

#----------------------------------------------------
#### 2. Clean data and create time series csv ####

#Transform the data to fit the IEA data format.
#For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
#Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.


### EDA; superfluous, but retained for future reference
# names(dat) ### pull columns 4:18
# dat$`Program Year`
# 
# for(i in c(1:19,25)){
#   if(i<12 | i>14){
#     plot(2010:2024, dat[i,4:18], typ = 'o', ylab = dat[i,1])
#     mtext(i) 
#   }
# }
# 
# plot(2010:2024, dat[1,4:18],
#      typ='o',ylab='pounds',
#      ylim=c(0,max(dat[1,4:18]|>as.numeric())))
# points(2010:2024, dat[2,4:18],typ='o',col=2)
# 
# plot(2010:2024, dat[15,4:18],
#      typ='o',ylab='GINI',
#      ylim=c(0.6,1))
# for(i in 16:19){
#   points(2010:2024, dat[i,4:18],typ='o',col=i-14)
#   
# }

### default to the SERO IFQ metrics, the SEFSC data for non-CS data
### uncertain about the difference between:
# Aggregate Revenue from non-catch share species  (SEFC)             
# Non-CS Species Revenue  2  (SEFC)
### Ask Juan Agar

### which rows?
# quota, landings, entities holding share, active vessels, trips, days at sea, 
# $$$ from CS spp (sefsc), $$$ from non-CS spp, $$$ non-CS spp non-CS trip,
# cost recovery
ifq_metrics <- dat[c(1:4,6:7,9:11,25),4:18] |> t() |> type.convert(as.is = T)
ifq_metrics <- cbind(rownames(ifq_metrics) |> as.numeric(),
                     ifq_metrics)
rownames(ifq_metrics) <- NULL
colnames(ifq_metrics) <- c("Year",
                           "Quota allocated to CS (lbs)",
                           "Aggregate Landings (lbs)",
                           "Entities holding share",
                           "Active vessels",
                           "Trips",
                           "Days at sea",
                           "CS Spp Revenue (USD)",
                           "Non-CS Spp Revenue (USD)",
                           "Non-CS Spp Revenue from non-CS trips (USD)",
                           "Cost recovery fee (USD)")


### separate set of graphs for gini coefficients
gini <- dat[c(15:19),4:18] |> t() |> type.convert(as.is = T)
gini <- cbind(rownames(gini) |> as.numeric(),
              gini)
rownames(gini) <- NULL
colnames(gini) <- c("Year",
                 dat[15:19,1] |> unlist() |> as.vector())


### add red snapper data 
# rs_dat$`Program Year`
rs_metrics <- rs_dat[c(1:4,6:7,9:11,20,14),4:21] |> t() |> type.convert(as.is = T)
rs_metrics <- cbind(rownames(rs_metrics) |> as.numeric(),
                    rs_metrics)
rownames(rs_metrics) <- NULL
colnames(rs_metrics) <- c("Year",
                           "Quota allocated to CS (lbs)",
                           "Aggregate Landings (lbs)",
                           "Entities holding share",
                           "Active vessels",
                           "Trips",
                           "Days at sea",
                           "CS Spp Revenue (USD)",
                           "Non-CS Spp Revenue (USD)",
                           "Non-CS Spp Revenue from non-CS trips (USD)",
                           "Cost recovery fee (USD)",
                          'Red Snapper')

### combine into one data frame for plotting
ifq_pRS_metrics <- merge(ifq_metrics, gini, by = "Year") |> 
  merge(rs_metrics, by = "Year", all = T, suffix = c('','_RS'))

ifq_metrics <- ifq_pRS_metrics |>
  group_by(Year) |>
  mutate(quota_lbs = sum(`Quota allocated to CS (lbs)`, `Quota allocated to CS (lbs)_RS` ,na.rm = T),
         landings_lbs = sum(`Aggregate Landings (lbs)`, `Aggregate Landings (lbs)_RS` ,na.rm = T),
         entities_w_share = sum(`Entities holding share`, `Entities holding share_RS` ,na.rm = T),
         vessels = sum(`Active vessels`, `Active vessels_RS` ,na.rm = T),
         trips = sum(`Trips`, `Trips_RS` ,na.rm = T),
         days_at_sea = sum(`Days at sea`, `Days at sea_RS` ,na.rm = T),
         cs_revenue = sum(`CS Spp Revenue (USD)`, `CS Spp Revenue (USD)_RS` ,na.rm = T),
         non_cs_revenue = sum(`Non-CS Spp Revenue (USD)`, `Non-CS Spp Revenue (USD)_RS` ,na.rm = T),
         non_cs_revenue_non_cs_tr = sum(`Non-CS Spp Revenue from non-CS trips (USD)`, `Non-CS Spp Revenue from non-CS trips (USD)_RS` ,na.rm = T),
         cost_recovery_usd = sum(`Cost recovery fee (USD)`, `Cost recovery fee (USD)_RS` ,na.rm = T)) |>
  select(-`Quota allocated to CS (lbs)`, -`Quota allocated to CS (lbs)_RS`,
         -`Aggregate Landings (lbs)`, -`Aggregate Landings (lbs)_RS`,
         -`Entities holding share`, -`Entities holding share_RS`,
         -`Active vessels`, -`Active vessels_RS`,
         -`Trips`, -`Trips_RS`,
         -`Days at sea`, -`Days at sea_RS`,
         -`CS Spp Revenue (USD)`, -`CS Spp Revenue (USD)_RS`,
         -`Non-CS Spp Revenue (USD)`, -`Non-CS Spp Revenue (USD)_RS`,
         -`Non-CS Spp Revenue from non-CS trips (USD)`, -`Non-CS Spp Revenue from non-CS trips (USD)_RS`,
         -`Cost recovery fee (USD)`, -`Cost recovery fee (USD)_RS`) |>
  filter(Year >= 2010) |>
  mutate(quota_lbs = quota_lbs/1e6,
         landings_lbs = landings_lbs/1e6,
         cs_revenue = cs_revenue/1e6,
         non_cs_revenue = non_cs_revenue/1e6,
         non_cs_revenue_non_cs_tr = non_cs_revenue_non_cs_tr/1e6,
         cost_recovery_usd = cost_recovery_usd/1e6)

#Define header components for the data rows (ignore year). Fill in the blanks here.

### IFQ metrics
indicator_names = c('Red Grouper',
                    'Gag',
                    'SWG',
                    'DWG',
                    'TF',
                    'Red Snapper',
                    "Total Quota",
                    "Total Landings",
                    "Entities with share",
                    "Active vessels",
                    "Trips",
                    "Days at sea",
                    "CS Spp Revenue",
                    "Non-CS Spp Revenue",
                    "Non-CS Spp Revenue from non-CS trips",
                    "Cost recovery fee")
unit_names = c(rep('Gini Coefficient',6), 
               'million lbs','million lbs','Entities','Vessels','Trips','Days',
               'million USD','million USD','million USD','million USD')
extent_names = rep('Gulf-wide',16)

formatted_data = IEAnalyzeR::convert_cleaned_data(ifq_metrics, indicator_names, unit_names, extent_names)


#----------------------------------------------------
#### 3. Save Formatted data as csv ####
 
# This will save your data to the appropriate folder.

write.csv(formatted_data, file = csv_filename, row.names = F)

#----------------------------------------------------
#### 4. Create Data_Prep object ####
  
#Please use your formatted csv to create a "data_prep" object.
#For more info on the data_prep function see the vignette linked above.


### spliting the dataframe out to plot groups of plots

quota_landings_obj <- IEAnalyzeR::data_prep(formatted_data[,c(1,8:9)])
oth_metrics_obj <- IEAnalyzeR::data_prep(formatted_data[,c(1,10:13)])
revenues_obj <- IEAnalyzeR::data_prep(formatted_data[,c(1,14:17)])
gini_obj <- IEAnalyzeR::data_prep(formatted_data[,c(1:7)])

#----------------------------------------------------
#### 5. Save Formatted data_prep object ####

#This will save your data to the appropriate folder.
  
object_filename1 <- here(paste0("data/formatted/final_objects/", root_name, "_object1.rds"))
object_filename2 <- here(paste0("data/formatted/final_objects/", root_name, "_object2.rds"))
object_filename3 <- here(paste0("data/formatted/final_objects/", root_name, "_object3.rds"))
object_filename4 <- here(paste0("data/formatted/final_objects/", root_name, "_object4.rds"))

saveRDS(quota_landings_obj, file = object_filename1)
saveRDS(oth_metrics_obj, file = object_filename2)
saveRDS(revenues_obj, file = object_filename3)
saveRDS(gini_obj, file = object_filename4)


######################################################
### the plots are broken into 4 separate groupings ###
######################################################

#----------------------------------------------------
#### 6. Preview Plot ####
# Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# For more info on the plot_fn_obj function go HERE

########################################################
### alternative combined plot for quota and landings ###
########################################################

png(here(paste0("figures/plots/", root_name, "_plot1a.png")),
    height = 5, width = 7, units = "in", res = 300, pointsize = 12)
plot(ifq_metrics$Year, ifq_metrics$quota_lbs, 
     type = 'o', pch = 16, lwd = 2,  las = 1, lty = 5, col = 'red',
     ylim = c(min(ifq_metrics$quota_lbs, ifq_metrics$landings_lbs), 
              max(ifq_metrics$quota_lbs, ifq_metrics$landings_lbs)),
     ylab = 'Quota and Landings (million lbs)', xlab = 'Year')
points(ifq_metrics$Year, ifq_metrics$landings_lbs, 
       type = 'o', pch = 16, lwd = 2, col = 1)
legend('topleft', legend = c('Quota', 'Landings'), 
       col = c('red', 1), lwd = 2, bty = 'n', lty = c(5,1))
dev.off()


IEAnalyzeR::plot_fn_obj(df_obj = quota_landings_obj, trend = TRUE, 
                        sep_ylabs = T, manual_title = 'IFQ Quota and Landings',
                        ylab_sublabel = c('indicator','unit'),
                        fig.width = 8, pts = T, pt_size = 2)  +
  theme(plot.title = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 12, face = 'plain'),
        axis.text = element_text(size = 11, face = 'plain'),
        strip.text = element_text(size = 11, face = 'plain'))

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

plot_filename1 <- here(paste0("figures/plots/", root_name, "_plot1.png"))

ggsave(filename = plot_filename1, height = 6, width = 8, unit = "in")


#----------------------------------------------------
#### 6. Preview Plot ####
# Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# For more info on the plot_fn_obj function go HERE

IEAnalyzeR::plot_fn_obj(df_obj = oth_metrics_obj, trend = TRUE, 
                        sep_ylabs = T, manual_title = 'IFQ Metrics',
                        fig.width = 12, pts = T, pt_size = 2)  +
  theme(plot.title = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 12, face = 'plain'),
        axis.text = element_text(size = 11, face = 'plain'),
        strip.text = element_text(size = 11, face = 'plain'))

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

plot_filename2 <- here(paste0("figures/plots/", root_name, "_plot2.png"))

ggsave(filename = plot_filename2, height = 5, width = 12, unit = "in")


#----------------------------------------------------
#### 6. Preview Plot ####
# Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# For more info on the plot_fn_obj function go HERE

IEAnalyzeR::plot_fn_obj(df_obj = revenues_obj, trend = TRUE, 
                        sep_ylabs = T, manual_title = 'IFQ Revenues',
                        ylab_sublabel = c('indicator','unit'),
                        fig.width = 12, pts = T, pt_size = 2)  +
  theme(plot.title = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 12, face = 'plain'),
        axis.text = element_text(size = 11, face = 'plain'),
        strip.text = element_text(size = 11, face = 'plain'))

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

plot_filename3 <- here(paste0("figures/plots/", root_name, "_plot3.png"))

ggsave(filename = plot_filename3, height = 5, width = 12, unit = "in")


#----------------------------------------------------
#### 6. Preview Plot ####
# Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# For more info on the plot_fn_obj function go HERE

IEAnalyzeR::plot_fn_obj(df_obj = gini_obj, trend = TRUE, 
                        sep_ylabs = T, manual_title = 'IFQ Gini',
                        fig.width = 12, pts = T, pt_size = 2, facet_scales = 'fixed')

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

plot_filename4 <- here(paste0("figures/plots/", root_name, "_plot4.png"))

ggsave(filename = plot_filename4, height = 7, width = 12, unit = "in")


