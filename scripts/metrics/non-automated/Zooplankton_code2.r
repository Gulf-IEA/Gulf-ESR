# File created on 2026-07-15

#### 0. Setup ####
# Add any packages that are needed for analysis here.
library(IEAnalyzeR)
library(here)
library(ggplot2)

# File Naming Setup.
# !! Auto generated-Do Not Change !!
root_name <- "Zooplankton"

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
#raw_data <- read.csv(here("data/unformatted/ESR_Calanoid_unformatted_SPRING.csv"))
raw_data <- read.csv(here("data/unformatted/ESR_Calanoid_unformatted.csv"),check.names = FALSE)
your_data <- raw_data[, c("YEAR", "SMean (no./m3)", "FMean (no./m3)")]
head(your_data)

spring_unc <- data.frame(
  YEAR = raw_data$YEAR,
  extent = "Spring",
  lower = raw_data$`Slower95 (no./m3)`,
  upper = raw_data$`Supper95 (no./m3)`
)

fall_unc <- data.frame(
  YEAR = raw_data$YEAR,
  extent = "Fall",
  lower = raw_data$`Flower95 (no./m3)`,
  upper = raw_data$`Fupper95 (no./m3)`
)

uncertainty <- rbind(spring_unc, fall_unc)

#----------------------------------------------------
#### 2. Clean data and create time series csv ####

#Transform the data to fit the IEA data format.
#For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
#Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.

#Define header components for the data rows (ignore year). Fill in the blanks here.
#indicator_names <- rep("Zooplankton Biomass", ncol(annual_by_area)-1)
#unit_names <- rep("Mean Calenoids (no./m3)", ncol(annual_by_area)-1)
indicator_names <- c(
  "Zooplankton Biomass",
  "Zooplankton Biomass"
)

unit_names <- c(
  "Mean Calanoids (no./m3)",
  "Mean Calanoids (no./m3)"
)

extent_names <- c(
  "Spring",
  "Fall"
)

multi_table <- IEAnalyzeR::convert_cleaned_data(your_data, indicator_names, unit_names, extent_names)
#multi_table <- IEAnalyzeR::convert_cleaned_data(your_data, extent_names, unit_names, indicator_names)
head(multi_table)

#----------------------------------------------------
#### 3. Save Formatted data as csv ####
 
# This will save your data to the appropriate folder.

write.csv(multi_table, file = csv_filename, row.names = F)

#----------------------------------------------------
#### 4. Create Data_Prep object ####
  
#Please use your formatted csv to create a "data_prep" object.
#For more info on the data_prep function see the vignette linked above.

#data_obj <- IEAnalyzeR::data_prep(csv_filename)
data_obj <- IEAnalyzeR::data_prep(
  csv_filename,
  subind = "extent"
)


#----------------------------------------------------
#### 5. Save Formatted data_prep object ####

#This will save your data to the appropriate folder.
  
saveRDS(data_obj, file = object_filename)



#----------------------------------------------------
#### 6. Create plot with IQR error bars ####

#library(dplyr)
#
## Rename to match IEAnalyzeR naming
#uncertainty2 <- uncertainty %>%
#  rename(
#    year = YEAR,
#    subnm = extent
#  )
#
## Join uncertainty to the plotting data
#plot_data <- left_join(
#  data_obj$data,
#  uncertainty2,
#  by = c("year", "subnm")
#)
#
## Plot
#p <- ggplot(plot_data, aes(x = year, y = value)) +
#  
#  geom_errorbar(
#    aes(
#      ymin = lower,
#      ymax = upper
#    ),
#    width = 0.25,
#    linewidth = 0.4
#  ) +
#  
#  geom_line(linewidth = 0.8) +
#  
#  geom_point(size = 2) +
#  
#  facet_wrap(
#    ~subnm,
#    ncol = 1,
#    scales = "free_y"
#  ) +
#  
#  labs(
#    title = "Zooplankton Biomass",
#    x = "Year",
#    y = "Median Calanoids (no./m3) + Interquartile Range"
#  ) +
#  
#  theme_bw() +
#  
#  theme(
#    plot.title = element_text(hjust = 0.5),
#    strip.background = element_rect(fill = "grey90"),
#    strip.text = element_text(face = "bold")
#  )
#
#print(p)

#----------------------------------------------------
#### 7. Save plot ####

#ggsave(
#  filename = plot_filename,
#  plot = p,
#  width = 6,
#  height = 6,
#  units = "in",
#  dpi = 300
#)


#----------------------------------------------------
#### 6. Preview Plot ####
# Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# For more info on the plot_fn_obj function go HERE

IEAnalyzeR::plot_fn_obj(data_obj, trend = TRUE)

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

ggsave(
  filename = plot_filename, 
  width = 6, 
  height = 6, 
  units = "in", 
  dpi = 300
)
