
# File created on 2026-01-12 and draft completed on 2026-01-12 by B.Turley

#### 0. Setup ####
library(IEAnalyzeR)
library(here)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(httr2)
library(jsonlite)
library(lubridate)

# File Naming Setup.
root_name <- "gdp-deflator"

csv_filename <- here(paste0("data/formatted/formatted_csvs/", root_name, "_formatted.csv"))
object_filename <- here(paste0("data/formatted/final_objects/", root_name, "_object.rds"))
plot_filename <- here(paste0("figures/plots/", root_name, "_plot.png"))

----------------------------------------------------
#### 1. Read Data ####

### need personal api key requested from BEA.gov; I keep mine in a file that is gitignored
keys <- readLines(here('data/intermediate/keys.api'))
bea_api <- keys[grep('bea',keys)+1]

### GDP deflator; only available for annual or quarterly
### https://apps.bea.gov/iTable/?reqid=19&step=3&isuri=1&1921=survey&1903=13
# Suggested citation: U.S. Bureau of Economic Analysis, "Table 1.1.9. Implicit Price Deflators for Gross Domestic Product" (accessed Monday, August 14, 2023).
# Calendar years and quarters. Unless noted otherwise, annual and quarterly data are presented on a calendar basis.

url <- paste0('https://apps.bea.gov/api/data/?&UserID=',
              bea_api,
              '&method=GetData&DataSetName=NIPA&TableName=T10109&Frequency=A,M,Q&Year=ALL&ResultFormat=json')

results <- url |>
  request() |>
  req_perform() |>
  resp_body_string() |>
  fromJSON()

gdp_df <- results$BEAAPI$Results$Data |>
  subset(LineDescription == 'Gross domestic product') |>
  type.convert(as.is = T)

gdp_dfq <- gdp_df[which(nchar(gdp_df$TimePeriod)>4), ] 
yr_qr <- strsplit(gdp_dfq$TimePeriod, 'Q') |> 
  unlist() |> as.numeric()
yr_qr <- yr_qr |> matrix(length(yr_qr)/2,2,byrow=T) |> as.data.frame() |>
  setNames(c('year','quarter'))
gdp_dfq <- cbind(gdp_dfq, yr_qr)

### for interpolation, I will assume that the quarterly value is the mid-point of the quarter (or should it be the last month of the quarter)
gdp_dfq$month <- (gdp_dfq$quarter - 1)/4*12+2
gdp_dfq$yr_mth <- gdp_dfq$year + (gdp_dfq$month - 1)/12

value_out <- seq(gdp_dfq$year[1], gdp_dfq$year[nrow(gdp_dfq)]+(5/12), 1/12)

gdp_int <- approx(gdp_dfq$yr_mth, gdp_dfq$DataValue, xout = value_out) |> 
  as.data.frame() |> 
  setNames(c('year_decimal', 'gdp_deflator_dollar'))
gdp_int$gdp_deflator_dollar <- round(gdp_int$gdp_deflator_dollar, digits = 2)
gdp_int$date <- paste(as.integer(gdp_int$year_decimal),
                      round((gdp_int$year_decimal%%1)*12+1),
                      1, sep = '-') |> ymd()
gdp_int <- gdp_int[,c(3,1:2)]

plot(gdp_dfq$yr_mth, gdp_dfq$DataValue)
points(gdp_int$year_decimal, gdp_int$gdp_deflator_dollar, col = 2)

setwd('data/intermediate')
# write.csv(gdp_dfq, 'bea_gdp_deflator_table119.csv', row.names = F) # raw, uninterpolated not needed
write.csv(gdp_int, 'gdp_deflator_interpolated.csv', row.names = F)


################################################################################
#### rest of the code is superfluous as this metric is an intermediate step ####
#### GDP deflator was kept separate because it is useful beyond fuel prices ####
################################################################################


# ----------------------------------------------------
# #### 2. Clean data and create time series csv ####
# 
# #Transform the data to fit the IEA data format.
# #For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
# #Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.
# 
# #Define header components for the data rows (ignore year). Fill in the blanks here.
# indicator_names = c("")
# unit_names = c("")
# extent_names = c("")
# 
# formatted_data = IEAnalyzeR::convert_cleaned_data(your_data, indicator_names, unit_names, extent_names)
# 
# 
# ----------------------------------------------------
# #### 3. Save Formatted data as csv ####
#  
# # This will save your data to the appropriate folder.
# 
# write.csv(formatted_data, file = csv_filename, row.names = F)
# 
# ----------------------------------------------------
# #### 4. Create Data_Prep object ####
#   
# #Please use your formatted csv to create a "data_prep" object.
# #For more info on the data_prep function see the vignette linked above.
# 
# data_obj<-IEAnalyzeR::data_prep(csv_filename)
# 
# 
# ----------------------------------------------------
# #### 5. Save Formatted data_prep object ####
# 
# #This will save your data to the appropriate folder.
#   
# saveRDS(data_obj, file = object_filename)
# 
# 
# ----------------------------------------------------
# #### 6. Preview Plot ####
# # Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# # For more info on the plot_fn_obj function go HERE
# 
# IEAnalyzeR::plot_fn_obj(df_obj = data_obj, trends = TRUE)
# 
# ----------------------------------------------------
# #### 7. Save plot ####
# # This will save the plot to the correct folder.
# # Adjust height & width using (height=, width=, unit="in") if needed.
# 
# ggsave(filename = plot_filename)
