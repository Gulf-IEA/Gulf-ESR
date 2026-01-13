
# File created on 2026-01-12 and draft completed on 2026-01-12 by B.Turley

#### 0. Setup ####
library(IEAnalyzeR)
library(here)
library(ggplot2)
library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(httr2)
library(jsonlite)

# File Naming Setup.
root_name <- "gulf-fuel-prices"

csv_filename <- here(paste0("data/formatted/formatted_csvs/", root_name, "_formatted.csv"))
object_filename <- here(paste0("data/formatted/final_objects/", root_name, "_object.rds"))
plot_filename <- here(paste0("figures/plots/", root_name, "_plot.png"))

#----------------------------------------------------
#### 1. Read Data ####

### need personal api key requested from BEA.gov; I keep mine in a file that is gitignored
keys <- readLines(here('data/intermediate/keys.api'))
eia_api <- keys[grep('eia',keys)+1]


### fuel prices
url <- paste0('https://api.eia.gov/v2/petroleum/pri/gnd/data/?api_key=',
              eia_api,
              '&frequency=monthly&data[0]=value&facets[series][]=EMD_EPD2DXL0_PTE_R30_DPG&facets[series][]=EMD_EPD2D_PTE_R30_DPG&facets[series][]=EMM_EPM0_PTE_R30_DPG&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000')


results <- url |>
  request() |>
  req_perform() |>
  resp_body_string() |>
  jsonlite::fromJSON()

fuel_prices <- results$response$data
fuel_prices$value <- as.numeric(fuel_prices$value)
fuel_prices$date <- paste0(fuel_prices$period,'-01') |> as.Date()
names(fuel_prices)[which(names(fuel_prices)=='value')] <- 'fuel_value'

### No 2 Diesel Low Sulfur (0-15 ppm) is fuel used for marine or offroad
### No 2 Diesel and low sulfur are approximately the same price; but No 2 is longer time series
# table(fuel_prices$`product-name`)

diesel_m <- subset(fuel_prices, `product-name`=='No 2 Diesel Low Sulfur (0-15 ppm)')
diesel <- subset(fuel_prices, `product-name`=='No 2 Diesel')
gas <- subset(fuel_prices, `product-name`=='Total Gasoline')

### plot for gut check
plot(diesel$date,diesel$fuel_value, typ = 'l', lwd = 2)
points(diesel_m$date,diesel_m$fuel_value, typ = 'l', lwd = 3, col = 2, lty = 3)
points(gas$date,gas$fuel_value, typ = 'l', lwd = 2, col = 3, lty = 2)


min(gas$date)
min(diesel$date)
min_yr <- 1995


### gdp deflation; from previous step see gdp-deflator_code.R
gdp_df <- read.csv(here('data/intermediate/gdp_deflator_interpolated.csv'))
gdp_df$date <- ymd(gdp_df$date)
gdp_df <- subset(gdp_df, year(date)>=min_yr)

### select year to adjust for inflation; estiamte a yearly mean
deflat_2025 <- subset(gdp_df, year(date)==2025, select = gdp_deflator_dollar) |>
  unlist() |> mean(na.rm=T) |> round(digits = 3)

g_merge <- merge(gas, gdp_df, by = 'date')
d_merge <- merge(diesel, gdp_df, by = 'date')

### adjust for inflation to 2025 USD
g_merge$value_2025 <- g_merge$fuel_value*(deflat_2025/g_merge$gdp_deflator_dollar)
d_merge$value_2025 <- d_merge$fuel_value*(deflat_2025/d_merge$gdp_deflator_dollar)

### plot for gut check
plot(d_merge$date,d_merge$value_2025, ylim = c(0, max(d_merge$value_2025,na.rm=T)), 
     typ = 'l', lwd = 3,panel.first = grid())
points(g_merge$date,g_merge$value_2025, typ = 'l', lwd = 3, col = 3)
points(d_merge$date,d_merge$fuel_value, typ = 'l', lwd = 3, col = 1, lty = 3)
points(g_merge$date,g_merge$fuel_value, typ = 'l', lwd = 3, col = 3,lty = 3)

### select and merge data
gas_out <- g_merge |> 
  select(date, fuel_value, value_2025) |>
  rename(gas_usd_gal = fuel_value,
         gas_2025_usd_gal = value_2025) |>
  mutate(gas_2025_usd_gal = round(gas_2025_usd_gal, digits = 3))
diesel_out <- d_merge |> 
  select(date, fuel_value, value_2025) |>
  rename(diesel_usd_gal = fuel_value,
         diesel_2025_usd_gal = value_2025) |>
  mutate(diesel_2025_usd_gal = round(diesel_2025_usd_gal, digits = 3))

fuel_merged <- merge(gas_out, diesel_out, by = c('date'))

setwd(here('data/intermediate'))
write.csv(fuel_merged, 'gulf_fuel_prices.csv', row.names = F)


#----------------------------------------------------
#### 2. Clean data and create time series csv ####

#Transform the data to fit the IEA data format.
#For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
#Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.

### load from previous stopping point
fuel_prices <- read.csv(here('data/intermediate/gulf_fuel_prices.csv'))
fuel_prices <- fuel_prices[ ,c(1,3,5)]
fuel_prices$date <- substr(fuel_prices$date, 1, 7)

#Define header components for the data rows (ignore year). Fill in the blanks here.
indicator_names = rep('Fuel Price', 2)
unit_names = c('Gasoline $/gallon (2025 USD)', 'Diesel $/gallon (2025 USD)')
extent_names = rep('Gulf-wide', 2)


fuel_prices_formatted = IEAnalyzeR::convert_cleaned_data(fuel_prices, indicator_names, unit_names, extent_names)

head(fuel_prices_formatted)

#----------------------------------------------------
#### 3. Save Formatted data as csv ####
 
# This will save your data to the appropriate folder.

write.csv(fuel_prices_formatted, file = csv_filename, row.names = F)

#----------------------------------------------------
#### 4. Create Data_Prep object ####
  
#Please use your formatted csv to create a "data_prep" object.
#For more info on the data_prep function see the vignette linked above.

fuel_price_obj <- IEAnalyzeR::data_prep(fuel_prices_formatted)


#----------------------------------------------------
#### 5. Save Formatted data_prep object ####

#This will save your data to the appropriate folder.
  
saveRDS(fuel_price_obj, file = object_filename)


#----------------------------------------------------
#### 6. Preview Plot ####
# Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# For more info on the plot_fn_obj function go HERE

IEAnalyzeR::plot_fn_obj(df_obj = fuel_price_obj, trend = T, sep_ylabs = TRUE, ylab_sublabel = c("extent", "unit"))

----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

ggsave(filename = plot_filename)
