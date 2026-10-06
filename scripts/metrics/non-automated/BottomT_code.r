# -*- coding: utf-8 -*-
# File created on 2026-06-12

# ### 0. Setup ####
# Add any packages that are needed for analysis here.
# If this is your first time running this script on this system, you must install 
# the R-to-Python bridge engine and its Python dependencies. 
# Highlight and run (Ctrl+Enter) the two lines below MANUALLY:
# renv::install("reticulate")
# reticulate::py_install(c("xarray", "numpy", "netcdf4", "pandas"))

library(IEAnalyzeR)
library(here)
library(ggplot2)
library(reticulate)
reticulate::py_require("scipy")

# File Naming Setup.
# !! Auto generated-Do Not Change !!
root_name <- "Bottom_T"

csv_filename <- here(paste0("data/formatted/formatted_csvs/", root_name, "_formatted.csv"))
object_filename <- here(paste0("data/formatted/final_objects/", root_name, "_object.rds"))
plot_filename <- here(paste0("figures/plots/", root_name, "_plot.png"))

# ----------------------------------------------------
# ### 0. Process data in Python ####

py_run_string(R"(
import os
import numpy as np
import xarray as xr


# ==========================================
# FILES
# ==========================================

gebco_file = ('data/intermediate/gebco_2026.nc')

bt_file = ('data/intermediate/cmems_mod_glo_phy_my_0.083deg_P1D-m_bottomT_97.50W-82.50W_24.00N-31.00N_1993-01-01-2026-05-26.nc')

start_date = "1993-01-01"
end_date = "2025-12-31"


# ==========================================
# SECTORS
# ==========================================

sectors = {

    "West": {
        "lon_min": -97.5,
        "lon_max": -89.0,
        "lat_min": 26.0
    },

    "Central": {
        "lon_min": -89.0,
        "lon_max": -85.3,
        "lat_min": 27.0
    },

    "WFS": {
        "lon_min": -85.3,
        "lon_max": -83.0,
        "lat_min": 24.5
    }
}


depth_min = -500
depth_max = -70


# ==========================================
# LOAD
# ==========================================

print("Loading bottom temperature...")

ds_bt = xr.open_dataset(bt_file)

bt = ds_bt["bottomT"].sel(
    time=slice(start_date,end_date)
)


print("Loading bathymetry...")

gebco = xr.open_dataset(gebco_file)

depth = gebco["elevation"].rename(
    {"lon":"longitude",
     "lat":"latitude"}
)

depth_bt = depth.interp(
    longitude=bt.longitude,
    latitude=bt.latitude
)


# ==========================================
# PROCESS
# ==========================================

def process_sector(cfg):

    mask = (
        (depth_bt >= depth_min) &
        (depth_bt <= depth_max) &
        (bt.longitude >= cfg["lon_min"]) &
        (bt.longitude <= cfg["lon_max"]) &
        (bt.latitude >= cfg["lat_min"])
    )

    bt_sector = bt.where(mask)

    index = bt_sector.mean(
        dim=["latitude","longitude"],
        skipna=True
    )


    # detrend

    coeff = index.polyfit(
        dim="time",
        deg=1
    )

    trend = xr.polyval(
        index.time,
        coeff.polyfit_coefficients
    )

    detrended = index-trend


    # remove seasonal cycle

    clim = (
        detrended
        .groupby("time.dayofyear")
        .mean("time")
    )

    anomaly = (
        detrended
        .groupby("time.dayofyear")
        -
        clim
    )

    return anomaly



results = {}

for name,cfg in sectors.items():

    print("Processing",name)

    results[name] = process_sector(cfg)


# ==========================================
# EXPORT
# ==========================================

df = xr.Dataset(results).to_dataframe().reset_index()

df["Date"] = df["time"].dt.strftime("%m/%d/%Y")

df = df[
    ["Date",
     "West",
     "Central",
     "WFS"]
]


output_dir = "data/unformatted"

os.makedirs(output_dir,exist_ok=True)

output_path = os.path.join(
    output_dir,
    "Bottom_T.csv"
)

df.to_csv(
    output_path,
    index=False
)

print("Saved:",output_path)
)")

# ----------------------------------------------------
# ### 1. Read Data ####
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

your_data <- read.csv("data/unformatted/Bottom_T.csv")
names(your_data)[1] <- "Year"
temp_dates <- as.Date(your_data$Year, format="%m/%d/%Y")
your_data$Year <- format(temp_dates,"%b-%Y")

# ----------------------------------------------------
# ### 2. Clean data and create time series csv ####

# Transform the data to fit the IEA data format.
# For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
# Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.

#Define header components for the data rows (ignore year). Fill in the blanks here.

indicator_names = c(
  "Western Gulf Shelf Bottom Temperature Anomaly",
  "Central Gulf Shelf Bottom Temperature Anomaly",
  "West Florida Shelf Bottom Temperature Anomaly"
)

unit_names = c(
  "deg C",
  "deg C",
  "deg C"
)

extent_names = c(
  "West",
  "Central",
  "West Florida Shelf"
)


formatted_data =
  IEAnalyzeR::convert_cleaned_data(
    your_data,
    indicator_names,
    unit_names,
    extent_names
  )

# ----------------------------------------------------
# ### 3. Save Formatted data as csv ####

# This will save your data to the appropriate folder.

write.csv(formatted_data, file = csv_filename, row.names = F)

# ----------------------------------------------------
# ### 4. Create Data_Prep object ####

# Please use your formatted csv to create a "data_prep" object.
# For more info on the data_prep function see the vignette linked above.

data_obj <- IEAnalyzeR::data_prep(csv_filename)


# ----------------------------------------------------
# ### 5. Save Formatted data_prep object ####

# This will save your data to the appropriate folder.

saveRDS(data_obj, file = object_filename)


# ----------------------------------------------------
# ### 6. Preview Plot ####
# Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# For more info on the plot_fn_obj function go HERE

IEAnalyzeR::plot_fn_obj(df_obj=data_obj, trend=TRUE)
p <- IEAnalyzeR::plot_fn_obj(df_obj = data_obj, trend = TRUE)

p <- p + ggtitle("Bottom Temperature Anomalies (1993–2025)") + theme(plot.title = element_text(size = 16, face = "bold"))

# ----------------------------------------------------
# ### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

ggsave(
  filename = plot_filename, 
  plot = p,
  width = 8, 
  height = 8, 
  units = "in", 
  dpi = 300
)
