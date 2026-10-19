# File created on 2026-06-12

#### 0. Setup ####
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
root_name <- "Loop_current"

csv_filename <- here(paste0("data/formatted/formatted_csvs/", root_name, "_formatted.csv"))
object_filename <- here(paste0("data/formatted/final_objects/", root_name, "_object.rds"))
plot_filename <- here(paste0("figures/plots/", root_name, "_plot.png"))

#----------------------------------------------------
#### 0. Process data in Python ####

py_run_string(R"(
import os
import numpy as np
import xarray as xr
from scipy.ndimage import label

# File paths configured to look inside data/intermediate/
# These files were generated and downloaded from Copernicus:
# FILE_NAME from https://data.marine.copernicus.eu/product/SEALEVEL_GLO_PHY_L4_MY_008_047/download?dataset=cmems_obs-sl_glo_phy-ssh_my_allsat-l4-duacs-0.125deg_P1D_202411
# NRT_FILE from https://data.marine.copernicus.eu/product/SEALEVEL_GLO_PHY_L4_NRT_008_046/download?dataset=cmems_obs-sl_glo_phy-ssh_nrt_allsat-l4-duacs-0.125deg_P1D_202506
# Change the filenames below to reflect any updates in Lat/Lon or Date Range if you download new data

FILE_NAME = 'data/intermediate/cmems_obs-sl_glo_phy-ssh_my_allsat-l4-duacs-0.125deg_P1D_sla-adt_98.94W-80.06W_18.06N-30.94N_1993-01-01-2025-10-18.nc'
NRT_FILE  = 'data/intermediate/cmems_obs-sl_glo_phy-ssh_nrt_allsat-l4-duacs-0.125deg_P1D_sla-adt_98.94W-80.06W_18.06N-30.94N_2025-01-01-2026-05-31.nc'

# This section defines: 
# 1) the longitude-based search corridor to look for the loop current core jet stream, 
# 2) the 'base-latitude' anchor line which forces the dynamic threshold to fulfill an "open path" constraint (avoids loop current eddies)

LON_CORRIDOR = (-90, -84)     
BASE_LATITUDE = 22.0          
START_DATE = '1994-01-01'

# SAFETY FLOORS: Prevents the dynamic threshold from picking a value too low and "flooding" the entire Gulf map during messy or retracted months.
MIN_THRESHOLD = 0.4  

# We're going to merge the re-analysis file with the NRT file so we're up to date :-)
print("Python: Loading NetCDF datasets from data/intermediate/...")
ds_reanalysis = xr.open_dataset(FILE_NAME)
ds_nrt = xr.open_dataset(NRT_FILE)
last_reanalysis_time = ds_reanalysis.time.max()
ds_nrt = ds_nrt.sel(time=slice(np.datetime64(last_reanalysis_time.values) + np.timedelta64(1, "D"), None))
ds = xr.concat([ds_reanalysis, ds_nrt], dim="time")

# Slice by time *before* calculating gradients, keeping the full spatial domain intact
# to prevent edge-effect corruption during coordinate derivative calculations
ds_time_subset = ds.sel(time=slice(START_DATE, None))

print("Python: Resampling to monthly means...")
adt_monthly = ds_time_subset["adt"].resample(time="MS").mean()

# Calculate horizontal ADT gradients across the global domain (Lindo-Atichati Step 1 & 2)
print("Python: Calculating horizontal ADT gradient fields...")
meters_per_degree_lat = 111120.0
meters_per_degree_lon = 111120.0 * np.cos(np.deg2rad(adt_monthly.latitude))
dadt_dy = adt_monthly.differentiate("latitude") / meters_per_degree_lat
dadt_dx = adt_monthly.differentiate("longitude") / meters_per_degree_lon
grad_magnitude = np.sqrt(dadt_dx**2 + dadt_dy**2)

print("Python: Extracting dynamic thresholds and running 2D object connectivity filter...")
num_months = len(adt_monthly.time)
filtered_max_lats = np.full(num_months, np.nan)

# This loops through each month to find the peak northward penetration of the Loop Current.
# It dynamically locates the absolute maximum spatial gradient (core velocity jet) inside the corridor,
# reads the ADT value at that point to establish a seasonal threshold, maps the features as 2D islands,
# isolates the island crossing the 22°N baseline, and completely discards horizontally separated eddies.

for t in range(num_months):
    adt_t = adt_monthly.isel(time=t)
    grad_t = grad_magnitude.isel(time=t)
    
    # Identify core jet front location via maximum gradient within active bounds
    search_zone_grad = grad_t.sel(longitude=slice(LON_CORRIDOR[0], LON_CORRIDOR[1]), latitude=slice(21.0, 28.0))
    max_grad_idx = search_zone_grad.argmax(dim=["latitude", "longitude"])
    max_grad_lat = search_zone_grad.latitude[max_grad_idx["latitude"]]
    max_grad_lon = search_zone_grad.longitude[max_grad_idx["longitude"]]

    # Extract local ADT height and enforce the MIN_THRESHOLD safety floor
    calculated_threshold = float(adt_t.sel(latitude=max_grad_lat, longitude=max_grad_lon).values)
    dynamic_threshold = max(calculated_threshold, MIN_THRESHOLD)
    
    # Formulate true 2D Connected Component Filter (The "Open Path" Constraint)
    binary_mask = (adt_t > dynamic_threshold).astype(int)
    labeled_array, num_features = label(binary_mask.values)
    
    # Locate array pixel indices matching the BASE_LATITUDE checkpoint line
    base_lat_idx = np.abs(adt_t.latitude.values - BASE_LATITUDE).argmin()
    lon_indices = np.where((adt_t.longitude.values >= LON_CORRIDOR[0]) & 
                           (adt_t.longitude.values <= LON_CORRIDOR[1]))[0]
    
    # Determine which island ID maps directly to the continuous trunk at the baseline checkpoint
    baseline_labels = labeled_array[base_lat_idx, lon_indices]
    valid_labels = baseline_labels[baseline_labels > 0]
    
    if len(valid_labels) > 0:
        main_trunk_id = np.bincount(valid_labels).argmax()
        
        # Build clean mask of ONLY the main trunk, wiping out all detached eddy IDs
        main_trunk_mask = xr.DataArray(labeled_array == main_trunk_id, coords=adt_t.coords)
        
        # Measure maximum latitude achieved exclusively by this connected continuous body inside the corridor
        corridor_trunk = main_trunk_mask.sel(longitude=slice(LON_CORRIDOR[0], LON_CORRIDOR[1]))
        filtered_max_lats[t] = corridor_trunk.latitude.where(corridor_trunk).max().values

# Wrap results back into an Xarray DataArray for export processing
northernmost_extent = xr.DataArray(filtered_max_lats, coords={"time": adt_monthly.time}, dims=["time"])

# Format for R import, make sure it reads out in a format that IEAnalyzeR doesn't get mad at
df = northernmost_extent.to_dataframe(name="Loop_current").reset_index()
df = df.rename(columns={"time": "Date"})
df["Date"] = df["Date"].dt.strftime("%m/%d/%Y")

# Export directly to data/unformatted/
output_dir = "data/unformatted"
os.makedirs(output_dir, exist_ok=True)
output_path = os.path.join(output_dir, "Loop_current.csv")
df.to_csv(output_path, index=False)

print(f"Python: Success! Saved clean CSV to: {output_path}"))")


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

your_data <- read.csv("data/unformatted/Loop_current.csv")
names(your_data)[1] <- "Year"
#temp_dates <- as.Date(your_data$Year, format = "%m/%d/%Y")
#your_data$Year <- as.numeric(format(temp_dates, "%Y")) + 
#  (as.numeric(format(temp_dates, "%m")) - 1) / 12
temp_dates <- as.Date(your_data$Year, format = "%m/%d/%Y")
your_data$Year <- format(temp_dates, "%b-%Y")

#----------------------------------------------------
#### 2. Clean data and create time series csv ####

#Transform the data to fit the IEA data format.
#For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
#Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.

#Define header components for the data rows (ignore year). Fill in the blanks here.
indicator_names = c("Northernmost Extent of Loop Current")
unit_names = c("latitude")
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
