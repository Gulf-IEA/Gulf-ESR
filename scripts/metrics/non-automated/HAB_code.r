# -*- coding: utf-8 -*-
# File updated on 2026-06-18

# ### 0. Setup ####
library(IEAnalyzeR)
library(here)
library(ggplot2)
library(reticulate)
library(dplyr)
library(tidyr)

# Automatically hook into the host computer's active Python execution engine
reticulate::use_python(Sys.which("python"), required = FALSE)

# File Naming Setup.
# !! Auto generated-Do Not Change !!
root_name <- "HAB_counties"

csv_filename  <- here(paste0("data/formatted/formatted_csvs/", root_name, "_formatted.csv"))
object_filename <- here(paste0("data/formatted/final_objects/", root_name, "_object.rds"))
plot_filename   <- here(paste0("figures/plots/", root_name, "_plot.png"))

# Generate dynamic project folder paths for the Python subsystem
r_data_dir   <- here("data/unformatted/ecospace_ascii")
r_shp_path   <- here("data/intermediate/FloridaCountyBoundarieswithFDOTDistricts.shp")
r_output_dir <- here("data/unformatted")

# ----------------------------------------------------
# ### 0. Process data in Python ####

py_run_string(R"(
import os
import glob
import numpy as np
import pandas as pd
import geopandas as gpd

# Load location paths directly from R environment variables
data_dir   = r.r_data_dir
shp_path   = r.r_shp_path
output_dir = r.r_output_dir

start_year = 1985
end_year = 2025

target_counties = [
    "Escambia", "Santa Rosa", "Okaloosa", "Walton", "Bay", "Gulf",
    "Franklin", "Wakulla", "Jefferson", "Taylor", "Dixie", "Levy",
    "Citrus", "Hernando", "Pasco", "Pinellas", "Hillsborough",
    "Manatee", "Sarasota", "Charlotte", "Lee", "Collier",
    "Monroe"
]

def read_asc(filepath):
    header = {}
    with open(filepath, "r") as f:
        header_lines = 0
        while header_lines < 6:
            line = f.readline()
            if not line.strip():
                continue
            parts = line.strip().split()
            if len(parts) < 2:
                continue
            key = parts[0].upper()
            value = parts[1]
            header[key] = float(value)
            header_lines += 1
            
        ncols = int(header["NCOLS"])
        nrows = int(header["NROWS"])
        xll = header["XLLCORNER"]
        yll = header["YLLCORNER"]
        cellsize = header["CELLSIZE"]
        nodata = header.get("NODATA_VALUE", -3.4e38)
        data = np.loadtxt(f)

    data[data <= -1e20] = np.nan
    data[data == nodata] = np.nan
    data[data <= 0] = np.nan
    data[data < 10000] = np.nan 
    data = np.ma.masked_invalid(data)

    extent = [xll, xll + ncols * cellsize, yll, yll + nrows * cellsize]
    return data, extent, header

print("Python: Loading and projecting county shapefile...")
gdf = gpd.read_file(shp_path)
gdf = gdf[gdf["NAME"].isin(target_counties)].copy()
gdf = gdf[["NAME", "geometry"]]
gdf = gdf.to_crs("EPSG:3086")

print("Python: Precomputing grid-to-county spatial mapping...")
sample_pattern = os.path.join(data_dir, "sdmTMB_log__*.asc")
sample_files = glob.glob(sample_pattern)
if not sample_files:
    raise FileNotFoundError(f"No ASCII (.asc) files found in {data_dir}")

_, _, sample_header = read_asc(sample_files[0])
ncols = int(sample_header["NCOLS"])
nrows = int(sample_header["NROWS"])
xll = sample_header["XLLCORNER"]
yll = sample_header["YLLCORNER"]
cellsize = sample_header["CELLSIZE"]

x_coords = xll + (np.arange(ncols) + 0.5) * cellsize
y_coords = yll + (nrows - 1 - np.arange(nrows) + 0.5) * cellsize
X_mesh, Y_mesh = np.meshgrid(x_coords, y_coords)

flat_X = X_mesh.flatten()
flat_Y = Y_mesh.flatten()

grid_points_gdf = gpd.GeoDataFrame(geometry=gpd.points_from_xy(flat_X, flat_Y), crs="EPSG:4326")
grid_points_gdf = grid_points_gdf.to_crs("EPSG:3086")

joined_lookup = gpd.sjoin_nearest(grid_points_gdf, gdf, how="left")
joined_lookup = joined_lookup.reset_index().drop_duplicates(subset='index').set_index('index')
pixel_county_map = joined_lookup["NAME"].values

date_range = pd.date_range(start=f"{start_year}-01-01", end=f"{end_year}-12-31", freq="MS")
hovmoller_df = pd.DataFrame(np.nan, index=target_counties, columns=date_range)

print(f"Python: Processing monthly timelines from {start_year} to {end_year}...")
for year in range(start_year, end_year + 1):
    pattern = os.path.join(data_dir, f"sdmTMB_log__{year}*.asc")
    files = sorted(glob.glob(pattern))
    
    monthly_files = {}
    for f in files:
        basename = os.path.basename(f)
        datestr = basename.split("__")[1].split(".")[0]
        month = int(datestr[4:6])
        monthly_files[month] = f
        
    for month in range(1, 13):
        current_timestep = pd.Timestamp(year=year, month=month, day=1)
        if month in monthly_files:
            data, _, _ = read_asc(monthly_files[month])
            flat_data = data.filled(np.nan).flatten()
            valid_mask = ~np.isnan(flat_data)
            
            if np.any(valid_mask):
                valid_values = flat_data[valid_mask]
                valid_counties = pixel_county_map[valid_mask]
                
                temp_df = pd.DataFrame({'concentration': valid_values, 'county': valid_counties})
                temp_df = temp_df.dropna(subset=['county'])
                monthly_county_means = temp_df.groupby('county')['concentration'].mean()
                
                for county, mean_val in monthly_county_means.items():
                    if county in hovmoller_df.index:
                        hovmoller_df.loc[county, current_timestep] = mean_val

print("Python: Reshaping data layout for time-series export...")
export_df = hovmoller_df.T.reset_index().rename(columns={'index': 'Date'})
export_df["Date"] = export_df["Date"].dt.strftime("%m/%d/%Y")

os.makedirs(output_dir, exist_ok=True)
output_path = os.path.join(output_dir, "HAB_timeseries.csv")
export_df.to_csv(output_path, index=False)
print(f"Python: Success! Saved wide format matrix to: {output_path}")
)")

# ----------------------------------------------------
# ### 1. Read Data ####

target_counties <- c(
  "Escambia", "Santa Rosa", "Okaloosa", "Walton", "Bay", "Gulf",
  "Franklin", "Wakulla", "Jefferson", "Taylor", "Dixie", "Levy",
  "Citrus", "Hernando", "Pasco", "Pinellas", "Hillsborough",
  "Manatee", "Sarasota", "Charlotte", "Lee", "Collier", "Monroe"
)

# Load Python's exported file safely without converting spaces to dots
your_data <- read.csv(here("data/unformatted/HAB_timeseries.csv"), check.names = FALSE)

# Store a clean copy with true Date objects explicitly for our custom Hovmöller visualization
plotting_data <- your_data
plotting_data$TrueDate <- as.Date(plotting_data$Date, format = "%m/%d/%Y")

# Transform Date column to standard fractional years required exclusively by IEAnalyzeR core
names(your_data)[1] <- "Year"
temp_dates <- as.Date(your_data$Year, format = "%m/%d/%Y")
your_data$Year <- as.numeric(format(temp_dates, "%Y")) + 
  (as.numeric(format(temp_dates, "%m")) - 1) / 12

# ----------------------------------------------------
# ### 2. Clean data and create time series csv ####

# Calculate a clean Regional Average across all counties.
# This provides the clean 2-column structure that preserves your template layout perfectly.
your_data$Regional_Average <- rowMeans(your_data[, target_counties], na.rm = TRUE)
baseline_data <- your_data[, c("Year", "Regional_Average")]

indicator_names <- "Karenia brevis Bloom Intensity (Regional Average)"
unit_names      <- "Mean Log Concentration (>10k Cells)"
extent_names    <- "Florida Gulf Coast"

# Executes positionally without a hitch, matching your original Loop Current layout
formatted_data <- IEAnalyzeR::convert_cleaned_data(
  baseline_data, 
  indicator_names, 
  unit_names, 
  extent_names
)

# ----------------------------------------------------
# ### 3. Save Formatted data as csv ####

write.csv(formatted_data, file = csv_filename, row.names = F)

# ----------------------------------------------------
# ### 4. Create Data_Prep object ####

data_obj <- IEAnalyzeR::data_prep(csv_filename)

# ----------------------------------------------------
# ### 5. Save Formatted data_prep object ####

saveRDS(data_obj, file = object_filename)

# ----------------------------------------------------
# ### 6. Preview Plot (The Theme Thief Method) ####

print("R: Extracting package styling guidelines and generating Hovmöller plot...")

# Step A: Generate a quick standard package timeline to harvest its official design theme
dummy_plot <- IEAnalyzeR::plot_fn_obj(df_obj = data_obj, trend = FALSE)
iea_style  <- dummy_plot$theme

# Step B: Pivot our wide layout data into a long structure for standard 2D ggplot tiles
long_hab <- plotting_data %>%
  dplyr::select(-Date) %>%
  tidyr::pivot_longer(cols = all_of(target_counties), names_to = "County", values_to = "Concentration") %>%
  dplyr::mutate(
    # FIX: Convert NAs to 0 so the cut() function can safely snap them into the (0 - 10k) bucket
    Concentration = ifelse(is.na(Concentration), 0, Concentration),
    
    Category = cut(Concentration,
                   breaks = c(0, 10000, 100000, 1000000, Inf), 
                   labels = c("Below toxic threshold (0 - 10k)",  
                              "Medium Concentration (10k - 100k)", 
                              "High Concentration (100k - 1M)", 
                              "Extreme Concentration (> 1M)"),
                   include.lowest = TRUE),
    County = factor(County, levels = rev(target_counties)) # Forces geographic Northwest to Southeast ordering
  )

# Step C: Construct Custom Hovmöller Matrix and graft IEAnalyzeR styles on top
hovmoller_plot <- ggplot(long_hab, aes(x = TrueDate, y = County, fill = Category)) +
  geom_tile(color = NA) +
  scale_fill_manual(
    values = c("Below toxic threshold (0 - 10k)"  = "#EFEFEF", # Explicitly paints the 0-10k bucket gray
               "Medium Concentration (10k - 100k)" = "#FFD700", 
               "High Concentration (100k - 1M)"    = "#D32F2F", 
               "Extreme Concentration (> 1M)"      = "#8B0000"),
    na.value = "#EFEFEF", # Safety net fallback
    name = "Bloom Severity Levels"
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", expand = c(0,0)) +
  labs(
    title = "Karenia brevis Bloom History by County (2015 - 2025)",
    x = "Timeline",
    y = "Coastal Florida Counties"
  ) +
  iea_style + # Stolen formatting applied directly!
  theme(
    legend.position = "right",                    # Protects color-key from vanishing
    panel.grid.major = element_blank(),           # Wipes out vertical/horizontal line slicing
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

# Display preview within R Session / IDE
print(hovmoller_plot)

# ----------------------------------------------------
# ### 7. Save plot ####

ggsave(
  plot = hovmoller_plot,
  filename = plot_filename, 
  width = 18,          
  height = 8, 
  units = "in", 
  dpi = 300
)

print(paste0("Process complete! Cohesive figure saved cleanly to: ", plot_filename))