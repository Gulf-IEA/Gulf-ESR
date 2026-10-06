# -*- coding: utf-8 -*-
# File created on 2026-08-06

# ### 0. Setup ####
library(here)
library(ggplot2)
library(reticulate)
library(sf)
library(rnaturalearth)

# File Naming Setup.
# !! Auto generated-Do Not Change !!
root_name <- "Bottom_Tmap"

csv_filename <- here(paste0("data/formatted/formatted_csvs/", root_name, "_formatted.csv"))
object_filename <- here(paste0("data/formatted/final_objects/", root_name, "_object.rds"))
plot_filename <- here(paste0("figures/plots/", root_name, "_plot.png"))

# ----------------------------------------------------
# ### 0. Process data in Python ####

py_run_string(R"(
import os
import numpy as np
import xarray as xr
import pandas as pd

# ==========================================
# CONFIGURATION
# ==========================================

gebco_file = 'data/intermediate/gebco_2026.nc'
map_extent = [-99, -80, 18, 31]

depth_min = -500
depth_max = -70

sectors = {
    "West": {
        "lon_min": -97.5, "lon_max": -89.0, "lat_min": 26.0, "color": "#e63946"
    },
    "Central": {
        "lon_min": -89.0, "lon_max": -85.3, "lat_min": 27.0, "color": "#457b9d"
    },
    "WFS": {
        "lon_min": -85.3, "lon_max": -83.0, "lat_min": 24.5, "color": "#2a9d8f"
    }
}

# ==========================================
# LOAD & PROCESS GEBCO
# ==========================================

print("Loading bathymetry...")
gebco = xr.open_dataset(gebco_file)

# Standardize coordinate naming
if "longitude" in gebco.coords:
    gebco = gebco.rename({"longitude": "lon", "latitude": "lat"})

# Crop to bounding box
gebco_sub = gebco.sel(
    lon=slice(map_extent[0], map_extent[1]),
    lat=slice(map_extent[2], map_extent[3])
)

# Coarsen resolution slightly to optimize memory transfer to R
gebco_sub = gebco_sub.coarsen(lon=4, lat=4, boundary="trim").mean()

depth = gebco_sub["elevation"]
lon = depth.lon.values
lat = depth.lat.values
lon_2d, lat_2d = np.meshgrid(lon, lat)
depth_values = depth.values

# Convert bathymetry grid to DataFrame for ggplot contouring
bathy_df = depth.to_dataframe().reset_index()

# Mask sectors within depth corridor
sector_records = []

for name, cfg in sectors.items():
    mask = (
        (lat_2d >= cfg["lat_min"]) &
        (lon_2d >= cfg["lon_min"]) &
        (lon_2d <= cfg["lon_max"]) &
        (depth_values >= depth_min) &
        (depth_values <= depth_max)
    )
    
    for x, y, z in zip(lon_2d[mask], lat_2d[mask], depth_values[mask]):
        sector_records.append({
            "lon": x,
            "lat": y,
            "elevation": z,
            "sector": name,
            "color": cfg["color"]
        })

sectors_df = pd.DataFrame(sector_records)
print("Processing complete.")
)")

# ----------------------------------------------------
# ### 1. Read Spatial Objects from Python ####

bathy_data <- py$bathy_df
sector_data <- py$sectors_df

# Explicitly set factor levels to prevent alphabetical reordering
sector_data$sector <- factor(
  sector_data$sector, 
  levels = c("West", "Central", "WFS")
)

# Load land boundaries for basemap
world_land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# ----------------------------------------------------
# ### 2. Clean Data & Structure RDS Object ####

contour_levels <- c(-2000, -1000, -500, -300, -200, -70, -50)

sector_colors <- c(
  "West"    = "#e63946",
  "Central" = "#457b9d",
  "WFS"     = "#2a9d8f"
)

sector_labels <- c(
  "West"    = "West Sector",
  "Central" = "Central Sector",
  "WFS"     = "WFS Sector"
)

# Bundle spatial layers into a list object for RDS export
map_spatial_obj <- list(
  bathymetry_grid = bathy_data,
  sector_points   = sector_data,
  levels          = contour_levels,
  extent          = c(-99, -80, 18, 31)
)

# ----------------------------------------------------
# ### 3. Save Formatted Data & RDS Object ####

write.csv(sector_data, file = csv_filename, row.names = FALSE)
saveRDS(map_spatial_obj, file = object_filename)

# ----------------------------------------------------
# ### 4. Create Map (R ggplot2) ####

p <- ggplot() +
  # 1. Sector Corridor Masking
  geom_tile(
    data = sector_data,
    aes(x = lon, y = lat, fill = sector),
    alpha = 0.5
  ) +
  scale_fill_manual(
    name   = "Sector",
    values = sector_colors,
    labels = sector_labels
  ) +
  # 2. Bathymetry Contour Lines
  geom_contour(
    data = bathy_data,
    aes(x = lon, y = lat, z = elevation),
    breaks = contour_levels,
    color = "gray40",
    linewidth = 0.3
  ) +
  # 3. Base Land Overlay
  geom_sf(data = world_land, fill = "lightgray", color = "gray30", inherit.aes = FALSE) +
  # 4. Map Projections and Limits
  coord_sf(
    xlim = c(-99, -80),
    ylim = c(18, 31),
    expand = FALSE
  ) +
  # 5. Formatting & Styling
  labs(
    title = "Gulf of Mexico Regional Upwelling Sectors",
    subtitle = "500–70 m depth corridor | Sector-specific latitude limits",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = c(0.15, 0.2),
    legend.background = element_rect(fill = alpha("white", 0.8), color = "black", linewidth = 0.3),
    panel.grid.major = element_line(color = "gray80", linetype = "dashed", linewidth = 0.4)
  )

print(p)

# ----------------------------------------------------
# ### 5. Save Plot ####

ggsave(
  filename = plot_filename,
  plot = p,
  width = 10,
  height = 7,
  units = "in",
  dpi = 300
)
