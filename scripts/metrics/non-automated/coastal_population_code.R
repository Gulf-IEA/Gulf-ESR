#### 0. Setup ####
library(IEAnalyzeR)
library(here)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(maps)
library(scales)

# File Naming Setup
root_name <- "coastal-population"

raw_filename    <- here("data/unformatted/Population_CoastalCounties_20102023.csv")
csv_filename    <- here(paste0("data/formatted/formatted_csvs/", root_name, "_formatted.csv"))
object_filename <- here(paste0("data/formatted/final_objects/", root_name, "_object.rds"))
plot_filename   <- here(paste0("figures/plots/", root_name, "_plot.png"))
map_filename    <- here(paste0("figures/plots/", root_name, "_county_change_map.png"))

# Ensure output directories exist
dir.create(here("data/formatted/formatted_csvs"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data/formatted/final_objects"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("figures/plots"), recursive = TRUE, showWarnings = FALSE)

#----------------------------------------------------
#### 1. Read & Filter Gulf-Adjacent Counties ####

raw_pop <- read.csv(raw_filename)

# 1. All coastal counties/parishes in TX, LA, MS, and AL
western_gulf_states <- c("Texas", "Louisiana", "Mississippi", "Alabama")

# 2. Florida Gulf-adjacent counties west/northwest of the Florida Keys
# (Extending from Collier County in southwest FL up through Escambia County)
fl_gulf_counties <- c(
  "Bay", "Charlotte", "Citrus", "Collier", "Dixie", "Escambia",
  "Franklin", "Gulf", "Hernando", "Hillsborough", "Jefferson",
  "Lee", "Levy", "Manatee", "Okaloosa", "Pasco", "Pinellas",
  "Santa Rosa", "Sarasota", "Taylor", "Wakulla", "Walton"
)

# Optional toggle: Monroe County represents the Florida Keys.
# Set to TRUE if your working group includes the Keys in the Gulf boundary.
include_monroe_keys <- FALSE
if (include_monroe_keys) {
  fl_gulf_counties <- c(fl_gulf_counties, "Monroe")
}

# Filter for Gulf-adjacent counties
gulf_pop <- raw_pop |>
  filter(
    STATE %in% western_gulf_states |
      (STATE == "Florida" & str_remove(NAME, " County, Florida") %in% fl_gulf_counties)
  )

message(paste("Total Gulf-adjacent counties identified:", nrow(gulf_pop)))

#----------------------------------------------------
#### 2. Clean Data & Create Gulf-Wide Time Series CSV ####

# Reshape annual columns (2010-2023) to long format
year_cols <- paste0("X", 2010:2023)
# Handle whether read.csv prefixed numbers with 'X'
if (!any(year_cols %in% names(gulf_pop))) {
  year_cols <- as.character(2010:2023)
}

gulf_annual_summary <- gulf_pop |>
  select(all_of(year_cols)) |>
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) |>
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "coastal_population"
  ) |>
  mutate(
    Year = str_remove(Year, "^X")
  ) |>
  arrange(as.numeric(Year))

# Define header metadata for IEAnalyzeR
indicator_names <- "Coastal Population"
unit_names      <- "Total Residents"
extent_names    <- "Gulf-wide"

# Format for IEAnalyzeR
pop_formatted <- IEAnalyzeR::convert_cleaned_data(
  gulf_annual_summary,
  indicator_names = indicator_names,
  unit_names      = unit_names,
  extent_names    = extent_names
)

head(pop_formatted)

#----------------------------------------------------
#### 3. Save Formatted Data as CSV ####

write.csv(pop_formatted, file = csv_filename, row.names = FALSE)

#----------------------------------------------------
#### 4. Create & Save Data_Prep Object ####

pop_obj <- IEAnalyzeR::data_prep(pop_formatted)
saveRDS(pop_obj, file = object_filename)

#----------------------------------------------------
#### 5. Generate & Save Time Series Plot ####

ts_plot <- IEAnalyzeR::plot_fn_obj(
  df_obj = pop_obj,
  trend = TRUE,
  sep_ylabs = TRUE,
  ylab_sublabel = c("extent", "unit")
)

print(ts_plot)
ggsave(filename = plot_filename, plot = ts_plot, width = 8, height = 5, units = "in")

#----------------------------------------------------
#### 6. County-Level Heatmap (Net Population Change) ####

# Calculate net change (2023 minus 2010) per county
col_2010 <- ifelse("X2010" %in% names(gulf_pop), "X2010", "2010")
col_2023 <- ifelse("X2023" %in% names(gulf_pop), "X2023", "2023")

county_change <- gulf_pop |>
  mutate(
    net_change = .data[[col_2023]] - .data[[col_2010]],
    pct_change = (net_change / .data[[col_2010]]) * 100,
    # Standardize subregion strings to match ggplot2::map_data('county')
    region = tolower(STATE),
    subregion = tolower(NAME) |>
      str_remove(" (county|parish),.*$") |>
      str_remove_all("\\.") |>
      str_trim()
  )

# Get US county map geometries for the 5 Gulf states
gulf_map_base <- map_data("county") |>
  filter(region %in% c("texas", "louisiana", "mississippi", "alabama", "florida"))

# Join change metrics onto base spatial geometries
map_data_joined <- left_join(gulf_map_base, county_change, by = c("region", "subregion"))

# Retrieve state boundary borders for context
state_borders <- map_data("state") |>
  filter(region %in% c("texas", "louisiana", "mississippi", "alabama", "florida"))

# Heatmap plot: Diverging gradient centered at 0 net population change
# Non-adjacent inland counties render in light gray
county_map_plot <- ggplot(map_data_joined, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "gray92", color = "white", linewidth = 0.15) +
  geom_polygon(aes(fill = net_change), color = "gray40", linewidth = 0.2) +
  geom_polygon(data = state_borders, fill = NA, color = "gray25", linewidth = 0.6) +
  coord_quickmap(xlim = c(-98, -80), ylim = c(24.5, 31.5)) +
  scale_fill_gradient2(
    name = "Net Population\nChange (2010-2023)",
    low = "#ca0020",      # Red for population loss
    mid = "#ffffbf",      # Warm light yellow for static
    high = "#0571b0",     # Deep blue for large gains
    midpoint = 0,
    na.value = "gray92",
    labels = scales::label_comma(scale_cut = scales::cut_short_scale())
  ) +
  labs(
    title = "Gulf of Mexico Coastal County Population Change",
    subtitle = "Net resident population change from 2010 to 2023 (US Census Bureau)",
    caption = "Inland and non-Gulf coastal counties shown in gray."
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 4)),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30", margin = margin(b = 10)),
    plot.caption = element_text(hjust = 0.5, color = "gray50", size = 8, margin = margin(t = 8)),
    legend.position = "bottom",
    legend.key.width = unit(1.8, "cm"),
    legend.key.height = unit(0.35, "cm"),
    legend.title = element_text(vjust = 0.85, size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    panel.background = element_rect(fill = "aliceblue", color = NA) # Ocean tint
  )

print(county_map_plot)

# Save the county heatmap
ggsave(
  filename = map_filename,
  plot = county_map_plot,
  width = 10,
  height = 5.5,
  units = "in",
  dpi = 300
)