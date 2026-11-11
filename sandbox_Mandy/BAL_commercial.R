# ==============================================================================
# Script: multi_panel_base_r_biomass_acl.R
# Purpose: Combined 2x4 multi-panel figure in base R saved to figures folder.
#          Panels 1-7: Species time series with significantly enlarged text.
#          Panel 8: Dedicated unboxed legend.
# ==============================================================================

# 1. Load Data -----------------------------------------------------------------
sero_file    <- "data/intermediate/sero_acl_pull.csv"
biomass_file <- "data/intermediate/combined_biomass_trends.csv"

sero_raw    <- read.csv(sero_file, stringsAsFactors = FALSE)
biomass_raw <- read.csv(biomass_file, stringsAsFactors = FALSE)

# 2. Filter & Clean Commercial ACL Data ----------------------------------------
sero_comm <- sero_raw[tolower(sero_raw$sector) == "commercial", ]
sero_comm$species  <- trimws(sero_comm$spp)
sero_comm$year     <- as.integer(sero_comm$year)

# Convert Landings and ACL to millions of pounds
sero_comm$landings <- as.numeric(sero_comm$total_landings_lbs) / 1e6
sero_comm$acl      <- as.numeric(sero_comm$acl_lbs) / 1e6
sero_comm          <- sero_comm[, c("species", "year", "landings", "acl")]

# 3. Filter & Clean Biomass Data -----------------------------------------------
# Select indicator (year) and columns strictly ending in "Biomass" (ignoring .1, .2)
biomass_cols  <- grep("(?i)Biomass$", names(biomass_raw), value = TRUE)
biomass_clean <- biomass_raw[, c("indicator", biomass_cols)]
names(biomass_clean)[names(biomass_clean) == "indicator"] <- "year"
biomass_clean$year <- as.integer(biomass_clean$year)

# Map raw column names to clean species names (dots replaced with spaces)
col_to_species <- function(col_name) {
  spp_part <- sub("_.*$", "", col_name)
  spp_clean <- gsub("\\.", " ", spp_part)
  trimws(spp_clean)
}

# 4. Identify Matching Species -------------------------------------------------
mapped_biomass_spp <- sapply(biomass_cols, col_to_species)
comm_species       <- unique(sero_comm$species)
shared_species     <- sort(intersect(comm_species, mapped_biomass_spp))

cat(sprintf("Plotting %d species across 2 rows and 4 columns with much larger fonts.\n", length(shared_species)))

# 5. Setup Graphic Export (PNG with Global pointsize = 20) ---------------------
dir.create("figures", showWarnings = FALSE, recursive = TRUE)
output_path <- "figures/plots/commercial_biomass_acl_base_r.png"

# Canvas dimensions (24 x 13 inches) with global pointsize = 20 for much larger text
png(output_path, width = 24, height = 11, units = "in", res = 300, pointsize = 20)

# 2 rows x 4 columns layout with widened margins for large typography
par(
  mfrow    = c(2, 4),
  oma      = c(2.5, 2.5, 5.0, 2.5) - 2,   # Outer margins: bottom, left, top, right
  mar      = c(4.0, 6.2, 4.2, 6.2) - 1,   # Panel margins for large axis titles & ticks
  mgp      = c(3.5, 1.1, 0),          # Positioning: title, tick labels, axis line
  cex.lab  = 1.35,                    # Scaled on top of pointsize = 20
  cex.axis = 1.25                     # Scaled tick labels
)

# Colors definition
col_landings <- 1  # black
col_acl      <- "#e41a1c"  # Red
col_biomass  <- "#1E90FF"  # Blue

# 6. Render Panels 1-7 (Species Plots) -----------------------------------------
for (spp in shared_species) {
  s_data <- sero_comm[sero_comm$species == spp, ]
  b_col  <- biomass_cols[mapped_biomass_spp == spp]
  
  b_data <- biomass_clean[, c("year", b_col)]
  names(b_data)[2] <- "biomass"
  
  # Left join (all.x = TRUE) to preserve all recent Landings/ACL years (up to 2024)
  df_sub <- merge(s_data, b_data, by = "year", all.x = TRUE)
  df_sub <- df_sub[order(df_sub$year), ]
  
  if (nrow(df_sub) == 0) next
  
  # Calculate scale factor: mean(Biomass * scale_factor) == mean(ACL)
  mean_acl     <- mean(df_sub$acl, na.rm = TRUE)
  mean_biomass <- mean(df_sub$biomass, na.rm = TRUE)
  scale_factor <- if (is.finite(mean_biomass) && mean_biomass > 0) mean_acl / mean_biomass else 1
  
  df_sub$scaled_biomass <- df_sub$biomass * scale_factor
  
  y_primary_max <- max(c(df_sub$landings, df_sub$acl, df_sub$scaled_biomass), na.rm = TRUE) * 1.15
  y_primary_min <- 0
  
  # Integer years only
  x_years <- df_sub$year
  x_ticks <- seq(min(x_years), max(x_years), by = max(1, ceiling(length(x_years) / 5)))
  
  # --- Primary Plot: Landings & ACL ---
  plot(
    x = df_sub$year,
    y = df_sub$landings,
    type = "n",
    xlim = c(min(x_years), max(x_years)),
    ylim = c(y_primary_min, y_primary_max),
    axes = FALSE,
    xlab = "",
    ylab = "Landings & ACL (million lbs)",
    main = spp,
    font.main = 2,
    cex.main  = 1.55                  # Extra-large panel heading
  )
  
  grid(nx = NULL, ny = NULL, col = "gray90", lty = "dotted")
  
  # X-axis with integer years only
  axis(side = 1, at = x_ticks, labels = as.character(x_ticks), cex.axis = 1.25)
  
  # Left Y-axis (Landings & ACL in million lbs)
  y_ticks_left <- axTicks(2)
  axis(side = 2, at = y_ticks_left, labels = format(y_ticks_left, trim = TRUE), las = 1, cex.axis = 1.25)
  
  # Solid Green Landings
  lines(df_sub$year, df_sub$landings, col = col_landings, lwd = 3.2, lty = 1)
#  points(df_sub$year, df_sub$landings, col = col_landings, pch = 16, cex = 1.4)
  
  # Solid Red ACL
  lines(df_sub$year, df_sub$acl, col = col_acl, lwd = 3.2, lty = 1)
#  points(df_sub$year, df_sub$acl, col = col_acl, pch = 17, cex = 1.4)
  
  # --- Secondary Plot: Stock Biomass ---
  par(new = TRUE)
  
  plot(
    x = df_sub$year,
    y = df_sub$scaled_biomass,
    type = "n",
    xlim = c(min(x_years), max(x_years)),
    ylim = c(y_primary_min, y_primary_max),
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  
  # Solid Blue Biomass
  lines(df_sub$year, df_sub$scaled_biomass, col = col_biomass, lwd = 3.2, lty = 1)
#  points(df_sub$year, df_sub$scaled_biomass, col = col_biomass, pch = 15, cex = 1.4)
  
  # Right Y-axis (Blue, back-transformed units in million lbs)
  max_biomass_range <- y_primary_max / scale_factor
  raw_right_ticks   <- pretty(c(0, max_biomass_range), n = 5)
  int_right_ticks   <- raw_right_ticks[raw_right_ticks == floor(raw_right_ticks) & raw_right_ticks <= max_biomass_range]
  
  # Position ticks at the corresponding primary axis coordinate
  at_positions <- int_right_ticks * scale_factor
  
  axis(
    side = 4,
    at = at_positions,
    labels = as.character(as.integer(int_right_ticks)),
    las = 1,
    col.axis = col_biomass,
    col = col_biomass,
    cex.axis = 1.25
  )
  mtext("Stock Biomass (million lbs)", side = 4, line = 3, cex = 0.95, col = col_biomass)
  box()
}

# 7. Render Panel 8 (Dedicated Unboxed Legend Panel) ---------------------------
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")

legend(
  x = "center",
  legend = c("Commercial ACL", "Commercial Landings", "Stock Biomass"),
  col = c(col_acl, col_landings, col_biomass),
  lty = c(1, 1, 1),
  lwd = 5,
  # pch = c(16, 17, 15),
  # pt.cex = 2.0,
  bty = "n",
  cex = 1.4,
  y.intersp = 2.0
)

# 8. Main Outer Title & Close Device -------------------------------------------
mtext(
  "Commercial Catch Limits, Landings & Stock Biomass Trends",
  outer = TRUE,
  side = 3,
  line = 1,
  cex = 1.4,
  font = 2
)

dev.off()
cat(sprintf("Figure saved to: %s\n", output_path))


