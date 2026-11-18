
rm(list = ls())

plot.new()
dev.off()

library(plotTimeSeries)
library(pals)
library(zoo)
library(tidyverse)

df <- read.csv("../../data/intermediate/combined_biomass_trends.csv")

########## NOTE: NEED TO CHECK UNITS HERE!!!!!!!  

dim(df)
names(df)

datdata <- as.integer(df$indicator)

d1 <- df[, 2:ncol(df)]

matplot(datdata, d1, type = "l")

d1[, ] <- na.locf(d1[, ], na.rm = FALSE)

matplot(datdata, d1, type = "l")

# dpos <- d1
# dpos[!is.na(dpos)] <- 1
# dpos[is.na(dpos)] <- 0
# perc <- data.frame(cbind(datdata, rowSums(dpos)/ncol(dpos)))
# perc
# cutyr <- perc$datdata[min(which(perc$V2 < 0.9))]
# cutyr
# dim(d1)
# d1 <- d1[which(datdata < cutyr), ]
# datdata <- datdata[which(datdata < cutyr)]
# dim(d1)
# length(datdata)
# d1 <- d1[, !is.na(colSums(d1))]
# dim(d1)

table(is.na(colSums(d1)))

rowSums(d1)

cols_to_remove <- grep("Red.Snap", names(d1))
d2 <- d1[, -cols_to_remove]
cols_to_remove <- grep("Vermilion", names(d2))
d2 <- d2[, -cols_to_remove]

rowSums(d2)

sppnames <- spp <- sub("_.*$", "", colnames(d1))
numspp <- length(sppnames)
cols <- as.numeric(as.factor(sppnames))
matplot(datdata, d1, type = "b", lwd = 1, lty = 1, pch = 1:numspp, 
        col = glasbey()[cols])
legend("topleft", ncol = 2, lwd = 1, lty = 1, pch = 1:20, col = glasbey()[cols], sppnames, bty = "n")

sppnames <- spp <- sub("_.*$", "", colnames(d2))
numspp <- length(sppnames)
matplot(datdata, d2, type = "b", lwd = 1, lty = 1, pch = 1:numspp, 
        col = glasbey(numspp))
legend("topright", ncol = 2, lwd = 1, lty = 1, pch = 1:20, col = glasbey(numspp), sppnames, bty = "n")


# format into indicator object ------------------

inddata <- data.frame(rowSums(d1), rowSums(d2))

labs <- c("Total estimated available biomass of assessed species", "millions of pounds", NA, 
          "Total estimated biomass (excl. red and vermilion snapper)", "millions of pounds", NA)

indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

s <- list(labels = indnames, indicators = inddata, datelist = datdata) # , ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"
s


###########
years <- datdata

d2 <- t(d1)
d2 <- d2[order(rowSums(d2)), ]
colnames(d2) <- years

# 2. Convert d2 safely (handles both matrices and data frames, even with duplicate rownames)
df_clean <- tibble(raw_name = rownames(d2)) %>%
  bind_cols(as_tibble(d2, .name_repair = "minimal")) %>%
  setNames(c("raw_name", as.character(years))) %>%
  
  # 3. Clean species names
  mutate(
    Species = raw_name %>%
      str_remove("_SEDAR.*") %>%
      str_replace_all("\\.", " ") %>%
      str_to_sentence()
  ) %>%
  
  # 4. Reshape to long format
  pivot_longer(
    cols = -c(raw_name, Species),
    names_to = "Year",
    values_to = "Biomass"
  ) %>%
  mutate(
    Year = as.numeric(Year),
    Biomass = as.numeric(Biomass)
  ) %>%
  
  # 5. Sum duplicate species per year and sort descending
  group_by(Year, Species) %>%
  summarise(Biomass = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
  mutate(Species = fct_reorder(Species, Biomass, .fun = sum, .desc = TRUE))

# 6. Generate Glasbey colors
glasbey_colors <- pals::glasbey(n_distinct(df_clean$Species))

# 7. Plot
ggplot(df_clean, aes(x = Year, y = Biomass, fill = Species)) +
  geom_col(position = "stack", width = 0.75) +
  scale_fill_manual(values = glasbey_colors) +
  labs(
    x = "",
    y = "Biomass (millions of pounds)",
    fill = "Species"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    legend.position = "right"
  )



# save and plot ---------------------------------------

plotIndicatorTimeSeries(s, coltoplot = 1:2, plotrownum = 2)

plotIndicatorTimeSeries(s, coltoplot = 1:2, plotrownum = 2, trendAnalysis = T, widadj = 0.9, hgtadj = 0.6, outtype = "png")




