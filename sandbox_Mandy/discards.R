
setwd("../../data/intermediate/")
library(zoo)
library(pals)


#########  commercial analysis ##############

rm(list = ls())

# 1. Read files
discards <- read.csv("combined_commercial_discard_trends.csv", check.names = FALSE)
landings <- read.csv("combined_commercial_landings_trends.csv", check.names = FALSE)

# 2. Identify and keep common columns
common_cols <- intersect(colnames(discards), colnames(landings))
discards_sub <- discards[, common_cols]
landings_sub <- landings[, common_cols]

# Order chronologically by year
discards_sub <- discards_sub[order(discards_sub$year), ]
landings_sub <- landings_sub[order(landings_sub$year), ]

species_cols <- setdiff(common_cols, "year")
sppnames <- spp <- sub("_.*$", "", species_cols)
numspp <- length(sppnames)

# 3. Impute NAs using last observation carried forward (LOCF)
discards_sub[, species_cols] <- na.locf(discards_sub[, species_cols], na.rm = FALSE)
landings_sub[, species_cols] <- na.locf(landings_sub[, species_cols], na.rm = FALSE)

matplot(discards_sub$year, discards_sub[, species_cols], type = "b", lwd = 1, lty = 1, pch = 1:12, 
        col = glasbey(numspp), xlab = "year", ylab = "thousands of lbs", main = "commercial discards by species")
legend("topright", lwd = 1, lty = 1, pch = 1:numspp, col = glasbey(numspp), sppnames, bty = "n")

matplot(landings_sub$year, landings_sub[, species_cols], type = "b", lwd = 1, lty = 1, pch = 1:12, 
        col = glasbey(numspp), xlab = "year", ylab = "thousands of lbs", main = "commercial landings by species")
legend("topleft", ncol = 2, lwd = 1, lty = 1, pch = 1:numspp, col = glasbey(numspp), sppnames, bty = "n")

# 4. Sum across species rows
total_discards <- rowSums(discards_sub[, species_cols])/ 1000
total_landings <- rowSums(landings_sub[, species_cols])/ 1000
ldratio <- total_landings / total_discards

discards_sub$year == landings_sub$year

# format into indicator object ------------------

datdata <- as.integer(landings_sub$year)

inddata <- data.frame(total_landings, total_discards, ldratio)

labs <- c("Total commercial landings", "millions of pounds", NA, 
          "Total commercial dead discards", "millions of pounds", NA, 
          "Commercial landings:discards", "ratio", NA)

indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

s <- list(labels = indnames, indicators = inddata, datelist = datdata) # , ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"
s

# save and plot ---------------------------------------

plotIndicatorTimeSeries(s, coltoplot = 1:3, plotrownum = 3, trendAnalysis = T)

setwd("../../figures/plots/")
plotIndicatorTimeSeries(s, coltoplot = 1:3, plotcolnum = 3, trendAnalysis = T, widadj = 1.25, outtype = "png")



#############  recreational analysis  ##################

rm(list = ls())
setwd("../../data/intermediate/")

# 1. Read files
discards <- read.csv("combined_recreational_discard_trends.csv", check.names = FALSE)
landings <- read.csv("combined_recreational_landings_trends.csv", check.names = FALSE)

# 2. Identify and keep common columns
common_cols <- intersect(colnames(discards), colnames(landings))
discards_sub <- discards[, common_cols]
landings_sub <- landings[, common_cols]

# Order chronologically by year
discards_sub <- discards_sub[order(discards_sub$year), ]
landings_sub <- landings_sub[order(landings_sub$year), ]

species_cols <- setdiff(common_cols, "year")
sppnames <- spp <- sub("_.*$", "", species_cols)
numspp <- length(sppnames)

# 3. Impute NAs using last observation carried forward (LOCF)
discards_sub[, species_cols] <- na.locf(discards_sub[, species_cols], na.rm = FALSE)
landings_sub[, species_cols] <- na.locf(landings_sub[, species_cols], na.rm = FALSE)

matplot(discards_sub$year, discards_sub[, species_cols], type = "b", lwd = 1, lty = 1, pch = 1:12, 
        col = glasbey(numspp), xlab = "year", ylab = "thousands of lbs", main = "recreational dscards by species")
legend("topleft", lwd = 1, lty = 1, pch = 1:numspp, col = glasbey(numspp), sppnames, bty = "n")

matplot(landings_sub$year, landings_sub[, species_cols], type = "b", lwd = 1, lty = 1, pch = 1:12, 
        col = glasbey(numspp), xlab = "year", ylab = "thousands of lbs", main = "recreational landings by species")
legend("topleft", ncol = 2, lwd = 1, lty = 1, pch = 1:numspp, col = glasbey(numspp), sppnames, bty = "n")

# 4. Sum across species rows
total_discards <- rowSums(discards_sub[, species_cols])/1000
total_landings <- rowSums(landings_sub[, species_cols])/1000
ldratio <- total_landings / total_discards

discards_sub$year == landings_sub$year

# format into indicator object ------------------

datdata <- as.integer(landings_sub$year)

inddata <- data.frame(total_landings, total_discards, ldratio)

labs <- c("Total recreational landings", "millions of fish", NA, 
          "Total recreational dead discards", "millions of fish", NA, 
          "Recreational landings:discards", "ratio", NA)

indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

s <- list(labels = indnames, indicators = inddata, datelist = datdata) # , ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"
s

# save and plot ---------------------------------------

plotIndicatorTimeSeries(s, coltoplot = 1:3, plotrownum = 3, trendAnalysis = T)

setwd("../../figures/plots/")
plotIndicatorTimeSeries(s, coltoplot = 1:3, plotcolnum = 3, trendAnalysis = T, widadj = 1.25, outtype = "png")




