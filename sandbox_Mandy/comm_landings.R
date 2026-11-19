
library(dplyr)
library(here)
library(ggplot2)
library(ROracle)
library(keyring)
library(tidyverse)
library(scales)
library(stringr)

rm(list = ls())

# Load data
landings_github = readRDS("data/intermediate/all_commercial_landings_1950_2025_nonconf_raw.rds")

# Load the GDP deflator dataset and aggregate to annual average deflators
deflator_annual <- read_csv("data/intermediate/gdp_deflator_interpolated.csv") %>%
  mutate(LAND_YEAR = as.numeric(format(as.Date(date), "%Y"))) %>%
  group_by(LAND_YEAR) %>%
  summarise(annual_deflator = mean(gdp_deflator_dollar, na.rm = TRUE), .groups = "drop")

# Define target baseline deflator (2024 dollars)
target_deflator_2024 <- deflator_annual %>%
  filter(LAND_YEAR == 2024) %>%
  pull(annual_deflator)

d <- landings_github %>%
  left_join(deflator_annual, by = "LAND_YEAR") %>%
  mutate(
    # Convert nominal dollars to constant 2024 dollars
    TOTAL_DOLLARS_2024 = TOTAL_DOLLARS * (target_deflator_2024 / annual_deflator),
    total_mil_lbs = TOTAL_LIVE_POUNDS / 1e6,
    total_mil_dollars_2024 = TOTAL_DOLLARS_2024 / 1e6
  )

menlis <- unique(d$COMMON_NAME[grep("MENHADEN", d$COMMON_NAME)])
d <- d[-which(d$COMMON_NAME %in% menlis), ]

tab <- tapply(d$total_mil_dollars_2024, list(d$LAND_YEAR, d$FLAG_FMP_SPP), sum, na.rm = T)
matplot(rownames(tab), tab, type = "l", col = 1:2, lty = 1,
        xlab = "Year", ylab = "total value")
legend("topleft", c("nonmanaged species", "managed species"), col = 1:2, lty = 1)


shrlis <- unique(d$COMMON_NAME[grep("SHRIMP", d$COMMON_NAME)])
d1 <- d[-which(d$COMMON_NAME %in% shrlis), ]

tab1 <- tapply(d1$total_mil_dollars_2024, list(d1$LAND_YEAR, d1$FLAG_FMP_SPP), sum, na.rm = T)
matplot(rownames(tab1), tab1, type = "l", col = 1:2, lty = 1,
        xlab = "Year", ylab = "total value")
legend("topleft", c("nonmanaged species", "managed species"), col = 1:2, lty = 1)

tab <- tab[which(as.numeric(rownames(tab)) >= 1965), ]
tab1 <- tab1[which(as.numeric(rownames(tab1)) >= 1965), ]

rat <- tab[, 2] / rowSums(tab)
rat1 <- tab1[, 2] / rowSums(tab1)

datdata <- as.integer(rownames(tab)) 
inddata <- data.frame(rat, rat1)

labs <- c("Value of FMP species relative to total value of Gulf fisheries", "Proportion of total value", NA, 
          "Value of FMP species relative to total value of Gulf fisheries - excluding shrimp species", "Proportion of total value", NA)
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

s <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(s) <- "indicatordata"
s

# save and plot ---------------------------------------

plotIndicatorTimeSeries(s, coltoplot = 1:2, plotrownum = 2)

plotIndicatorTimeSeries(s, coltoplot = 1:2, plotrownum = 2, trendAnalysis = T, outtype = "png", widadj = 0.7, hgtadj = 0.4)


# gini index -----------------

rm(list = ls())

d <- readRDS("../CONFIDENTIAL/landings_raw_for_Mandy.rds")

head(d)

d <- d[which(d$LAND_YEAR >= 1980), ]

shrlis <- unique(d$COMMON_NAME[grep("SHRIMP", d$COMMON_NAME)])
d <- d[-which(d$COMMON_NAME %in% shrlis), ]

d1 <- d[which(d$FLAG_FMP_SPP == 1), ]
par(mar = c(15, 3, 1, 1))
barplot(sort(tapply(d1$LIVE_POUNDS, d1$COMMON_NAME, sum, na.rm = T), decreasing = T)[1:30], las = 2)

vals <- sort(tapply(d1$LIVE_POUNDS, d1$COMMON_NAME, sum, na.rm = T), decreasing = T)
sum(vals[1:20]) / sum(vals)
sum(vals[1:30]) / sum(vals)  # top 96% of landings

spplis <- names(vals[1:30])

spplis <- spplis[-c(3, 4, 9, 10, 15, 18, 21, 24, 28, 29, 30)]
spplis

tapply(d1$LIVE_POUNDS, d1$COMMON_NAME %in% spplis, sum, na.rm = T) / sum(d1$LIVE_POUNDS, na.rm = T)


calcGini <- function(vec) {
  v <- sort(vec)
  N <- length(vec)
  gini <- sum(sapply(1:N, function(x) ((2 * x - N - 1) * v[x]))) / (N^2 * mean(vec))
  return(gini)
}

fin <- data.frame(unique(d1$LAND_YEAR))
names(fin) <- "year"

for (i in 1:length(spplis)) { 
  ds <- d1[which(d1$COMMON_NAME == spplis[i]), ]
  ds$LAND_YEAR <- factor(ds$LAND_YEAR, levels = unique(d1$LAND_YEAR))
  tab <- tapply(ds$LIVE_POUNDS, list(ds$LAND_YEAR, ds$LAND_MONTH), sum, na.rm = T)
  tab[is.na(tab)] <- 0
  matplot(t(tab), type = "l", main = unique(ds$COMMON_NAME))
  gi <- apply(tab, 1, function(x) calcGini(x))
  fin <- data.frame(cbind(fin, gi))
  names(fin)[i+1] <- unique(ds$COMMON_NAME)
} 

head(fin)
fin <- fin[which(fin$year > 1985), ]
fin
datdata <- as.integer(fin$year)
fin <- fin[, -1]

matplot(datdata, fin, type = "b", pch = 1:10, lty = 1, col = glasbey(20))
legend("topleft", names(fin), lty = 1, col = glasbey(20), pch = 1:10)

table(is.na(fin))
dim(fin)

fin <- fin * (12 / 11)  # small sample size adjustment
colSums(fin)
dim(fin)

ind <-   apply(fin, 1, mean, na.rm = T)
indse <- apply(fin, 1, sd, na.rm = T)

inddata <- data.frame(ind)
ulidata <- data.frame(ind + indse)
llidata <- data.frame(ind - indse)

labs <- c("Distribution of commercial catch across the year", "Concentration across 12 months", NA)

indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

s <- list(labels = indnames, indicators = inddata, datelist = datdata, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"
s

# save and plot ---------------------------------------

plotIndicatorTimeSeries(s)

plotIndicatorTimeSeries(s, coltoplot = 1, trendAnalysis = T, outtype = "png", widadj = 1.2, hgtadj = 1.2)


# revenue 

par(mar = c(15, 3, 1, 1))
barplot(sort(tapply(d1$DOLLARS, d1$COMMON_NAME, sum, na.rm = T), decreasing = T)[1:30], las = 2)

vals <- sort(tapply(d1$DOLLARS, d1$COMMON_NAME, sum, na.rm = T), decreasing = T)
sum(vals[1:20]) / sum(vals)
sum(vals[1:30]) / sum(vals)  # top 96% of landings

spplis <- names(vals[1:24])
spplis

spplis <- spplis[-c(2, 4, 9, 12, 16, 23)]
spplis

tapply(d1$DOLLARS, d1$COMMON_NAME %in% spplis, sum, na.rm = T) / sum(d1$DOLLARS, na.rm = T)

calcGini <- function(vec) {
  v <- sort(vec)
  N <- length(vec)
  gini <- sum(sapply(1:N, function(x) ((2 * x - N - 1) * v[x]))) / (N^2 * mean(vec))
  return(gini)
}

fin <- data.frame(unique(d1$LAND_YEAR))
names(fin) <- "year"

for (i in 1:length(spplis)) { 
  ds <- d1[which(d1$COMMON_NAME == spplis[i]), ]
  ds$LAND_YEAR <- factor(ds$LAND_YEAR, levels = unique(d1$LAND_YEAR))
  tab <- tapply(ds$DOLLARS, list(ds$LAND_YEAR, ds$LAND_MONTH), sum, na.rm = T)
  tab[is.na(tab)] <- 0
  matplot(t(tab), type = "l", main = unique(ds$COMMON_NAME))
  gi <- apply(tab, 1, function(x) calcGini(x))
  fin <- data.frame(cbind(fin, gi))
  names(fin)[i+1] <- unique(ds$COMMON_NAME)
} 

head(fin)
fin <- fin[which(fin$year > 1985), ]
fin
datdata <- as.integer(fin$year)
fin <- fin[, -1]

matplot(datdata, fin, type = "b", pch = 1:10, lty = 1, col = glasbey(20))
legend("topleft", names(fin), lty = 1, col = glasbey(20), pch = 1:10)

table(is.na(fin))
dim(fin)

fin <- fin * (12 / 11)  # small sample size adjustment
colSums(fin)
dim(fin)

ind <-   apply(fin, 1, mean, na.rm = T)
indse <- apply(fin, 1, sd, na.rm = T)

inddata <- data.frame(ind)
ulidata <- data.frame(ind + indse)
llidata <- data.frame(ind - indse)

labs <- c("Distribution of commercial revenue across the year", "Concentration across 12 months", NA)

indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

s <- list(labels = indnames, indicators = inddata, datelist = datdata, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"
s

# save and plot ---------------------------------------

plotIndicatorTimeSeries(s)

plotIndicatorTimeSeries(s, coltoplot = 1, trendAnalysis = T, outtype = "png", widadj = 1.1, hgtadj = 1.2)



