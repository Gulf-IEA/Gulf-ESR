
rm(list = ls())

plot.new()
dev.off()

library(plotTimeSeries)
library(rerddap)

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
matplot(datdata, d1, type = "b", lwd = 1, lty = 1, pch = 1:numspp, 
        col = glasbey(numspp))
legend("topleft", ncol = 2, lwd = 1, lty = 1, pch = 1:20, col = glasbey(numspp), sppnames, bty = "n")

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

# save and plot ---------------------------------------

plotIndicatorTimeSeries(s, coltoplot = 1:2, plotrownum = 2)

plotIndicatorTimeSeries(s, coltoplot = 1:2, plotrownum = 1, trendAnalysis = T, widadj = 0.7, hgtadj = 0.6, outtype = "png")




