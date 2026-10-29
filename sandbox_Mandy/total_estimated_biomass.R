
rm(list = ls())

plot.new()
dev.off()

library(plotTimeSeries)
library(rerddap)

d <- read.csv("../../data/intermediate/combined_biomass_trends.csv")

########## NOTE: NEED TO CHECK UNITS HERE!!!!!!!  

dim(d)
names(d)

which(names(d) == "Red.Snapper_SEDAR74_Biomass")

d <- d[, -which(names(d) == "Red.Snapper_SEDAR74_Biomass")]

datdata <- as.integer(d$indicator)

d1 <- d[, 2:ncol(d)]

matplot(datdata, d1, type = "l")

dpos <- d1

dpos[!is.na(dpos)] <- 1
dpos[is.na(dpos)] <- 0
perc <- data.frame(cbind(datdata, rowSums(dpos)/ncol(dpos)))
perc
cutyr <- perc$datdata[min(which(perc$V2 < 0.9))]
cutyr

dim(d1)
d1 <- d1[which(datdata < cutyr), ]
datdata <- datdata[which(datdata < cutyr)]
dim(d1)
length(datdata)
d1 <- d1[, !is.na(colSums(d1))]
dim(d1)

table(is.na(colSums(d1)))

rowSums(d1)

inddata <- data.frame(rowSums(d1))

# format into indicator object ------------------

labs <- c("Total estimated available biomass", "millions of pounds", NA)

indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

s <- list(labels = indnames, indicators = inddata, datelist = datdata) # , ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"
s

# save and plot ---------------------------------------

plotIndicatorTimeSeries(s)

plotIndicatorTimeSeries(s, coltoplot = 1, trendAnalysis = T, widadj = 1.3, hgtadj = 1.2, outtype = "png")




