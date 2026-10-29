
rm(list = ls())

plot.new()
dev.off()

library(plotTimeSeries)
library(rerddap)

d <- read.csv("assessment year to management.csv")
head(d)

d$gap <- d$Implementation.Year - d$Terminal.Year.of.Data

plot(d$Assessment.Year, d$gap)
#d$Assessment.Year <- factor(d$Assessment.Year, levels = min(d$Assessment.Year):max(d$Assessment.Year))

table(d$Assessment.Year)
mn <- tapply(d$gap, d$Assessment.Year, mean, na.rm = T)
va <- tapply(d$gap, d$Assessment.Year, sd, na.rm = T)
va[is.na(va)] <- 0

names(mn) == names(va)

inddata <- data.frame(mn)
datdata <- as.integer(names(mn))
ulidata <- data.frame(mn + 1.96*va)
llidata <- data.frame(mn - 1.96*va)

# format into indicator object ------------------

labs <- c("Time from terminal assessment year to management implementation", "years", NA)

indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

s <- list(labels = indnames, indicators = inddata, datelist = datdata, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"
s

# save and plot ---------------------------------------

plotIndicatorTimeSeries(s)
                        
plotIndicatorTimeSeries(s, coltoplot = 1, trendAnalysis = T, widadj = 1.22, outtype = "png", hgtadj = 0.8)





