
rm(list = ls())

# Load data
sero_file    <- "data/intermediate/sero_acl_pull.csv"
d    <- read.csv(sero_file, stringsAsFactors = FALSE)

d$over <- 0
d$over[which(d$acl_percent > 100)] <- 1
d[which(d$acl_percent > 1000), ] <- 0

d <- d[which(d$sector != 0), ]

d[which(d$over == 1 & d$sector == "commercial"), ]
d[which(d$over == 1 & d$sector == "recreational"), ]
d[which(d$over == 1 & d$sector == "total stock"), ]

tab <- tapply(d$over, list(d$year, d$sector), sum, na.rm = T)
tab

tab <- tab[as.numeric(rownames(tab)) >= 2010, ]

matplot(rownames(tab), tab, type = "b")

datdata <- as.integer(rownames(tab))
inddata <- data.frame(tab)

labs <- c("Number of ACL overages", "Commercial", NA, 
          "Number of ACL overages", "Recreational", NA,
          "Number of ACL overages", "Combined ACL", NA)

indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

s <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(s) <- "indicatordata"
s

# save and plot ---------------------------------------

plotIndicatorTimeSeries(s, coltoplot = 1:3, plotrownum = 2, sameYscale = T)

plotIndicatorTimeSeries(s, coltoplot = 1:3, plotrownum = 2, trendAnalysis = T, 
                        sameYscale = T, outtype = "png", widadj = 1.2, hgtadj = 0.9)

