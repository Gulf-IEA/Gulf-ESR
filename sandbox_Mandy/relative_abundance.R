
rm(list = ls())

# 1. Read and clean the multi-row header CSV
df <- read.csv("../../data/formatted/formatted_csvs/reef_fish_abundance_formatted.csv", skip = 2)

colMeans(df, na.rm = T)
round(colMeans(df, na.rm = T), 1)

cols <- setdiff(names(df), "extent")
df[cols] <- lapply(df[cols], function(x) x / mean(x, na.rm = TRUE))
colMeans(df, na.rm = T)

matplot(df$extent, df[, -1], type = "b", pch = 19, xlab = "", ylab = "standardized indices", col = glasbey(17))
matplot(df$extent, df[, -1], type = "b", pch = 19, xlab = "", ylab = "standardized indices", col = glasbey(17), xlim = c(2010, 2023))

df1 <- df
df1[, ] <- na.locf(df1[, ], na.rm = FALSE)

matplot(df1$extent, df1[, -1], type = "b", pch = 19, xlab = "", ylab = "standardized indices", col = glasbey(17))
matplot(df1$extent, df1[, -1], type = "b", pch = 19, xlab = "", ylab = "standardized indices", col = glasbey(17), xlim = c(2010, 2023))

rowSums(df1[, -1])

plot(df$extent, rowMeans(df[, -1], na.rm = T), type = "b")
lines(df1$extent, rowMeans(df1[, -1]), col = 2)

df1 <- df1[which(!is.na(rowSums(df1[, -1]))), ]

matplot(df1$extent, df1[, -1], type = "b", pch = 19)

plot(df$extent, rowMeans(df[, -1], na.rm = T), type = "b")

tot <- rowMeans(df1[, -1])
totse <- apply(df1[, -1], 1, sd)

plot(df1$extent, tot, type = "b")

# format indicator ------------------------

inddata <- data.frame(tot)
llidata <- data.frame(tot - totse)
ulidata <- data.frame(tot + totse)
datdata <- as.integer(df1$extent)

labs <- c("Average of standardized indices", "relative abundance", NA)
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

s <- list(labels = indnames, indicators = inddata, datelist = datdata, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"
s

# save and plot ---------------------------------------

plotIndicatorTimeSeries(s)
plotIndicatorTimeSeries(s, trendAnalysis = T, widadj = 1.4, hgtadj = 1.3, outtype = "png")
