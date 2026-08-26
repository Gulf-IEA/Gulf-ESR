

library(ROracle)

rm(list = ls())

# connect to Oracle
Sys.setenv(TZ = "UTC", ORA_SDTZ = "UTC")

con = dbConnect(dbDriver("Oracle"), username = keyring::key_list()[1,2],
                password = keyring::key_get("mandy.karnauskas@noaa.gov", keyring::key_list()[1,2]),
                dbname = "SECPR")

# pull data
acls = dbGetQuery(con, "SELECT * FROM kdettloff.wh_acl_totals WHERE region IN ('GULF OF MEXICO', 'GOM & SA')")

dbDisconnect(con)

head(acls)

table(acls$REGION)
acls$QUOTA_NAME
unique(acls$QUOTA_NAME)

nam <- unlist(strsplit(acls$QUOTA_NAME, " ACL"))
acls$spp <- nam[seq(1, nrow(acls)*2, 2)]

acls$year <- substr(acls$SEASON_END, 1, 4)
acls$seasoff <- substr(acls$SEASON_START, 1, 4) == substr(acls$SEASON_END, 1, 4)
unique(acls$spp[which(acls$seasoff == "FALSE")])

par(mar = c(12, 5, 1, 1))
#barplot(sort(tapply(acls$ACL, acls$spp, mean, na.rm = T)), las = 2)

acls$REGION[which(acls$spp == "SPANISH MACKEREL")] <- "GULF OF MEXICO"

acls$QUOTA_NAME[grep("KING MACKEREL", acls$QUOTA_NAME)]
acls$QUOTA_NAME[grep("KING MACKEREL ACL SA", acls$QUOTA_NAME)]
acls <- acls[-grep("KING MACKEREL ACL SA", acls$QUOTA_NAME),]
acls$QUOTA_NAME[grep("KING MACKEREL", acls$QUOTA_NAME)]

acls$REGION[which(acls$spp == "KING MACKEREL")] <- "GULF OF MEXICO"

acls <- acls[which(acls$REGION == "GULF OF MEXICO"), ]
unique(acls$spp)

par(mar = c(10, 5, 1, 1))
barplot(tapply(acls$VESSELS, acls$spp, mean, na.rm = T), las = 2)
barplot(tapply(acls$DEALERS, acls$spp, mean, na.rm = T), las = 2)

plot(acls$year, acls$VESSELS, col = as.numeric(as.factor(acls$spp)), pch = 19)
plot(acls$year, acls$DEALERS, col = as.numeric(as.factor(acls$spp)), pch = 19)

boxplot(acls$VESSELS ~ acls$year)
boxplot(acls$DEALERS ~ acls$year)

king <- acls[which(acls$spp == "KING MACKEREL"), ]
totland <- tapply(king$TOTAL_LANDINGS, king$year, sum, na.rm = T)
totacl <- tapply(king$ACL, king$year, sum, na.rm = T)

grep("KING MACKEREL ACL GOM WESTERN ZONE", king$QUOTA_NAME)
king <- king[grep("KING MACKEREL ACL GOM WESTERN ZONE", king$QUOTA_NAME), ]
king
king$TOTAL_LANDINGS <- totland
king$ACL <- totacl

acls <- acls[-which(acls$spp == "KING MACKEREL"), ]
acls <- rbind(acls, king)

unique(acls$spp)

par(mfrow = c(3, 6), mar = c(3, 4, 1, 1))


url <- "https://www.fisheries.noaa.gov/southeast/commercial-fishing/gulf-america-historical-commercial-landings-and-annual-catch-limit"

webpage <- read_html(url) 

# Select the table using CSS selector 
table_head <- html_nodes(webpage, "h2") 
table_node <- html_nodes(webpage, "table") 

labs <- table_head[2:12]

colist <- c("spp", "Year", "Total.Reported", "Total.Reported.", "ACL", "ACL.", "ACL..")
dall <- data.frame()

for (i in 1:length(table_node))  { 
  spp <- strsplit(substr(as.character(labs[i]), 5, 50), "<")[[1]][1]
  spp <- strsplit(spp, "\\(")[[1]][1]
  tab <- html_table(table_node)[[i]] 
  if (i == 10)  { tab <- tab %>% row_to_names(row_number = 2)  } else {
    tab <- tab %>% row_to_names(row_number = 1)  }
  d1 <- data.frame(tab)
  d1$Year <- substr(d1$Year, 1, 4)
  d1 <- d1[which(!is.na(as.numeric(d1$Year))), ]
  d1$spp <- spp
  cols <- unlist(sapply(colist, function(x) which(x == names(d1))))
  d2 <- d1[cols]
  names(d2)[3] <- "Total.Landings"
  names(d2)[5] <- "ACL."
  d2$Total.Landings <- as.numeric(gsub(",", "", d2$Total.Landings))
  d2$ACL <- as.numeric(gsub(",", "", d2$ACL))
  d2
  dall <- rbind(dall, d2)
}

dall$spp <- gsub("[*]", "", dall$spp)

dat <- dall
dat$Year <- as.numeric(dat$Year)
dat
dat$spp <- substr(dat$spp, 1, (nchar(dat$spp) - 1))

dat1 <- dat[which(dat$Year <= 2011), ]
dat1 <- dat1[which(!is.na(dat1$ACL)), ]
dat <- dat[which(dat$Year > 2011), ]

lis <- c("Gag Grouper", "Red Grouper", "Deep Water Grouper", "Shallow Waters Grouper", "Red Snapper")

dat <- dat[dat$spp %in% lis, ]
head(dat)
head(dat1)
dat <- rbind(dat, dat1)

d <- data.frame(cbind(acls$spp, acls$year, acls$TOTAL_LANDINGS, acls$ACL, acls$ACL_PCT), stringsAsFactors = FALSE)

head(d)
head(dat)

names(d) <- names(dat)

dat <- rbind(dat, d)
dat <- as.data.frame(lapply(dat, type.convert, as.is = TRUE))

dat <- dat[-which(dat$spp == "ROYAL RED SHRIMP"), ]
dat <- dat[-which(dat$spp == "GAG GROUPER"), ]
unique(dat$spp)

dat <- dat[order(dat$spp, dat$Year), ]
#write.csv(dat, file = "comm_ACLs_and_landings.csv", row.names = F)

dat$spp <- str_to_title(dat$spp)
splis <- names(sort(tapply(dat$ACL, dat$spp, mean, na.rm = T), decreasing = T))
splis

dat <- dat[-which(dat$spp == "Shallow Waters Grouper" & dat$Year == 2009), ]

par(mfrow = c(5, 4), mar = c(3, 4, 1, 1))

for (i in splis)  {
  d <- dat[which(dat$spp == i), ]
  plot(d$Year, d$ACL/10^6, type = "l", col = 2, main = i, ylim = c(0, max(d$ACL/10^6)), 
       xlab = "", ylab = "millions of pounds")
  lines(d$Year, d$Total.Landings/10^6)
  legend("bottom", c("ACL", "landings"), lty = 1, col = c(2, 1), bty= "n")
}

dev.off()

cols <- glasbey(length(splis))
ltys <- rep(1, length(splis))

plot(dat$Year, dat$ACL., col = 0, ylim = c(0, 150), xlab = "", ylab = "Usage of annual catch limit (percent)", 
     las = 2, main = "Percent of commercial annual catch limit used\n for federally managed species in the Gulf")
abline(h = 100, col = 8, lty = 2)

for (i in 1:length(splis))  { 
  ds <- dat[which(dat$spp == splis[i]), ]
  points(ds$Year, (ds$ACL.), col = transparent(cols[i], trans.val = 0.5), pch = 20, cex = 1.5)
  out <- lm(ds$ACL. ~ ds$Year)
  if (coef(out)[2] > 0) { lt <- 2 } else { lt <- 1 }
  abline(out, col = cols[i], lwd = 2, lty = lt)
  print(splis[i])
  print((max(predict(out)) - min(predict(out)))/max(predict(out)))
  print(out$coefficients[2])
  if (out$coefficients[2] > 0) { ltys[i] <- 2 }
#  lines(ds$Year, (ds$ACL.), col = transparent(cols[i], trans.val = 0.7))
}
legend("topright", splis, col = cols, lty = ltys, lwd = 2, ncol = 2, bty = "n", cex = 0.8)


biom <- readRDS("C:/Users/mandy.karnauskas/Desktop/Gulf-ESR/indicator_data/stock assessment output plots and data/combined_biomass_trends.rds")

biom$`Red Snapper_SEDAR52`<- biom$`Red Snapper_Area1_SEDAR52` + biom$`Red Snapper_Area2_SEDAR52`
biom$`Vermilion Snapper_SEDAR67` <- biom$`Vermillion Snapper_Area1_SEDAR67` + biom$`Vermillion Snapper_Area2_SEDAR67`

biom <- biom[-grep("Red Snapper_Area", colnames(biom))]
biom <- biom[-grep("Vermillion Snapper_Area", colnames(biom))]
biom

colnams <- unlist(strsplit(names(biom), "_"))[seq(2, ncol(biom)*2-1, 2)]
colnames(biom) <- tolower(c("year", colnams))
head(biom)

names(biom)[which(names(biom) == "scamp grouper")] <- "Shallow Waters Grouper"


splis <- splis[1:19]

par(mfrow = c(5, 4), mar = c(3, 4, 1, 4), mgp=c(3,1,0))

for (i in splis)  {
  d <- dat[which(dat$spp == i), ]
  plot(d$Year, d$ACL/10^6, type = "l", col = 2, main = str_to_title(i), ylim = c(0, max(d$ACL/10^6)*1.1), 
       xlab = "", ylab = "", las = 2)
  points(d$Year, d$ACL/10^6, pch = 19, col = 2)
  lines(d$Year, d$Total.Landings/10^6)
  points(d$Year, d$Total.Landings/10^6, pch = 19)

  a <- which(names(biom) == tolower(i))
  if (length(a) > 0) { 
    b <- biom[which(biom$year %in% d$Year), a] * 2204.62 / 10^6
    b <- c(b, rep(NA, length(d$Year)-length(b)))
    if (sum(b, na.rm = T) > 0)  {
    sc <- max(b, na.rm = T) / max(d$ACL/10^6) 
    b1 <- b / sc
    lines(d$Year, b1, col = 4)
    points(d$Year, b1, col = 4, pch = 19)
    f <- c(0.1, 0.5, 1, 5, 10, 25, 50)
    f1 <- max(f[which(round(max(b, na.rm = T))/5 - f > 0)])
    axis(4, at = seq(0, round(max(b, na.rm = T)/f1)*f1, f1)/sc, 
          lab = seq(0, round(max(b, na.rm = T)/f1)*f1, f1), las = 2, col = 4)
    mtext("stock biomass", side = 4, line = 2, cex = 0.6, col = 4)
    }
  }
  mtext("ACL and landings", side = 2, line = 3, cex = 0.6)
}

plot.new()
legend("bottom", c("ACL", "landings", "biomass"), lty = 1, lwd = 2, pch = 19, col = c(2, 1, 4), 
       title = "(in millions of pounds)", cex = 1.2, bty = "n")




