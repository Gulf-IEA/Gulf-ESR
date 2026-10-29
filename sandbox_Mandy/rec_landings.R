
rm(list = ls())

library(ROracle)
library(keyring)

#key_set(
#  service = "mandy.karnauskas@noaa.gov",
#  username = "username") # Replace with your actual Oracle username
#key_list()


calcGini <- function(vec) {
  v <- sort(vec)
  N <- length(vec)
  gini <- sum(sapply(1:N, function(x) ((2 * x - N - 1) * v[x]))) / (N^2 * mean(vec))
  return(gini)
}

#con = dbConnect(dbDriver("Oracle"), username = keyring::key_list()[1,2],
#                password = key_get("mandy.karnauskas@noaa.gov", keyring::key_list()[1,2]),
#                dbname = "SECPR")

#dat <- dbGetQuery(con, "SELECT * 
#            FROM rdi.V_REC_ACL_MRIP_FES_IMP@secapxdv_dblk.sfsc.noaa.gov")

#saveRDS(dat, file = "../../../CONFIDENTIAL/allrecdata.rds")

dat <- readRDS("C://Users/mandy.karnauskas/Desktop/CONFIDENTIAL/allrecdata.rds")

head(dat)
apply(dat[2:18], 2, table, useNA = "always")

table(dat$NEW_STA, dat$COUNCILREG, useNA = "always")
table(dat$FL_REG, dat$COUNCILREG, useNA = "always")

# codes for WFL regions: 1 (NW FL Panhandle- Escambia to Dixie co.), 2 (SW FL Peninsula- Levy to Collier co), 
# 3 (FL Keys- 
# subregional codes: 6 == Atlantic and 7 == Gulf
# ensure Keys are assigned to the Atlantic not the Gulf
dat$COUNCILREG[which(dat$FL_REG == 3)] <- 6
table(dat$FL_REG, dat$COUNCILREG, useNA = "always")

d <- dat[which(dat$COUNCILREG == 7), ]
table(d$COUNCILREG)

table(d$YEAR, useNA = "always")
table(d$MONTH, useNA = "always")
table(d$WAVE, useNA = "always")
table(d$SUB_REG, useNA = "always")
table(d$NEW_AREAN, useNA = "always")
table(d$NEW_COM, useNA = "always")

gulf_council_managed <- c(
  # Reef Fish
  "gray triggerfish", "gray snapper", "red snapper", "warsaw grouper",
  "vermilion snapper", "goliath grouper", "blackfin snapper", "almaco jack",
  "greater amberjack", "lane snapper", "scamp", "wenchman",
  "yellowmouth grouper", "red grouper", "snowy grouper", "tilefish",
  "speckled hind", "silk snapper", "lesser amberjack", "yellowedge grouper",
  "banded rudderfish", "gag", "yellowtail snapper", "cubera snapper",
  "black grouper", "mutton snapper", "hogfish", "blueline tilefish",
  "queen snapper", "goldface tilefish",
  # Coastal Migratory Pelagics
  "king mackerel", "spanish mackerel", "cobia"  #,
  # Red Drum
#  "red drum"
)

state_managed <- c(
  # Nearshore & Estuarine Marine
  "black drum", "spotted seatrout", "sand seatrout", "silver seatrout",
  "atlantic croaker", "spot", "southern kingfish", "gulf kingfish",
  "northern kingfish", "star drum", "banded drum", "spotted drum",
  "southern flounder", "gulf flounder", "common snook", "fat snook",
  "swordspine snook", "tarpon snook", "florida pompano", "permit",
  "crevalle jack", "blue runner", "lookdown", "palometa", "sheepshead",
  "pinfish", "striped mullet", "white mullet", "fantail mullet",
  "ladyfish", "atlantic spadefish", "tripletail", "bonefish",
  "great barracuda", "atlantic tarpon", "gulf menhaden", "atlantic menhaden",
  "scaled sardine", "spanish sardine")

FW <- c("largemouth bass", "spotted bass", "smallmouth bass", "bluegill",
  "redear sunfish", "redbreast sunfish", "spotted sunfish", "green sunfish",
  "warmouth", "pumpkinseed", "rock bass", "black crappie", "white crappie",
  "channel catfish", "blue catfish", "flathead catfish", "white catfish",
  "brown bullhead", "yellow bullhead", "black bullhead", "alligator gar",
  "longnose gar", "spotted gar", "florida gar", "shortnose gar", "bowfin",
  "striped bass", "hybrid striped bass", "white bass", "yellow bass",
  "american eel", "alabama shad", "american shad", "hickory shad",
  "gizzard shad", "freshwater drum", "yellow perch", "blue tilapia",
  "mayan cichlid", "common carp", "grass carp", "Silver Carp")

dim(d)
d <- d[d$NEW_COM %in% gulf_council_managed, ]
dim(d)

table(d$YEAR, useNA = "always")
table(d$MONTH, useNA = "always")
table(d$WAVE, useNA = "always")
table(d$YEAR, d$WAVE, useNA = "always")
table(d$YEAR, d$MONTH, useNA = "always")
table(d$SUB_REG, useNA = "always")
table(d$NEW_AREAN, useNA = "always")
table(d$NEW_COM, useNA = "always")

d <- d[which(d$YEAR > 1997 & d$YEAR < 2026), ]

tab <- tapply(d$LBSEST_SECWWT, list(d$YEAR, d$WAVE), sum, na.rm = T)
tabp <- (tab / rowSums(tab, na.rm = TRUE))

matplot(t(tab), type = "l")
matplot(t(tabp), type = "l")

apply(tab, 1, function(x) calcGini(x))
apply(tabp, 1, function(x) calcGini(x))

unique(d$NEW_COM)
yrs <- min(d$YEAR):max(d$YEAR)

par(mar = c(10, 4, 1, 1))
wtvec <- sort(tapply(d$LBSEST_SECWWT, d$NEW_COM, sum, na.rm = T), decreasing = T)
barplot(wtvec, las = 2)
sum(wtvec[1:10]) / sum(wtvec)

splis <- names(wtvec)[1:10]
splis

fin <- c()

for (i in splis)  {
  d1 <- d[which(d$NEW_COM == i), ]
  d1$YEAR <- factor(d1$YEAR, levels = yrs)
  tab1 <- tapply(d1$LBSEST_SECWWT, list(d1$YEAR, d1$WAVE), sum, na.rm = T)
  tab1[is.na(tab1)] <- 0
  fin <- data.frame(cbind(fin, apply(tab1, 1, calcGini)))
}
names(fin) <- splis

fin

fin <- fin * (6/5)  # small sample size adjustment
colSums(fin)
dim(fin)
matplot(yrs, fin, type = "b", pch = 1:10, lty = 1, col = glasbey(10))
legend("topleft", names(fin), lty = 1, col = glasbey(10), pch = 1:10)

ind <-   apply(fin, 1, mean, na.rm = T)
indse <- apply(fin, 1, sd, na.rm = T)

datdata <- as.integer(yrs)
inddata <- data.frame(ind)
ulidata <- data.frame(ind + indse)
llidata <- data.frame(ind - indse)

labs <- c("Distribution of recreational catch across the year", "Concentration across 2-month waves", NA)

indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

s <- list(labels = indnames, indicators = inddata, datelist = datdata, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"
s

# save and plot ---------------------------------------

plotIndicatorTimeSeries(s)

plotIndicatorTimeSeries(s, coltoplot = 1, trendAnalysis = T, outtype = "png", widadj = 1.3, hgtadj = 1.2)







