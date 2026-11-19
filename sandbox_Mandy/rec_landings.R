
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

dg <- dat[which(dat$COUNCILREG == 7), ]
table(dg$COUNCILREG)

table(dg$YEAR, useNA = "always")
table(dg$MONTH, useNA = "always")
table(dg$WAVE, useNA = "always")
table(dg$SUB_REG, useNA = "always")
table(dg$NEW_AREAN, useNA = "always")
table(dg$NEW_COM, useNA = "always")

# Federal Management (Gulf Council FMPs)
gulf_council_managed <- c(
  # Snappers (Reef Fish FMP)
  "red snapper", "gray snapper", "vermilion snapper", "lane snapper",
  "yellowtail snapper", "mutton snapper", "cubera snapper", "silk snapper",
  "blackfin snapper", "queen snapper", "black snapper", "wenchman",
  # Groupers & Hinds (Reef Fish FMP)
  "gag", "red grouper", "black grouper", "scamp", "yellowmouth grouper",
  "yellowedge grouper", "snowy grouper", "warsaw grouper", "speckled hind",
  "yellowfin grouper", "misty grouper", "red hind", "rock hind", "graysby",
  "coney", "marbled grouper", "tiger grouper", "goliath grouper", "nassau grouper",
  # Jacks (Reef Fish FMP)
  "greater amberjack", "lesser amberjack", "almaco jack", "banded rudderfish",
  # Tilefishes (Reef Fish FMP)
  "tilefish", "blueline tilefish", "goldface tilefish", "blackline tilefish",
  "anchor tilefish",
  # Triggerfish & Hogfish (Reef Fish FMP)
  "gray triggerfish", "hogfish",
  # Coastal Migratory Pelagics (CMP FMP)
  "king mackerel", "spanish mackerel", "cobia")

# Highly Migratory Species (NOAA Atlantic HMS)
hms_managed <- c(
  # Tunas
  "bluefin tuna", "young school bluefin", "school bluefin",
  "large school bluefin", "small medium bluefin", "large medium bluefin",
  "giant bluefin", "yellowfin tuna", "bigeye tuna", "albacore", "skipjack tuna",
  # Billfishes & Swordfish
  "swordfish", "blue marlin", "white marlin", "sailfish",
  # Managed Sharks (Large Coastal, Small Coastal, Pelagic & Prohibited)
  "blacktip shark", "bull shark", "spinner shark", "lemon shark", "nurse shark",
  "tiger shark", "silky shark", "sandbar shark", "scalloped hammerhead",
  "great hammerhead", "smooth hammerhead", "atlantic sharpnose shark",
  "blacknose shark", "finetooth shark", "bonnethead",
  "shark, small coastal species", "shortfin mako", "mako shark", "blue shark",
  "thresher shark", "bigeye thresher", "ocean whitetip shark", "dusky shark",
  "night shark", "bignose shark", "sand tiger", "smalltail shark",
  "atlantic angel shark", "six gill shark", "reef shark")

# State & Interstate Management (Gulf States / GSMFC)
state_managed <- c(
  "red drum",
  # Major Inshore/Estuarine Gamefish & Foodfish
  "spotted seatrout", "sand seatrout", "silver seatrout", "black drum",
  "southern flounder", "gulf flounder", "sheepshead", "florida pompano",
  "permit", "bonefish", "atlantic tarpon", "common snook", "fat snook",
  "swordspine snook", "tarpon snook", "tripletail",
  # Coastal Baitfish & Forage
  "gulf menhaden", "atlantic menhaden", "yellowfin menhaden",
  "finescale menhaden", "striped mullet", "white mullet", "fantail mullet",
  "redeye mullet",
  # Nearshore Reef & Demersal Species (State-Regulated)
  "red porgy", "whitebone porgy", "knobbed porgy", "jolthead porgy",
  "saucereye porgy", "sheepshead porgy", "grass porgy", "littlehead porgy",
  "longspine porgy", "white grunt", "bluestriped grunt", "french grunt",
  "spanish grunt", "cottonwick", "margate", "black margate", "sailors choice",
  "porkfish", "tomtate", "black sea bass", "bank sea bass", "rock sea bass",
  "bluefish", "weakfish", "atlantic croaker", "spot", "southern kingfish",
  "gulf kingfish", "northern kingfish", "atlantic spadefish", "queen triggerfish",
  "ocean triggerfish", "scrawled cowfish", "honeycomb cowfish", "trunkfish",
  "stoplight parrotfish", "redband parrotfish", "redtail parrotfish",
  "blue parrotfish", "emerald parrotfish", "spanish hogfish", "spotfin hogfish",
  "red hogfish", "puddingwife", "blue tang", "ocean surgeon", "doctorfish",
  "rock beauty", "queen angelfish", "gray angelfish", "french angelfish",
  "blue angelfish", "spotfin butterflyfish", "reef butterflyfish",
  "american eel", "ladyfish", "crevalle jack", "dolphin", "pompano dolphin",
  "wahoo", "great barracuda", "guaguanche", "southern sennet",
  "northern sennet", "lionfish genus", "turkeyfish", "mahogany snapper",
  "dog snapper", "schoolmaster", "cero")

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

dim(dg)

# look at total catch ------------------

par(mar = c(10, 4, 1, 1))
barplot(sort(tapply(dg$LBSEST_SECWWT, dg$NEW_COM, sum, na.rm = T), decreasing = T)[1:30], las = 2)

dg$class <- "other"
dg$class[dg$NEW_COM %in% gulf_council_managed] <- "council"
dg$class[dg$NEW_COM %in% state_managed] <- "state"
dg$class[dg$NEW_COM %in% FW] <- "FW"
dg$class[dg$NEW_COM %in% hms_managed] <- "HMS"
#dg$class[grep("red drum", dg$NEW_COM)] <- "red drum"

d3 <- dg[which(dg$class == "council"), ]
barplot(sort(tapply(d3$LBSEST_SECWWT, d3$NEW_COM, sum, na.rm = T), decreasing = T)[1:20], las = 2)
d3 <- dg[which(dg$class == "state"), ]
barplot(sort(tapply(d3$LBSEST_SECWWT, d3$NEW_COM, sum, na.rm = T), decreasing = T)[1:20], las = 2)
d3 <- dg[which(dg$class == "FW"), ]
barplot(sort(tapply(d3$LBSEST_SECWWT, d3$NEW_COM, sum, na.rm = T), decreasing = T)[1:20], las = 2)
d3 <- dg[which(dg$class == "HMS"), ]
barplot(sort(tapply(d3$LBSEST_SECWWT, d3$NEW_COM, sum, na.rm = T), decreasing = T)[1:20], las = 2)
d3 <- dg[which(dg$class == "other"), ]
barplot(sort(tapply(d3$LBSEST_SECWWT, d3$NEW_COM, sum, na.rm = T), decreasing = T)[1:20], las = 2)

dg$class <- factor(dg$class, levels = c("council", "state", "HMS", "other", "FW"))
dg <- dg[which(dg$YEAR <=2025), ]

tab <- data.frame(tapply(dg$LBSEST_SECWWT, list(dg$YEAR, dg$class), sum, na.rm = T))
matplot(rownames(tab), tab, type = "l", lty = 1, col = 1:5)
legend("topright", col = 1:5, colnames(tab), lty = 1, bty = "n")

tab <- tab[, 1:4]
#tab$tot <- rowSums(tab, na.rm = T)

datdata <- as.integer(rownames(tab))

labs <- c("Gulf Council managed species", "millions of lbs", NA, 
          "State managed species", "millions of lbs", NA, 
          "Highly migratory species", "millions of lbs", NA, 
          "Unmanaged species", "millions of lbs", NA)

indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

s <- list(labels = indnames, indicators = tab/10^6, datelist = datdata)
class(s) <- "indicatordata"
s

# save and plot ---------------------------------------

plotIndicatorTimeSeries(s, coltoplot = 1:4, sameYscale = F, plotrownum = 2)

plotIndicatorTimeSeries(s, coltoplot = 1:4, sameYscale = F, trendAnalysis = T, outtype = "png", widadj = 1.5, hgtadj = 1.3)


# gini index indicator ------------------

d <- dg[dg$NEW_COM %in% gulf_council_managed, ]
dim(d)

table(d$YEAR, useNA = "always")
table(d$MONTH, useNA = "always")
table(d$WAVE, useNA = "always")
table(d$YEAR, d$WAVE, useNA = "always")
table(d$YEAR, d$MONTH, useNA = "always")
table(d$SUB_REG, useNA = "always")
table(d$NEW_AREAN, useNA = "always")
table(d$NEW_COM, useNA = "always")

d <- d[which(d$YEAR > 1997), ]

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







