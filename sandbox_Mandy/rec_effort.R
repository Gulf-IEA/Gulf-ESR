
rm(list = ls())

library(ROracle)
library(keyring)

con = dbConnect(dbDriver("Oracle"), username = keyring::key_list()[1,2],
                password = key_get("mandy.karnauskas@noaa.gov", keyring::key_list()[1,2]),
                dbname = "SECPR")

#Effort is in separate views for MRIP, TXPD, and LA Creel.

mrip = dbGetQuery(con,
                      paste0("select *
                        from RDI.v_mrip_domain_cal_eff_imp@secapxdv_dblk.sfsc.noaa.gov" ))

tpwd = dbGetQuery(con,
                      paste0("select *
                        from rdi.tpwd_estimates_effort@secapxdv_dblk.sfsc.noaa.gov" ))
tpwd$NTRP <- tpwd$ESTHRS / tpwd$TRIPLEN

# NTRP	Effort Estimates in number of angler trips ( = ESTHRS / TRIPLEN )

lacr = dbGetQuery(con,
                      paste0("select *
                        from rdi.la_creel_effort@secapxdv_dblk.sfsc.noaa.gov" ))

#  EXPANDED_EFFORT	Estimate of the total number of angler trips (i.e., EFFORT) 
#  expanded by the observed non-compliance rates

# The sampling frames for both LA Creel effort surveys (private and charter) 
# consists of anglers in possession of the appropriate fishing licenses. 
# Unlicensed anglers are therefore not sampled by these surveys, and the 
# associated effort estimates (i.e., in EFFORT) are considered underestimates. 
# This is accounted for by expanding observed effort estimates by LA recreational 
# saltwater license requirement and compliance levels determined from the access 
# point survey, the results of which are saved as EXPANDED_EFFORT.


table(mrip$SUB_REG, mrip$NEW_STA)
table(mrip$FL_REG, mrip$SUB_REG)

# codes for WFL regions: 1 (NW FL Panhandle- Escambia to Dixie co.), 2 (SW FL Peninsula- Levy to Collier co), 
# 3 (FL Keys- 
# subregional codes: 6 == Atlantic and 7 == Gulf
# ensure Keys are assigned to the Atlantic not the Gulf

mrip$SUB_REG[which(mrip$FL_REG == 3)] <- 6
table(mrip$FL_REG, mrip$SUB_REG)

mrip <- mrip[which(mrip$SUB_REG == 7), ]

yrs <- min(mrip$INT_YEAR) : (as.numeric(format(Sys.Date(), "%Y")) - 1)

mrip$INT_YEAR <- factor(mrip$INT_YEAR, levels = yrs)
tpwd$SYEAR <- factor(tpwd$SYEAR, levels = yrs)
lacr$INT_YEAR <- factor(lacr$INT_YEAR, levels = yrs)

barplot(tapply(mrip$ESTRIPS, list(mrip$NEW_AREAN, mrip$INT_YEAR), sum, na.rm = T), 
          legend = unique(mrip$NEW_AREAN), col = 1:5, args.legend = list(x = "topleft"))
barplot(tapply(tpwd$NTRP, list(tpwd$AREANAME, tpwd$SYEAR), sum, na.rm = T), 
        legend = unique(mrip$NEW_AREAN), col = 1:5, args.legend = list(x = "topleft"))
barplot(tapply(lacr$EXPANDED_EFFORT, list(lacr$AREA_X_F, lacr$INT_YEAR), sum, na.rm = T), 
        legend = unique(mrip$NEW_AREAN), col = 1:5, args.legend = list(x = "topleft"))

unique(mrip$NEW_AREAN)

m2 <- mrip[which(mrip$NEW_AREAN == "Ocean>3mi" | mrip$NEW_AREAN == "Ocean>10mi"), ]

mtot <- tapply(mrip$ESTRIPS, mrip$INT_YEAR, sum, na.rm = T)
ttot <- tapply(tpwd$NTRP, tpwd$SYEAR, sum, na.rm = T)
ltot <- tapply(lacr$EXPANDED_EFFORT, lacr$INT_YEAR, sum, na.rm = T)

tot <- data.frame(mtot, ttot, ltot)
matplot(yrs, tot, type = "l")

tot2 <- tapply(m2$ESTRIPS, m2$INT_YEAR, sum, na.rm = T)

inddata <- data.frame(cbind(rowSums(tot, na.rm = T), tot2)/10^6)

datdata <- as.integer(yrs)

labs <- c("Total recreational effort", "millions of angler trips", NA, 
          "Offshore recreational effort", "millions of angler trips", NA)

indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

s <- list(labels = indnames, indicators = inddata, datelist = datdata) # , ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"
s

# save and plot ---------------------------------------

plotIndicatorTimeSeries(s, coltoplot = 1:2, plotrownum = 2)

plotIndicatorTimeSeries(s, coltoplot = 1:2, plotrownum = 2, widadj = 1.25, hgtadj = 1.1, outtype = "png")



