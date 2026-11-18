

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

tpwd.raw = dbGetQuery(con,
                      paste0("select *
                        from rdi.tpwd_estimates_effort@secapxdv_dblk.sfsc.noaa.gov" ))

lacr.raw = dbGetQuery(con,
                      paste0("select *
                        from rdi.la_creel_effort@secapxdv_dblk.sfsc.noaa.gov" ))


table(mrip$SUB_REG, mrip$NEW_STA)
mrip <- mrip[which(mrip$SUB_REG == 7), ]

tapply(mrip$ESTRIPS, mrip$INT_YEAR, sum, na.rm = T)

plot(tapply(mrip$ESTRIPS, mrip$INT_YEAR, sum, na.rm = T), type = "l")
