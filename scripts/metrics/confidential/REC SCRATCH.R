# REC EFFORT DATA SCRATCH

Sys.setenv(TZ = "UTC", ORA_SDTZ = "UTC")

# Define your username
my_username <- "CGERVASI"

# Connect using stored keyring password
con <- dbConnect(dbDriver("Oracle"), 
                 username = my_username,
                 password = key_get("SECPR_Oracle", my_username),
                 dbname = "SECPR")



# Quick test query for rec landings. BTW this is super helpful: https://sefsc.github.io/SEFSC-dolphin-analyses/MRIP.html

ACL.raw = dbGetQuery(con,
                      paste0("select *
                        from rdi.V_REC_ACL_MRIP_FES_IMP@secapxdv_dblk.sfsc.noaa.gov WHERE ROWNUM <= 1000" ))




# Run quick test queries (this is how you extract angler effort, number of trips per year)

mrip.raw = dbGetQuery(con,
                      paste0("select *
                        from RDI.v_mrip_domain_cal_eff_imp@secapxdv_dblk.sfsc.noaa.gov WHERE ROWNUM <= 1000" ))


tpwd.raw = dbGetQuery(con,
                      paste0("select *
                        from rdi.tpwd_estimates_effort@secapxdv_dblk.sfsc.noaa.gov WHERE ROWNUM <= 1000" ))


lacr.raw = dbGetQuery(con,
                      paste0("select *
                        from rdi.la_creel_effort@secapxdv_dblk.sfsc.noaa.gov WHERE ROWNUM <= 1000" ))

dbDisconnect(con)
