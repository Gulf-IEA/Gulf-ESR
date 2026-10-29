
### to do
# sum subsectors into annual estimates
# check IFQ data for CS species

library(here)
library(rvest)
library(janitor)


##############################
### commercial ---------------
##############################

url <- "https://www.fisheries.noaa.gov/southeast/commercial-fishing/gulf-america-historical-commercial-landings-and-annual-catch-limit"

webpage <- read_html(url) 

# Select the table using CSS selector 
table_head <- html_elements(webpage, "h2") 
# table_node <- html_elements(webpage, "table") 
tables <- html_table(webpage, header = T) |>
  lapply(function(x) x |> clean_names() |> data.frame())

### clean up spp list
labs <- table_head[2:(length(tables)+1)] |>
  html_text2() |>
  lapply(function(x) strsplit(x, "\\(")[[1]][1]) |> trimws()
labs <- gsub("[*]", "", labs)

colist <- c("spp", "year",'sector', "total_reported", "acl", "acl_percent" )
dat <- data.frame()

### do king mackerel separate
kgm <- grep('king mackerel',labs, ignore.case = T)
spp_ls <- seq(1,length(labs))[-kgm]

for (i in spp_ls){ 
  d1 <- tables[[i]] 
  if(names(d1)[1]== "x") {
    d1 <- d1 |> row_to_names(row_number = 1) |> 
      clean_names() |> data.frame()
  }
  # tab <- html_table(table_node)[[i]]
  # ### need to add a condition that if the first row is empty, try the second row for headers
  # if(all(tab[1,]=='')) {
  #   irow <- 2
  # } else {
  #   irow <- 1
  # }
  # d1 <- tab |> row_to_names(row_number = irow) |> 
  #   clean_names() |> data.frame()
  d1$year <- substr(d1$year, 1, 4) |> as.numeric()
  d1 <- d1[which(!is.na(as.numeric(d1$year))), ]
  d1$spp <- labs[i]
  d1$sector <- "commercial"
  cols <- unlist(sapply(colist, function(x) which(x == names(d1))))
  d2 <- d1[cols]
  names(d2)[which(names(d2)=='total_reported')] <- "total_landings"
  d2$total_landings <- gsub(",", "", d2$total_landings)
  d2$acl <- gsub(",", "", d2$acl)
  dat <- rbind(dat, d2) |> type.convert(as.is = T)
  rm(d1, d2)
}

### king mackerel
# for (i in kgm){ 
### hand line
  d1 <- tables[[kgm[1]]] 
  if(names(d1)[1]== "x") {
    d1 <- d1 |> row_to_names(row_number = 1) |> 
      clean_names() |> data.frame()
  }
  d1$year <- substr(d1$year, 1, 4) |> as.numeric()
  d1 <- d1[which(!is.na(as.numeric(d1$year))), ]
  # d1$spp <- labs[i]
  d1$sector <- "hl"
  d1$total_landings <- gsub(",", "", d1$total_reported) |> as.numeric()
  d1$acl <- gsub(",", "", d1$acl) |> as.numeric()
  # d1$acl_percent <- as.numeric(d1$acl_percent)
  
  d1 <- d1 |>
    group_by(year, sector) |>
    summarize(total_landings = sum(total_landings, na.rm = T),
              acl = max(acl, na.rm = T)) |>
              # acl_percent = max(acl_percent, na.rm = T)) |>
    as.data.frame()
  
  ### gillnet
  d2 <- tables[[kgm[2]]] 
  if(names(d2)[1]== "x") {
    d2 <- d2 |> row_to_names(row_number = 1) |> 
      clean_names() |> data.frame()
  }
  d2$year <- substr(d2$year, 1, 4) |> as.numeric()
  d2 <- d2[which(!is.na(as.numeric(d2$year))), ]
  # d2$spp <- labs[i]
  d2$sector <- "gn"
  d2$total_landings <- gsub(",", "", d2$total_reported) |> as.numeric()
  d2$acl <- gsub(",", "", d2$acl) |> as.numeric()
  
  d2 <- select(d2, year, sector, total_landings, acl)
  
  dt <- rbind(d1, d2) |>
    group_by(year) |>
    summarize(total_landings = sum(total_landings, na.rm = T),
              acl = sum(acl, na.rm = T)) |>
    mutate(acl_percent = total_landings / acl * 100) |>
    mutate(spp = "King Mackerel", .before = year) |>
    mutate(sector = 'commercial', .after = year)

  dat <- rbind(dat, dt) |> type.convert(as.is = T)
  rm(d1, d2, dt)
# }


##############################
### recreational -------------
##############################

url <- "https://www.fisheries.noaa.gov/southeast/recreational-fishing-data/gulf-america-historical-recreational-landings-and-annual-catch"

webpage <- read_html(url) 

# Select the table using CSS selector 
table_head <- html_elements(webpage, "h3") 
# table_node <- html_elements(webpage, "table") 
tables <- html_table(webpage, header = T) |>
  lapply(function(x) x |> clean_names() |> data.frame())

### clean up spp list
labs <- table_head |>
  html_text2() #|>
#   lapply(function(x) strsplit(x, "\\(")[[1]][1]) |> trimws()
# labs <- gsub("[*]", "", labs)

# colist <- c("spp", "Year", "Total.Reported", "Total.Reported.", "ACL", "ACL.", "ACL..")
colist <- c("spp", "year",'sector', "total_reported", "acl", "acl_percent" )
# dat <- data.frame()

for (i in 1:length(labs))  { 
  d1 <- tables[[i]] 
  if(names(d1)[1]!= "year") {
    d1 <- d1 |> row_to_names(row_number = 1) |> 
      clean_names() |> data.frame()
  }
  # tab <- html_table(table_node)[[i]] 
  # ### need to add a condition that if the first row is empty, try the second row for headers
  # if(tab[1,1]!='Year') {
  #   irow <- 2
  # } else {
  #   irow <- 1
  # }
  # d1 <- tab |> row_to_names(row_number = irow) |> 
  #   clean_names() |> data.frame()
  d1$year <- substr(d1$year, 1, 4) |> as.numeric()
  d1 <- d1[which(!is.na(as.numeric(d1$year))), ]
  d1$spp <- labs[i]
  d1$sector <- "recreational"
  cols <- unlist(sapply(colist, function(x) which(x == names(d1))))
  d2 <- d1[cols]
  names(d2)[which(names(d2)=='total_reported')] <- "total_landings"
  d2$total_landings <- as.numeric(gsub(",", "", d2$total_landings))
  d2$acl <- as.numeric(gsub(",", "", d2$acl))
  
  ### red snapper needs summation across private and for-hire sectors
  if(d2$spp[1] == "Red Snapper") {
    d2 <- d2 |>
      group_by(year, sector) |>
      summarize(total_landings = sum(as.numeric(gsub(",", "", total_landings)), na.rm = T),
                acl = sum(as.numeric(gsub(",", "", acl)), na.rm = T)) |>
      mutate(acl_percent = total_landings / acl * 100) |>
      mutate(spp = "Red Snapper", .before = year) |> as.data.frame()
    ### landings and acl percent are missing private angler data since 2018
  }
  
  dat <- rbind(dat, d2) |> type.convert(as.is = T)
  rm(d1, d2)
}



##############################
### stock acls ---------------
##############################

url <- "https://www.fisheries.noaa.gov/southeast/commercial-fishing/gulf-mexico-historical-stock-landings-and-annual-catch-limit"

webpage <- read_html(url) 

# Select the table using CSS selector 
table_head <- html_elements(webpage, "h2") 
# table_node <- html_elements(webpage, "table") 
tables <- html_table(webpage, header = T) |>
  lapply(function(x) x |> clean_names() |> data.frame())

### clean up spp list
labs <- table_head[2:length(table_head)] |>
  html_text2() #|>
#   lapply(function(x) strsplit(x, "\\(")[[1]][1]) |> trimws()
labs <- gsub("[*]", "", labs)

# colist <- c("spp", "Year", "Total.Reported", "Total.Reported.", "ACL", "ACL.", "ACL..")
colist <- c("spp", "year",'sector' ,"total_landings", "acl", "acl_percent" )
# dat <- data.frame()

for (i in 1:length(labs))  { 
  d1 <- tables[[i]] 
  if(names(d1)[1]!= "year") {
    d1 <- d1 |> row_to_names(row_number = 1) |> 
      clean_names() |> data.frame()
  }
  # tab <- html_table(table_node)[[i]] 
  # ### need to add a condition that if the first row is empty, try the second row for headers
  # if(tab[1,1]!='Year') {
  #   irow <- 2
  # } else {
  #   irow <- 1
  # }
  # d1 <- tab |> row_to_names(row_number = irow) |> 
  #   clean_names() |> data.frame()
  d1$year <- substr(d1$year, 1, 4) |> as.numeric()
  d1 <- d1[which(!is.na(as.numeric(d1$year))), ]
  d1$spp <- labs[i]
  d1$sector <- "total stock"
  cols <- unlist(sapply(colist, function(x) which(x == names(d1))))
  d2 <- d1[cols]
  # names(d2)[3] <- "total_landings"
  d2$total_landings <- as.numeric(gsub(",", "", d2$total_landings))
  d2$acl <- as.numeric(gsub(",", "", d2$acl))
  dat <- rbind(dat, d2) |> type.convert(as.is = TRUE)
  rm(d1, d2)
}


dat$spp[grep('cobia', dat$spp, ignore.case = T)] <- 'Cobia'
dat$spp[grep('shallow', dat$spp, ignore.case = T)] <- 'Shallow Water Grouper'
dat$spp[grep('King Mackerel', dat$spp, ignore.case = T)] <- 'King Mackerel'



### question: how to deal with units (as reported, gw, ww) when aggregating?
### question: King mackerel are on separate fishing year, should they be treated separately?
### question: do we include cobia?
# *The Florida East Coast Zone (east coast of Florida and Atlantic side of the 
# Florida Keys) is part of the Gulf of Mexico stock and is jointly managed with 
# the South Atlantic Fishery Management Council.

com_acls <- subset(dat, sector == 'commercial', select = c('year','spp','acl')) |>
  reshape(idvar = 'spp', timevar = 'year', direction = 'wide') #rows as spp and columns as year
rec_acls <- subset(dat, sector == 'recreational', select = c('year','spp','acl')) |>
  reshape(idvar = 'spp', timevar = 'year', direction = 'wide')
rownames(com_acls) <- com_acls$spp
rownames(rec_acls) <- rec_acls$spp
names(com_acls) <- gsub('acl.', '', names(com_acls))
names(rec_acls) <- gsub('acl.', '', names(rec_acls))

### with cobia 2015-2024; without cobia 2011 (rec) and 2012 (com)
# with cobia
rec_acls_wc <- rec_acls[,c(11:2)]
com_acls_wc <- com_acls[,c(11:2)]

### function that computes the percent different between previous value and current value returns a vector
pct_change_na <- function(x) {
  n <- length(x)
  r <- c(NA, (x[2:n] - x[1:(n - 1)]) / x[1:(n - 1)] * 100)
  return(r)
}

rec_acls_wc_pct <- t(apply(rec_acls_wc, 1, pct_change_na))
rec_acl_ind <- apply(rec_acls_wc_pct, 2, function (x) rbind(mean(x), sd(x)))
com_acls_wc_pct <- t(apply(com_acls_wc, 1, pct_change_na))
com_acl_ind <- apply(com_acls_wc_pct, 2, function (x) rbind(mean(x), sd(x)))

png(here('figures', 'plots', 'acl_percent_change_tmp.png'), width = 7, height = 4, units = 'in', res = 300, pointsize = 11)
par(mfrow=c(2,2), mar = c(4,4,1.5,1))
plot(colnames(rec_acl_ind), rec_acl_ind[1,], type = 'o', las = 1, pch = 16,
     xlab = 'Year', ylab = 'Percent Change in ACL', main = 'Recreational ACL Percent Change')
plot(colnames(rec_acl_ind), rec_acl_ind[2,], type = 'o', las = 1, pch = 16,
     xlab = 'Year', ylab = 'Percent Change SD in ACL', main = 'Recreational ACL Percent Change SD')
plot(colnames(com_acl_ind), com_acl_ind[1,], type = 'o', las = 1, pch = 16,
     xlab = 'Year', ylab = 'Percent Change in ACL', main = 'Commerical ACL Percent Change')
plot(colnames(com_acl_ind), com_acl_ind[2,], type = 'o', las = 1, pch = 16,
     xlab = 'Year', ylab = 'Percent Change SD in ACL', main = 'Commerical ACL Percent Change SD')
dev.off()


### total acls

tot_acls <- data.frame(year = colnames(rec_acls_wc),
                       rec = apply(rec_acls_wc,2,sum)/1e6,
                       com = apply(com_acls_wc,2,sum)/1e6)

png(here('figures', 'plots', 'acl_totals_tmp.png'), width = 4, height = 4, units = 'in', res = 300, pointsize = 11)
par(mfrow=c(2,1), mar = c(4,4,1.5,1))
plot(tot_acls$year, tot_acls$rec, type = 'o', las = 1, pch = 16,
     xlab = 'Year', ylab = 'Total ACL (million lbs)', main = 'Recreational ACL',
     ylim = c(range(tot_acls$rec, tot_acls$com)))
plot(tot_acls$year, tot_acls$com, type = 'o', las = 1, pch = 16,
     xlab = 'Year', ylab = 'Total ACL (million lbs)', main = 'Commercial ACL',
     ylim = c(range(tot_acls$rec, tot_acls$com)))
dev.off()






### Mandy's old code

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
