
library(here)
library(rvest)
library(dplyr)


##############################
### recreational -------------
##############################

# URL of the page
url <- "https://www.fisheries.noaa.gov/southeast/recreational-fishing-data/gulf-america-historical-recreational-landings-and-annual-catch"

# Read HTML content from the page
page <- read_html(url)

# Extract all tables from the webpage
rec_tables <- page %>% html_table(fill = TRUE)

# View total number of tables extracted
cat("Total tables found:", length(rec_tables), "\n")

# Access individual species tables (e.g., Table 1: Cobia, Table 2: Gag Grouper)
cobia_rec <- rec_tables[[1]]
gag_rec <- rec_tables[[2]]
graytf_rec <- rec_tables[[3]]
gaj_rec <- rec_tables[[4]]
kgm_rec <- rec_tables[[5]]
rg_rec <- rec_tables[[6]]
rs_rec <- rec_tables[[7]]


##############################
### commercial ---------------
##############################

# URL of the NOAA Fisheries page
url <- "https://www.fisheries.noaa.gov/southeast/commercial-fishing/gulf-america-historical-commercial-landings-and-annual-catch-limit"

# Read HTML content from the page
page <- read_html(url)

# Extract all HTML tables on the page into a list of data frames
com_tables <- page %>% html_table(fill = TRUE)

# Print total number of tables extracted
cat("Total tables found:", length(com_tables), "\n")

# Assign specific species tables to individual variables
cobia_com <- com_tables[[1]]
dwg_com <- com_tables[[2]]
gag_com <- com_tables[[3]]
graytf_com <- com_tables[[4]]
gaj_com <- com_tables[[5]]
kgm_com <- com_tables[[6]]
kgm_com_gn <- com_tables[[7]]
rg_com <- com_tables[[8]]
rs_com <- com_tables[[9]]
swg_com <- com_tables[[10]]
tf_com <- com_tables[[11]]


##############################
### stock acls ---------------
##############################

# URL of the NOAA Fisheries page currently being viewed
url <- "https://www.fisheries.noaa.gov/southeast/commercial-fishing/gulf-mexico-historical-stock-landings-and-annual-catch-limit"

# Read HTML content from the page
page <- read_html(url)

# Extract all HTML tables into a list of data frames
stock_tables <- page %>% html_table(fill = TRUE)

# Print the total number of species tables found
cat("Total tables found:", length(stock_tables), "\n")

# Assign specific species tables to individual variables
cobia_stock <- stock_tables[[1]]
cubera_stock <- stock_tables[[2]]
dwg_stock <- stock_tables[[3]]
gray_snapper_stock <- stock_tables[[4]]
hogfish_stock <- stock_tables[[5]]
jacks_stock <- stock_tables[[6]]
lane_snapper_stock <- stock_tables[[7]]
mid_water_snapper_stock <- stock_tables[[8]]
mutton_snapper_stock <- stock_tables[[9]]
swg_stock <- stock_tables[[10]]
tf_stock <- stock_tables[[11]]
spanish_mack_stock <- stock_tables[[12]]
vermilion_stock <- stock_tables[[13]]
yellowtail_snapper_stock <- stock_tables[[14]]

setwd(here('data/intermediate'))
save(rec_tables, com_tables, stock_tables, file = "sero_acl_scrape.RData")


