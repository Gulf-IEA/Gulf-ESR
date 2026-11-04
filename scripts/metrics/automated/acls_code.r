# File created on 2026-10-30 by B. Turley

#### 0. Setup ####
# Add any packages that are needed for analysis here.
library(IEAnalyzeR)
library(here)
library(ggplot2)
library(patchwork)
library(cmocean)

# File Naming Setup.
# !! Auto generated-Do Not Change !!
root_name <- "acls_total"

csv_filename <- here(paste0("data/formatted/formatted_csvs/", root_name, "_formatted.csv"))
object_filename <- here(paste0("data/formatted/final_objects/", root_name, "_object.rds"))
plot_filename <- here(paste0("figures/plots/", root_name, "_plot.png"))

root_name2 <- "acls_change"

csv_filename2 <- here(paste0("data/formatted/formatted_csvs/", root_name2, "_formatted.csv"))
object_filename2 <- here(paste0("data/formatted/final_objects/", root_name2, "_object.rds"))
plot_filename2 <- here(paste0("figures/plots/", root_name2, "_plot.png"))

#----------------------------------------------------
#### 1. Read Data ####
# Pull data from its source:
# Manual data: data/unformatted data
# Automated data: Add script for data call (API, package, etc.)
# Confidential data: Store locally in the confidential data folder
#   - This folder is excluded using gitignore and will not push to the GitHub repo
# If intermediate data (shapefiles etc.) are needed, please put them in data>intermediate
#   - Filename should use the syntax rootname_descriptivename

### These ACLs were scraped from the SERO ACLS website and are available in the 'data/intermediate' folder. 
### The data were scraped and cleaned using the 'sero_acl_scrape_mk.R' code found in the 'scripts/other' folder.
acls <- read.csv(here('data/intermediate/sero_acl_pull.csv'))

com_acls <- subset(acls, sector == 'commercial', select = c('year','spp','acl_lbs')) |>
  reshape(idvar = 'spp', timevar = 'year', direction = 'wide') #rows as spp and columns as year
rec_acls <- subset(acls, sector == 'recreational', select = c('year','spp','acl_lbs')) |>
  reshape(idvar = 'spp', timevar = 'year', direction = 'wide')
sto_acls <- subset(acls, sector == 'total stock', select = c('year','spp','acl_lbs')) |>
  reshape(idvar = 'spp', timevar = 'year', direction = 'wide') #rows as spp and columns as year
rownames(com_acls) <- com_acls$spp
rownames(rec_acls) <- rec_acls$spp
rownames(sto_acls) <- sto_acls$spp
names(com_acls) <- gsub('acl_lbs.', '', names(com_acls))
names(rec_acls) <- gsub('acl_lbs.', '', names(rec_acls))
names(sto_acls) <- gsub('acl_lbs.', '', names(sto_acls))

### with cobia 2015-2024; without cobia 2011 (rec) and 2012 (com)
# with cobia
com_acls_wc <- com_acls[,c(11:2)]
rec_acls_wc <- rec_acls[,c(11:2)]
sto_acls_wc <- sto_acls[,c(11:2)]

### function that computes the percent different between previous value and current value returns a vector
pct_change_na <- function(x) {
  n <- length(x)
  r <- c(NA, (x[2:n] - x[1:(n - 1)]) / x[1:(n - 1)] * 100)
  return(r)
}


com_acls_wc_pct <- t(apply(com_acls_wc, 1, pct_change_na))[,-1]
com_acl_ind <- apply(com_acls_wc_pct, 2, function (x) rbind(mean(x), sd(x)))
com_poly <- c(com_acl_ind[1,] + com_acl_ind[2,], rev(com_acl_ind[1,] - com_acl_ind[2,]))

rec_acls_wc_pct <- t(apply(rec_acls_wc, 1, pct_change_na))[,-1]
rec_acl_ind <- apply(rec_acls_wc_pct, 2, function (x) rbind(mean(x), sd(x)))
rec_poly <- c(rec_acl_ind[1,] + rec_acl_ind[2,], rev(rec_acl_ind[1,] - rec_acl_ind[2,]))

sto_acls_wc_pct <- t(apply(sto_acls_wc, 1, pct_change_na))[,-1]
sto_acl_ind <- apply(sto_acls_wc_pct, 2, function (x) rbind(mean(x), sd(x)))
sto_poly <- c(sto_acl_ind[1,] + sto_acl_ind[2,], rev(sto_acl_ind[1,] - sto_acl_ind[2,]))


par(mfrow=c(2,2), mar = c(4,4,1.5,1))
plot(colnames(com_acl_ind), com_acl_ind[1,], type = 'o', las = 1, pch = 16,
     ylim = range(com_poly, na.rm = TRUE),
     xlab = 'Year', ylab = 'Percent Change in ACL', main = 'Commerical ACL Percent Change')
polygon(c(colnames(com_acl_ind), rev(colnames(com_acl_ind))),
        com_poly,
        col = rgb(0.5, 0.5, 0.5, 0.5), border = NA)
plot(colnames(rec_acl_ind), rec_acl_ind[1,], type = 'o', las = 1, pch = 16,
     ylim = range(rec_poly, na.rm = TRUE),
     xlab = 'Year', ylab = 'Percent Change in ACL', main = 'Recreational ACL Percent Change')
polygon(c(colnames(rec_acl_ind), rev(colnames(rec_acl_ind))),
        rec_poly,
        col = rgb(0.5, 0.5, 0.5, 0.5), border = NA)
plot(colnames(sto_acl_ind), sto_acl_ind[1,], type = 'o', las = 1, pch = 16,
     ylim = range(sto_poly, na.rm = TRUE),
     xlab = 'Year', ylab = 'Percent Change in ACL', main = 'Stock ACL Percent Change')
polygon(c(colnames(sto_acl_ind), rev(colnames(sto_acl_ind))),
        sto_poly,
        col = rgb(0.5, 0.5, 0.5, 0.5), border = NA)



### quantiles not informative due to lots of zeros
# rec_acls_wc_pct <- t(apply(rec_acls_wc, 1, pct_change_na))[,-1]
# # rec_acl_ind <- apply(rec_acls_wc_pct, 2, function (x) rbind(mean(x), sd(x)))
# rec_acl_ind <- apply(rec_acls_wc_pct, 2, function (x) quantile(x, probs = c(0.5, 0.25, 0.75), na.rm = TRUE))
# rec_poly <- c(rec_acl_ind[2,], rev(rec_acl_ind[3,]))
# com_acls_wc_pct <- t(apply(com_acls_wc, 1, pct_change_na))[,-1]
# com_acl_ind <- apply(com_acls_wc_pct, 2, function (x) quantile(x, probs = c(0.5, 0.25, 0.75), na.rm = TRUE))
# com_poly <- c(com_acl_ind[2,], rev(com_acl_ind[3,]))
# 
# boxplot(rec_acls_wc_pct)
# matlines(rec_acls_wc_pct|>t(),typ='o')
# boxplot(com_acls_wc_pct)
# matlines(com_acls_wc_pct|>t(),typ='o')

rec_acls_wc_pct_long <- cbind(expand.grid(2016:2024,row.names(rec_acls_wc_pct),
                                          stringsAsFactors = F),
      as.vector(t(rec_acls_wc_pct))) |> 
  setNames(c('year','spp','pct_change'))

com_acls_wc_pct_long <- cbind(expand.grid(2016:2024,row.names(com_acls_wc_pct),
                                          stringsAsFactors = F),
                              as.vector(t(com_acls_wc_pct))) |> 
  setNames(c('year','spp','pct_change'))
com_acls_wc_pct_long$spp <- factor(com_acls_wc_pct_long$spp)

p1 <- ggplot(rec_acls_wc_pct_long, aes(x = year, y = pct_change, fill = spp)) +
  geom_bar(stat = "identity", position = "stack", color = 1) +
  scale_fill_cmocean(name = "thermal", discrete = T) +
  geom_hline(yintercept = 0, color = 1) + # Adds a clean baseline
  scale_x_continuous(breaks = seq(2016, 2024, by = 1))   +
  labs(x = "Year", y = "Percent Change in ACL") +
  theme_classic()

p2 <- ggplot(com_acls_wc_pct_long, aes(x = year, y = pct_change, fill = spp)) +
  geom_bar(stat = "identity", position = "stack", color = 1) +
  scale_fill_cmocean(name = "thermal", discrete = T) +
  geom_hline(yintercept = 0, color = 1) + # Adds a clean baseline
  scale_x_continuous(breaks = seq(2016, 2024, by = 1))   +
  labs(x = "Year", y = "Percent Change in ACL") +
  theme_classic()

p2 / p1 + plot_annotation(title = "Percent Change in ACLs by Species") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 11))
# ggsave(here('figures/plots/acl_bars_plot.png'), height = 7, width = 7, unit="in")

### total acls

tot_acls <- merge(
  data.frame(year = colnames(com_acls_wc),
             com = apply(com_acls_wc, 2, sum)/1e6),
  data.frame(year = colnames(rec_acls_wc),
             rec = apply(rec_acls_wc, 2, sum)/1e6),
  by = 'year') |>
  merge(data.frame(year = colnames(sto_acls_wc),
                     stock = apply(sto_acls_wc, 2, sum)/1e6),
          by = 'year')

# png(here('figures', 'plots', 'acl_totals_tmp.png'), width = 4, height = 4, units = 'in', res = 300, pointsize = 11)
par(mfrow=c(2,2), mar = c(4,4,1.5,1))
plot(tot_acls$year, tot_acls$rec, type = 'o', las = 1, pch = 16,
     xlab = 'Year', ylab = 'Total ACL (million lbs)', main = 'Recreational ACL',
     ylim = c(range(tot_acls$rec, tot_acls$com)))
plot(tot_acls$year, tot_acls$com, type = 'o', las = 1, pch = 16,
     xlab = 'Year', ylab = 'Total ACL (million lbs)', main = 'Commercial ACL',
     ylim = c(range(tot_acls$rec, tot_acls$com)))
plot(tot_acls$year, tot_acls$stock, type = 'o', las = 1, pch = 16,
     xlab = 'Year', ylab = 'Total ACL (million lbs)', main = 'Stock ACL',
     ylim = c(range(tot_acls$rec, tot_acls$com, tot_acls$stock)))
# dev.off()


#----------------------------------------------------
#### 2. Clean data and create time series csv ####

#Transform the data to fit the IEA data format.
#For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
#Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.


#Define header components for the data rows (ignore year). Fill in the blanks here.
indicator_names = rep('Total ACLs',3)
unit_names = rep('(million lbs)',3)
extent_names = c('Commercial ACL', 'Recreational ACL', 'Stock ACL')

formatted_data = IEAnalyzeR::convert_cleaned_data(tot_acls, 
                                                   indicator_names, unit_names, extent_names)


### annual change in acls
dat <- cbind(colnames(rec_acl_ind), com_acl_ind[1,], rec_acl_ind[1,], sto_acl_ind[1,]) |>
  type.convert(as.is = T)

dat_lci <- cbind(colnames(rec_acl_ind), 
                 com_acl_ind[1,] - com_acl_ind[2,],
                 rec_acl_ind[1,] - rec_acl_ind[2,],
                 sto_acl_ind[1,] - sto_acl_ind[2,]) |>
  type.convert(as.is = T) 

dat_uci <- cbind(colnames(rec_acl_ind), 
                 com_acl_ind[1,] + com_acl_ind[2,], 
                 rec_acl_ind[1,] + rec_acl_ind[2,],
                 sto_acl_ind[1,] + sto_acl_ind[2,]) |>
  type.convert(as.is = T) 


#Define header components for the data rows (ignore year). Fill in the blanks here.
indicator_names2 = c('Commercial ACLs', 'Recreational ACLs', 'Stock ACLs')
unit_names2 = rep('Percent Change',3)
extent_names2 = c('Commercial ACL', 'Recreational ACL', 'Stock ACL')

formatted_data2 = IEAnalyzeR::convert_cleaned_data(dat, 
                                                  indicator_names2, unit_names2, extent_names2)

ci_acl_pct <- IEAnalyzeR::convert_ci_data(dat_lci, dat_uci,
                                          indicator_names2, unit_names2, extent_names2)

#----------------------------------------------------
#### 3. Save Formatted data as csv ####
 
# This will save your data to the appropriate folder.

write.csv(formatted_data, file = csv_filename, row.names = F)
write.csv(formatted_data2, file = csv_filename2, row.names = F)

#----------------------------------------------------
#### 4. Create Data_Prep object ####
  
#Please use your formatted csv to create a "data_prep" object.
#For more info on the data_prep function see the vignette linked above.

data_obj <- IEAnalyzeR::data_prep(csv_filename)
data_obj2 <- IEAnalyzeR::data_prep(csv_filename2, ci_df = ci_acl_pct)


#----------------------------------------------------
#### 5. Save Formatted data_prep object ####

#This will save your data to the appropriate folder.

saveRDS(data_obj, file = object_filename)  
saveRDS(data_obj2, file = object_filename2)


#----------------------------------------------------
#### 6. Preview Plot ####
# Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# For more info on the plot_fn_obj function go HERE

IEAnalyzeR::plot_fn_obj(df_obj = data_obj, trend = TRUE, 
                        pts = T, pt_size = 1, 
                        fig.width = 10, 
                        facet_scales = 'fixed',
                        ncol = 2,
                        # manual_title = 'Annual Changes to ACLs',
                        sep_ylab = T,
                        ylab_sublabel = c('extent','unit'),) +
  scale_x_continuous(breaks = seq(2015, 2024, by = 1)) +
  theme(plot.title = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 12, face = 'plain'),
        axis.text = element_text(size = 10, face = 'plain'),
        strip.text = element_text(size = 12,face = "plain"))

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

ggsave(filename = plot_filename, height = 5, width = 10, unit="in")


#----------------------------------------------------
#### 6. Preview Plot ####
# Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# For more info on the plot_fn_obj function go HERE

IEAnalyzeR::plot_fn_obj(df_obj = data_obj2, trend = TRUE, 
                        pts = T, pt_size = 1, 
                        fig.width = 10, 
                        facet_scales = 'fixed',
                        ncol = 2, 
                        manual_title = 'Annual Changes to ACLs',
                        sep_ylab = T,
                        ylab_sublabel = c('extent','unit'),) +
  scale_x_continuous(breaks = seq(2015, 2024, by = 1)) +
  theme(plot.title = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 12, face = 'plain'),
        axis.text = element_text(size = 10, face = 'plain'),
        strip.text = element_text(size = 12,face = "plain"))

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

ggsave(filename = plot_filename2, height = 5, width = 10, unit="in")

