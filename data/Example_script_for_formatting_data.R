# Example code for formatting data in the correct indicator format

# 1. Dummy data
year = c(2000,2001,2002,2003,2004)
bio_spec1 = c(5,10,3,7,11)
bio_spec2 = c(7,3,8,8,12)
bio_spec3 = c(10,12,11,5,3)
dat = as.data.frame(cbind(year, bio_spec1, bio_spec2, bio_spec3))

# 2. Define ALL header components
indicator_names = c("", "Biomass", "Biomass", "Biomass") # Your desired header
unit_names = c("", "Numbers", "Numbers", "Numbers")           # Your Row 2 metadata
extent_names = c("", "species 1", "species 2", "species 3")  # Your Row 3 metadata

# 3. Create the data frame for R analysis:
#    a. Convert numeric data to character matrix for rbind
data_matrix = as.matrix(dat)
colnames(data_matrix) = NULL 

#    b. Combine the unit row and extent row with the numeric data
#       This puts the three metadata rows (Units, Extent, Data) into one block
data_block = rbind(indicator_names, unit_names, extent_names, data_matrix)
colnames(data_block) = NULL # Set the column names to the Indicator names

# 4. Convert to data frame
final_data_ready = as.data.frame(data_block, stringsAsFactors = FALSE)

# 5. Remove R's internal row names
rownames(final_data_ready) = NULL

# --- Final Structure Check ---
print(final_data_ready)
test = IEAnalyzeR::data_prep(final_data_ready, subind = "extent")

plot = IEAnalyzeR::plot_fn_obj(test)
