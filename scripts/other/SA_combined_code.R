### Code to combine all the stock assessment outputs by species into consolidated csv files. ###

library(dplyr)
library(janitor)
library(here)
library(purrr)
library(stringr)


### Biomass ###

# Set the folder where .csv files are stored
input_folder = here("data/formatted/formatted_csvs")

output_folder = here("data/intermediate")

# List all csv files that contain SEDAR and Biomass
files <- list.files(input_folder, full.names = TRUE)
files <- files[grepl("SEDAR", files) & grepl("Biomass", files)]

# Read and combine
biomass_df_list <- map(files, function(file) {
  # Read the .rds file
  dat <- read.csv(file)
  
  # Extract species_assessment from filename
  filename <- basename(file)
  name <- str_remove(filename, "_Biomass_formatted\\.csv$")  # remove suffix
  
  #remove metadata rows
  dat = dat[-c(1,2),] 
  
})

# Merge all data frames by Yr
# Reduce joins all the data frames by Yr column
combined_biomass <- reduce(biomass_df_list, full_join, by = "indicator")

# Arrange by year
combined_biomass <- combined_biomass %>% arrange(indicator)

write.csv(combined_biomass, file = file.path(output_folder, "combined_biomass_trends.csv"), row.names = FALSE)




### Recruitment deviations ###

# Set the folder where .csv files are stored
input_folder = here("data/formatted/formatted_csvs")

output_folder = here("data/intermediate")

# List all csv files that contain SEDAR and Biomass
files <- list.files(input_folder, full.names = TRUE)
files <- files[grepl("SEDAR", files) & grepl("RecDev", files)]

# Read and combine
recdev_df_list <- map(files, function(file) {
  # Read the .rds file
  dat <- read.csv(file)
  
  # Extract species_assessment from filename
  filename <- basename(file)
  name <- str_remove(filename, "_RecDev_formatted\\.csv$")  # remove suffix
  
  #remove metadata rows
  dat = dat[-c(1,2),] 
  
})

# Merge all data frames by Yr
# Reduce joins all the data frames by Yr column
combined_recdev <- reduce(recdev_df_list, full_join, by = "indicator")

# Arrange by year
combined_recdev <- combined_recdev %>% arrange(indicator)

write.csv(combined_recdev, file = file.path(output_folder, "combined_recdev_trends.csv"), row.names = FALSE)




### Commercial landings:discard ratio ###

# Set the folder where .csv files are stored
input_folder = here("data/formatted/formatted_csvs")

output_folder = here("data/intermediate")

# List all csv files that contain SEDAR and Biomass
files <- list.files(input_folder, full.names = TRUE)
files <- files[grepl("SEDAR", files) & grepl("Commercial", files) & grepl("Catch", files)]

# Read and combine
com_df_list <- map(files, function(file) {
 
  dat <- read.csv(file, stringsAsFactors = FALSE)
  
  dat = dat %>% 
    row_to_names(row_number = 2) %>% 
    rename(year = extent) 
  
  target_col = "commercial_ratio"
  
  if (target_col %in% names(dat)) {
    
    # Extract species_assessment from filename
    filename <- basename(file)
    name <- str_remove(filename, "_Commercial_Catch_formatted\\.csv$")  # remove suffix
    
    dat = dat %>% 
      select(year, all_of(target_col)) %>% 
      mutate(year = as.integer(year), 
             !!target_col := as.numeric(get(target_col))) %>% 
      rename(!!name := target_col)
    
    return(dat)
    
  } else {
    
      # Column does NOT exist: Return NULL to ignore this file
      message(paste("File ignored (missing column):", basename(file)))
      return(NULL)
    
  }
  
})

com_df_list = purrr::compact(com_df_list)

# Merge all data frames by Yr
# Reduce joins all the data frames by Yr column
combined_com <- reduce(com_df_list, full_join, by = "year")

# Arrange by year
combined_com <- combined_com %>% arrange(year)

write.csv(combined_com, file = file.path(output_folder, "combined_commercial_ratio_trends.csv"), row.names = FALSE)





### Recreational landings:discards ratio ###

# Set the folder where .csv files are stored
input_folder = here("data/formatted/formatted_csvs")

output_folder = here("data/intermediate")

# List all csv files that contain SEDAR and Biomass
files <- list.files(input_folder, full.names = TRUE)
files <- files[grepl("SEDAR", files) & grepl("Recreational", files) & grepl("Catch", files)]

# Read and combine
rec_df_list <- map(files, function(file) {
  
  dat <- read.csv(file, stringsAsFactors = FALSE)
  
  dat = dat %>% 
    row_to_names(row_number = 2) %>% 
    rename(year = extent) 
  
  target_col = "recreational_ratio"
  
  if (target_col %in% names(dat)) {
    
    # Extract species_assessment from filename
    filename <- basename(file)
    name <- str_remove(filename, "_Recreational_Catch_formatted\\.csv$")  # remove suffix
    
    dat = dat %>% 
      select(year, all_of(target_col)) %>% 
      mutate(year = as.integer(year), 
             !!target_col := as.numeric(get(target_col))) %>% 
      rename(!!name := target_col)
    
    return(dat)
    
  } else {
    
    # Column does NOT exist: Return NULL to ignore this file
    message(paste("File ignored (missing column):", basename(file)))
    return(NULL)
    
  }
  
})

rec_df_list = purrr::compact(rec_df_list)

# Merge all data frames by Yr
# Reduce joins all the data frames by Yr column
combined_rec <- reduce(rec_df_list, full_join, by = "year")

# Arrange by year
combined_rec <- combined_rec %>% arrange(year)

write.csv(combined_rec, file = file.path(output_folder, "combined_recreational_ratio_trends.csv"), row.names = FALSE)






### Commercial landings ###

# Set the folder where .csv files are stored
input_folder = here("data/formatted/formatted_csvs")

output_folder = here("data/intermediate")

# List all csv files that contain SEDAR and Biomass
files <- list.files(input_folder, full.names = TRUE)
files <- files[grepl("SEDAR", files) & grepl("Commercial", files) & grepl("Catch", files)]

# Read and combine
com_df_list <- map(files, function(file) {
  
  dat <- read.csv(file, stringsAsFactors = FALSE)
  
  dat = dat %>% 
    row_to_names(row_number = 2) %>% 
    rename(year = extent) 
  
  target_col = "commercial_landings"
  
  if (target_col %in% names(dat)) {
    
    # Extract species_assessment from filename
    filename <- basename(file)
    name <- str_remove(filename, "_Commercial_Catch_formatted\\.csv$")  # remove suffix
    
    dat = dat %>% 
      select(year, all_of(target_col)) %>% 
      mutate(year = as.integer(year), 
             !!target_col := as.numeric(get(target_col))) %>% 
      rename(!!name := target_col)
    
    return(dat)
    
  } else {
    
    # Column does NOT exist: Return NULL to ignore this file
    message(paste("File ignored (missing column):", basename(file)))
    return(NULL)
    
  }
  
})

com_df_list = purrr::compact(com_df_list)

# Merge all data frames by Yr
# Reduce joins all the data frames by Yr column
combined_com <- reduce(com_df_list, full_join, by = "year")

# Arrange by year
combined_com <- combined_com %>% arrange(year)

write.csv(combined_com, file = file.path(output_folder, "combined_commercial_landings_trends.csv"), row.names = FALSE)





### Recreational landings:discards ratio ###

# Set the folder where .csv files are stored
input_folder = here("data/formatted/formatted_csvs")

output_folder = here("data/intermediate")

# List all csv files that contain SEDAR and Biomass
files <- list.files(input_folder, full.names = TRUE)
files <- files[grepl("SEDAR", files) & grepl("Recreational", files) & grepl("Catch", files)]

# Read and combine
rec_df_list <- map(files, function(file) {
  
  dat <- read.csv(file, stringsAsFactors = FALSE)
  
  dat = dat %>% 
    row_to_names(row_number = 2) %>% 
    rename(year = extent) 
  
  target_col = "recreational_landings"
  
  if (target_col %in% names(dat)) {
    
    # Extract species_assessment from filename
    filename <- basename(file)
    name <- str_remove(filename, "_Recreational_Catch_formatted\\.csv$")  # remove suffix
    
    dat = dat %>% 
      select(year, all_of(target_col)) %>% 
      mutate(year = as.integer(year), 
             !!target_col := as.numeric(get(target_col))) %>% 
      rename(!!name := target_col)
    
    return(dat)
    
  } else {
    
    # Column does NOT exist: Return NULL to ignore this file
    message(paste("File ignored (missing column):", basename(file)))
    return(NULL)
    
  }
  
})

rec_df_list = purrr::compact(rec_df_list)

# Merge all data frames by Yr
# Reduce joins all the data frames by Yr column
combined_rec <- reduce(rec_df_list, full_join, by = "year")

# Arrange by year
combined_rec <- combined_rec %>% arrange(year)

write.csv(combined_rec, file = file.path(output_folder, "combined_recreational_landings_trends.csv"), row.names = FALSE)


#END SCRIPT