# Code to put all of the draft figures from the stock assessment output metrics into a ppt presentation

# Load the required libraries
library(officer)
library(tidyverse)
library(stringr)

# Define the directory containing the plots
plot_dir <- "figures/plots"

# Define the four specific plot types we are looking for
# NOTE: The plot names are now fixed to include the preceding underscore, 
# which is the consistent separator.
plot_type_patterns <- c(
  "_Biomass_plot\\.png$",
  "_Commercial_Catch_plot\\.png$",
  "_Recreational_Catch_plot\\.png$",
  "_RecDev_plot\\.png$"
)
# Define keys for layout placement
plot_type_keys <- c(
  "Biomass", 
  "Commercial_Catch", 
  "Recreational_Catch", 
  "Recruitment_Deviation"
)


# --- 1. List and Filter Files using a Strict Pattern ---

# Regular expression to match ONLY the desired files. 
# It finds any file that ends with one of the four patterns.
pattern_regex <- paste0("(", paste(plot_type_patterns, collapse = "|"), ")")

all_files <- list.files(
  path = plot_dir,
  pattern = pattern_regex, # Use the strict pattern to ignore other plots
  full.names = TRUE
)

if (length(all_files) == 0) {
  stop("No files matching the required pattern were found in the directory '", plot_dir, "'.")
}


# --- 2. Create a data frame and robustly parse the components ---
plot_data <- tibble(file_path = all_files) %>%
  mutate(
    file_name = basename(file_path),
    
    # 2a. Identify the specific plot type suffix (e.g., "_RecDev_plot.png")
    plot_suffix = str_extract(file_name, pattern_regex),
    
    # 2b. Extract the Species_Assessment ID: everything before the plot suffix
    species_assessment_id = str_remove(file_name, plot_suffix),
    
    # 2c. Robustly split the ID into Species and Assessment:
    # The Assessment ID is the segment between the LAST space or underscore and the end.
    
    # Assessment is the string after the LAST underscore/space in the ID
    assessment = str_extract(species_assessment_id, "[^_ ]+$"),
    
    # Species is the string BEFORE the LAST underscore/space in the ID
    species = str_extract(species_assessment_id, ".*(?=([_ ])[^_ ]+$)"),
    
    # Handle the case where there is no separator (unlikely here, but safer)
    species = if_else(is.na(species), species_assessment_id, species),
    assessment = if_else(is.na(assessment), "Unknown", assessment),
    
    # 2d. Standardize the plot type for easier layout placement
    plot_type_key = case_when(
      str_detect(plot_suffix, "_Biomass_plot") ~ "Biomass",
      str_detect(plot_suffix, "_Commercial_Catch_plot") ~ "Commercial_Catch",
      str_detect(plot_suffix, "_Recreational_Catch_plot") ~ "Recreational_Catch",
      str_detect(plot_suffix, "_RecDev_plot") ~ "Recruitment_Deviation"
    )
  )

# --- 3. Get the unique combinations (each one will be a slide) ---
unique_slide_ids <- unique(plot_data$species_assessment_id)


# --- 4. Define the layout parameters for the 2x2 grid ---
# (Dimensions in inches)
slide_width <- 10
slide_height <- 7.5

plot_dims <- list(width = 0.45 * slide_width, height = 0.40 * slide_height)

# Coordinates for the 2x2 grid, using the standardized plot_type_key
plot_positions <- list(
  Biomass = list(left = 0.2, top = 0.75, width = plot_dims$width, height = plot_dims$height),
  Commercial_Catch = list(left = 0.2 + plot_dims$width + 0.2, top = 0.75, width = plot_dims$width, height = plot_dims$height),

  Recreational_Catch = list(left = 0.2, top = 0.75 + plot_dims$height + 0.25, width = plot_dims$width, height = plot_dims$height),
  Recruitment_Deviation = list(left = 0.2 + plot_dims$width + 0.2, top = 0.75 + plot_dims$height + 0.25, width = plot_dims$width, height = plot_dims$height)
)

# Create a new, empty presentation
doc <- read_pptx()

# --- 5. Loop through each unique combination to create a slide ---
for (current_id in unique_slide_ids) {
  
  current_plots <- plot_data %>%
    filter(species_assessment_id == current_id)
  
  # Create a clean, readable slide title: "Species Name (Assessment Name)"
  # The species name is kept as extracted (with spaces or underscores preserved)
  slide_title <- paste0(current_plots$species[1], " (", current_plots$assessment[1], ")")
  
  doc <- doc %>%
    add_slide(layout = "Blank", master = "Office Theme") 
  
  # Add each of the available 4 plots to the slide
  for (i in 1:nrow(current_plots)) {
    plot_info <- current_plots[i, ]
    type_key <- plot_info$plot_type_key
    file_path <- plot_info$file_path
    
    # Place the plot at the defined position
    if (type_key %in% names(plot_positions)) {
      pos <- plot_positions[[type_key]]
      
      doc <- doc %>%
        ph_with(
          value = external_img(src = file_path),
          # Use ph_location() to define the exact position and size
          location = ph_location(
            left = pos$left, 
            top = pos$top, 
            width = pos$width, 
            height = pos$height
        )
      )
    }
  }
}

# --- 6. Save the final presentation ---
output_file <- "presentations/Assessment_output_plots_draft.pptx"
print(doc, target = output_file)

message(paste("✅ PowerPoint presentation created successfully:", output_file))
