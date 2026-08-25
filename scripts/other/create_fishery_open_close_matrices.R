suppressPackageStartupMessages({
  library(dplyr)
  library(httr)
  library(lubridate)
  library(tidyr)
})

data_url <- paste0(
  "https://raw.githubusercontent.com/Gulf-IEA/Gulf-ESR/",
  "main/data/intermediate/spp_open_close_output.RData"
)

script_argument <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_argument) != 1) {
  stop("Run this file with Rscript.")
}

script_path <- normalizePath(sub("^--file=", "", script_argument))
repository_root <- normalizePath(file.path(dirname(script_path), "..", ".."))
data_path <- file.path(
  repository_root,
  "data",
  "intermediate",
  "spp_open_close_output.RData"
)
output_directory <- file.path(
  repository_root,
  "data",
  "formatted",
  "formatted_csvs",
  "fishery_open_close"
)

dir.create(dirname(data_path), recursive = TRUE, showWarnings = FALSE)
response <- GET(data_url, write_disk(data_path, overwrite = TRUE))
stop_for_status(response)

data_environment <- new.env(parent = emptyenv())
loaded_objects <- load(data_path, envir = data_environment)

if (!"output" %in% loaded_objects || !is.data.frame(data_environment$output)) {
  stop("The downloaded file does not contain the expected 'output' data frame.")
}

required_columns <- c(
  "COMMON_NAME_USE",
  "SECTOR_USE",
  "YEAR",
  "VALUE",
  "start",
  "end"
)
fishery_intervals <- data_environment$output
missing_columns <- setdiff(required_columns, names(fishery_intervals))

if (length(missing_columns) > 0) {
  stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
}

valid_sectors <- c("COMMERCIAL", "RECREATIONAL")
valid_statuses <- c("OPEN", "CLOSE")

if (any(!fishery_intervals$SECTOR_USE %in% valid_sectors)) {
  stop("Unexpected values found in SECTOR_USE.")
}
if (any(!fishery_intervals$VALUE %in% valid_statuses)) {
  stop("Unexpected values found in VALUE.")
}

fishery_days <- fishery_intervals %>%
  transmute(
    species = COMMON_NAME_USE,
    sector = SECTOR_USE,
    reported_year = as.integer(YEAR),
    open = as.integer(VALUE == "OPEN"),
    start = as.Date(start),
    end = as.Date(end)
  ) %>%
  filter(!is.na(start), !is.na(end), end >= start) %>%
  mutate(date = Map(seq, start, end, MoreArgs = list(by = "day"))) %>%
  select(-start, -end) %>%
  unnest_longer(date) %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    day_of_year = yday(date)
  )

if (any(fishery_days$year != fishery_days$reported_year)) {
  stop("At least one interval extends outside its reported YEAR.")
}

daily_status <- fishery_days %>%
  group_by(species, sector, year, day_of_year) %>%
  summarise(open = min(open), .groups = "drop")

dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
unlink(
  file.path(
    output_directory,
    c("recreational_matrix_*.csv", "commercial_matrix_*.csv")
  )
)

sector_years <- daily_status %>%
  distinct(sector, year) %>%
  arrange(sector, year)

annual_averages <- vector("list", nrow(sector_years))

for (row_number in seq_len(nrow(sector_years))) {
  current_sector <- sector_years$sector[[row_number]]
  current_year <- sector_years$year[[row_number]]
  days_in_year <- if (leap_year(current_year)) 366L else 365L

  status_matrix <- daily_status %>%
    filter(sector == current_sector, year == current_year) %>%
    select(species, day_of_year, open) %>%
    complete(species, day_of_year = seq_len(days_in_year), fill = list(open = 0L)) %>%
    arrange(species, day_of_year) %>%
    pivot_wider(
      names_from = day_of_year,
      values_from = open,
      values_fill = 0L
    )

  annual_averages[[row_number]] <- tibble(
    sector = current_sector,
    year = current_year,
    average_open_species = mean(colSums(status_matrix[-1]))
  )

  output_name <- sprintf(
    "%s_matrix_%d.csv",
    tolower(current_sector),
    current_year
  )
  write.csv(
    status_matrix,
    file.path(output_directory, output_name),
    row.names = FALSE,
    na = "0"
  )
}

annual_summary <- bind_rows(annual_averages) %>%
  mutate(sector = tolower(sector)) %>%
  pivot_wider(
    names_from = sector,
    values_from = average_open_species
  ) %>%
  arrange(year)

write.csv(
  annual_summary,
  file.path(output_directory, "annual_average_open_species.csv"),
  row.names = FALSE
)

message(
  "Created ",
  nrow(sector_years),
  " matrices and an annual summary in ",
  output_directory
)
