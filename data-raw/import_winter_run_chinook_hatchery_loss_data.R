#' @title Import hatchery loss data
#' @description Import hatchery loss data from SacPAS CWT database tables -- use confirmed hatchery loss only.
require(tidyverse)
require(janitor)
require(usethis)
require(here)

source(here("R/utils_fct_assign_current_water_year.R"))
current_year <- assign_current_water_year()


# Update required: link to confirmed hatchery loss .csv with all years-- currently set to only pull one year
# import hatchery loss directly from SacPAS
url <- paste0("https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=", current_year, "&species=1%3At&dnaOnly=no&age=no")
hatchery_loss_raw <- read.csv(url, header = TRUE, stringsAsFactors = FALSE) %>% 
  janitor::clean_names()

#fct to check if data is valid
is_valid_data <- function(data) {
  # Example check: Ensure the data frame is not empty and has the expected columns
  required_columns <- c("sample_time", "lad_race")
  all(required_columns %in% colnames(df))
}

# If the data is invalid, load data from the previous water year
if (!is_valid_data(hatchery_loss_raw)) {
  previous_url <- sub(paste0("year=", current_year), paste0("year=", current_year-1), url)
  hatchery_loss_raw <- read_csv(previous_url) %>%
    janitor::clean_names()
}

# filter to only include winter-run chinook
hatchery_loss <- hatchery_loss_raw %>%
  filter(lad_race == "winter") %>%
  mutate(date = ymd(cwt_release_start)) %>%
  arrange(date) %>%
  mutate(
    WY = year(date) + (month(date) >= 10),
    wDay = if_else(month(date) >= 10, yday(date) - 273, yday(date) + 92),
    doy = yday(date),
    CY = year(date),
    wDate = if_else(month(date) >= 10, date + years(1), date)
  )

# save data to data folder
winter_run_chinook_hatchery_loss_data <- hatchery_loss
usethis::use_data(winter_run_chinook_hatchery_loss_data, overwrite = TRUE)
