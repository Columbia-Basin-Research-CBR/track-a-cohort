#' @title Import hatchery loss data
#' @description Import hatchery loss data from SacPAS CWT database tables -- use confirmed hatchery loss only.
#' @import dplyr
#' @import janitor
#' @import lubridate
#' @import here
#' @import usethis
#' @importFrom magrittr %>%

source(here("R/utils_fct_assign_current_water_year.R"))
current_year <- assign_current_water_year()

# Update required: link to confirmed hatchery loss .csv with all years-- currently set to only pull one year
# import hatchery loss directly from SacPAS
url <- paste0("https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=", current_year, "&species=1%3At&dnaOnly=no&age=no")
hatchery_loss_raw <- read.csv(url, header = TRUE, stringsAsFactors = FALSE)

# filter to only include winter-run chinook
hatchery_loss <- hatchery_loss_raw %>%
  janitor::clean_names() %>%
  filter(lad_race == "Winter") %>%
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
