#' @title Import hatchery loss data
#' @description Import hatchery loss data from SacPAS CWT database tables -- use confirmed hatchery loss only.
#' @import dplyr
#' @import janitor
#' @import lubridate
#' @import here
#' @import usethis
#' @importFrom magrittr %>%


# Update required: link to confirmed hatchery loss .csv with all years-- currently set to only pull one year
# import hatchery loss directly from SacPAS
url <- "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1719846904_849.csv"
hatchery_loss_raw <- read.csv(url, header = TRUE, stringsAsFactors = FALSE)

# filter to only include winter-run chinook
hatchery_loss <- hatchery_loss_raw %>%
  janitor::clean_names() %>%
  filter(cwt_tag_race == "Winter") %>%
  mutate(date = ymd(release_start)) %>%
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
