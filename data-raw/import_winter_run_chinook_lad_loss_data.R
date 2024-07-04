#' @title import LAD loss data for winter-run chinook
#' @description This script includes wrangling of lad data imported directly from SacPAS.
#' The lad loss data is joined with the JPE annual data to calculate cumulative loss and percent loss of the JPE.
#' Needs confirmation of numbers/link from SI/MC since numbers differ from BOR shared flat file.
#'  @details To calculate cumulative genetic loss, the JPE data is imported via `data` >`JPE_annual_data.R` wrangled via `data-raw` >`import_jpe_annual_data.R`.
#' @return writes a .rda list  with two data frames: 1) cumulative lad loss pct and numerical, and 2) total lad loss, pct and numerical. Final .rda is saved to the `data` folder
#' @import dplyr
#' @import lubridate
#' @import janitor
#' @import usethis
#' @importFrom magrittr %>%


# import LAD loss directly from SacPAS (include genetic when available)

url <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=default&year=all&species=1%3Af&dnaOnly=no&age=no"

# filter to include only winter-run chinook for LAD or winter-run for DNA
loss.wch <- read.csv(url) %>%
  filter(LAD.Race == "Winter") # DNA.Race == 'Winter'

# adjust imported loss to WY
df_lad_loss <- loss.wch %>%
  select(Sample.Time, Facility, LAD.Race, DNA.Race, nfish, Loss) %>%
  janitor::clean_names() %>%
  mutate(date = ymd_hms(sample_time)) %>%
  arrange(date) %>%
  mutate(
    WY = year(date) + (month(date) >= 10),
    wDay = if_else(month(date) >= 10, yday(date) - 273, yday(date) + 92),
    doy = yday(date),
    CY = year(date),
    wDate = if_else(month(date) >= 10, date + years(1), date)
  )

# load jpe annual estimates
load(here::here("data/jpe_annual_data.rda"))


# Filter loss to include only LAD loss for winter-run chinook
lad_cumulative_loss_data <- df_lad_loss %>%
  left_join(select(jpe_annual_data, value, WY), by = "WY") %>%
  group_by(WY, wDay) %>%
  summarize(
    loss = sum(loss),
    jpe = min(value)
  ) %>%
  ungroup() %>%
  group_by(WY) %>%
  mutate(cumloss = cumsum(loss)) %>%
  mutate(pct_cumloss = (cumloss / jpe))


# Calculate total loss
lad_total_loss_data <- df_lad_loss %>%
  left_join(select(jpe_annual_data, value, WY), by = "WY") %>%
  group_by(WY) %>%
  summarize(
    total_loss = sum(loss),
    jpe = min(value)
  ) %>%
  mutate(pct_total_loss = (total_loss / jpe))


# consolidate data into list
jpe_lad_loss_data <- list(
  lad_cumulative_loss_data = lad_cumulative_loss_data,
  lad_total_loss_data = lad_total_loss_data
)

# save data
usethis::use_data(jpe_lad_loss_data, overwrite = TRUE)
