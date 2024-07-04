#' import  genetic loss data
#'  @description This script imports the genetic loss data to be used for cumulative, total loss, and percent loss of the JPE.
#'  The genetic loss data is joined with the JPE annual data to calculate cumulative loss and percent loss of the JPE.
#'  As of now, genetic loss data shared by BOR as a flat file `WRGenetic.csv`.
#'  To be updated with query link from SacPAS when data becomes available.
#'  @import dplyr
#'  @import lubridate
#'  @import usethis
#'  @import here
#'  @importFrom magrittr %>%


# import genetic loss data (currently using BOR shared flat file)
df_genetic_loss <- read.csv(here::here("data-raw/WRgenetic.csv")) %>%
  mutate(date = mdy(SampleDateTime)) %>%
  rename(loss = "Loss_GeneticData") %>%
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

# join genetic loss data with jpe annual data to calculate cumulative loss and percent loss of the JPE
genetic_cumulative_loss_data <- df_genetic_loss %>%
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
genetic_total_loss_data <- df_genetic_loss %>%
  left_join(select(jpe_annual_data, value, WY), by = "WY") %>%
  group_by(WY) %>%
  summarize(
    total_loss = sum(loss),
    jpe = min(value)
  ) %>%
  mutate(pct_total_loss = (total_loss / jpe))



# consolidate data into list
jpe_genetic_loss_data <- list(
  genetic_cumulative_loss_data = genetic_cumulative_loss_data,
  genetic_total_loss_data = genetic_total_loss_data
)


# save data
usethis::use_data(jpe_genetic_loss_data, overwrite = TRUE)
