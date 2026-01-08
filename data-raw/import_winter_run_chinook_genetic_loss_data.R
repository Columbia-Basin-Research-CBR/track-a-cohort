#'  @title Import  genetic loss data
#'  @description This script imports the genetic loss data to be used for cumulative, total loss, and percent loss of the JPE.
#'  The genetic loss data is joined with the JPE annual data to calculate cumulative loss and percent loss of the JPE.
#'  As of now, genetic loss data shared by BOR as a flat file `WRGenetic.csv`. The data shared ranges from 1996 to 2024. 
#'  However, SacPAS provides genetic data for winter-run chinook salmon from WY2020 onwards that has undergone a level of QA/QC. 
#'  Until otherwise stated, this script filters the BOR data to include only entries before the first SacPAS date and combines 
#'  the filtered BOR dataset and SacPAS generated data (i.e., pre WY2020 is BOR provided data and any data in or beyond WY2020 
#'  is provided by SacPAS (DNA information sourced from the CDFW Salvage Access DB, and Data Courtesy of California Department of Fish and Wildlife.)
#'  Note: currently All Years on SacPas allows some 2018 data-- SI to update. Remove note once complete. 
#'  @details To see a comparision of BOR and SacPAS merged data see `data-raw/utils_winter_run_chinook_compare_BOR_SacPAS_genetic_loss_data.R`
#'  @return List of data frames with genetic loss data: 1) cumulative loss and 2) total loss  saved in `data` folder. 

require(tidyverse)
require(usethis)
require(here)

# import non QA/QC'd genetic loss data shared via BOR flat file
df_genetic_loss_bor <- read.csv(here::here("data-raw/WRGenetic.csv")) %>%
  mutate(date = mdy(SampleDateTime)) %>%
  rename(loss = "Loss_GeneticData") %>%
  arrange(date) %>%
  mutate(
    WY = year(date) + (month(date) >= 10),
    wDay = if_else(month(date) >= 10, yday(date) - 273, yday(date) + 92),
    doy = yday(date),
    CY = year(date),
    wDate = if_else(month(date) >= 10, date + years(1), date)
  ) %>% 
  select(date, CY, WY, wDay, doy, wDate, loss)

#import genetic loss data QA/QC'd via SacPAS link

url <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=all&species=1%3Af&dnaOnly=yes&age=no"

# filter to include only winter-run chinook for winter-run for DNA
loss.wch <- read.csv(url) %>%
  filter(DNA.Race == "Winter") 

df_genetic_loss_sacpas <- loss.wch %>%
  janitor::clean_names() %>%
  select(sample_time, facility, dna_race, nfish, loss, length) %>%
  mutate(date = as_date(ymd_hms(sample_time))) %>%
  arrange(date) %>%
  mutate(
    WY = year(date) + (month(date) >= 10),
    wDay = if_else(month(date) >= 10, yday(date) - 273, yday(date) + 92),
    doy = yday(date),
    CY = year(date),
    wDate = if_else(month(date) >= 10, date + years(1), date)
  ) %>% 
  select( date, CY, WY, wDay, doy, wDate, loss)

min_sacpas_water_year <- 2010
min_sacpas_water_date <- as.Date(paste0(min_sacpas_water_year, "-10-01"))

# Filter BOR data to include only entries before the minimum date required to
# use SacPAS data: 10-01-2009
df_genetic_loss_bor_filtered <- df_genetic_loss_bor %>%
  filter(date < min_sacpas_water_date)

# Combine the filtered BOR dataset and SacPAS generated data
df_genetic_loss <- bind_rows(df_genetic_loss_bor_filtered, df_genetic_loss_sacpas)

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


#add hydro type classification and BiOp year designation -- used in ShinyApp
# load hydrological classification data
source(here::here("data-raw/utils_import_hydrological_classification_index.R"))

add_variables_df <- function(df) {
  df %>%
    mutate(status = case_when(
      WY < 2009 ~ 'Pre-2009 BiOp\n(1994 to 2008)',
      WY >= 2009 ~ '2009 & 2019 BiOp\n(2009 to present)'
    )) %>%
    mutate(status = factor(status, levels = c('Pre-2009 BiOp\n(1994 to 2008)', '2009 & 2019 BiOp\n(2009 to present)'))) %>%
    left_join(select(hydrological_classification_index, WY, Classification), by = "WY") %>%
    filter(!is.na(date)) %>%
    mutate( hydro_type = factor(Classification, 
                                levels = c("Wet", "Above Normal", "Below Normal", "Dry", "Critical")),
            hydro_type_grp = case_when( hydro_type %in% c("Wet", "Above Normal") ~ "Wet & Above Normal",
                                        hydro_type %in% c("Below Normal", "Dry", "Critical") ~ "Below Normal, Dry, & Critical"
            )
    )
}

# Apply transformation to each data frame
jpe_genetic_loss_data <- lapply(jpe_genetic_loss_data, add_variables_df)



# save data
usethis::use_data(jpe_genetic_loss_data, overwrite = TRUE)
