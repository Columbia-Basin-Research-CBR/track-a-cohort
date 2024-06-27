#' import  genetic loss data and hydrological water year type
#'  @description This script imports the genetic loss data and hydrological water year type data, wrangles the data, and saves the data to the data folder. The genetic loss data is joined with the JPE annual data to calculate cumulative loss and percent loss of the JPE. The data is then joined with the water year type data to designate wet/dry year classification.
#'  Script to import cumulative loss for genetic loss data shared by BOR as a flat file `WRGenetic.csv`. To be updated with query link from SacPas when data becomes available. This scripts also  assigns water year hydrologic classification (data provided by CDEC, will be hosted by SacPas; update when available).*Note: Genetic data is not QA/QC'd prior to 2020 (data source: BOR)
#'  @details The script imports the genetic loss data shared by BOR as a flat file `WRGenetic.csv` and the water year type data shared by CDEC as a flat file `data` > `WYtype.csv`. To calculate cumulative genetic loss, the JPE data is imported via `data` >`JPE_annual_data.R` sourced via `data-raw` >`import_jpe_annual_data.R`. 
#'  
#'  @returns The data is saved to the data folder as `jpe_cumulative_loss_genetic_data.rda`.
#'  @import tidyverse (dplyr, readr, tidyr, lubridate)
#'  @import usethis
#'  @import here


# import genetic loss data (currently using BOR shared flat file)
df_genetic_loss <- read.csv(here::here("data-raw/WRgenetic.csv")) %>% 
  mutate(date= mdy(SampleDateTime)) %>%
  rename( loss = 'Loss_GeneticData') %>%
  arrange(date) %>%
    mutate(WY = year(date) + (month(date) >= 10),
           wDay = if_else(month(date) >= 10, yday(date) - 273, yday(date) + 92),
           doy = yday(date),
           CY = year(date),
           wDate = if_else(month(date) >= 10, date + years(1), date))

# load jpe annual estimates
load(here("data/jpe_annual_data.rda"))

# #join genetic loss data with jpe annual data to calculate cumulative loss and percent loss of the JPE
 df_cum_gen_loss <-df_genetic_loss %>%
  left_join(select(jpe_annual_data,value, WY), by = 'WY') %>%
  group_by(WY, wDay) %>%
  summarize(loss = sum(loss), 
            jpe = min(value)) %>% 
  ungroup() %>%
  group_by(WY) %>% 
  mutate(cum_loss = cumsum(loss)) %>%
  mutate(pct_genetic_cumloss_jpe = (cum_loss/jpe))

# load hydrologic water year classification-- will automate this process once data is available
wytype <- read.csv(here::here('data-raw/WYtype.csv')) %>% filter(Basin == "SacramentoValley")


# assign status to designate pre/post 2009 BiOp, join with wytype to designate wet/dry year
cumulativeGEN<- df_cum_gen_loss %>% 
  #designate pre/post 2009 BiOp
  mutate(status = case_when(WY < 2009 ~ 'Pre-2009 BiOp\n(1994 to 2008)',
                            WY > 2008 ~ '2009 & 2019 BiOp\n(2009 to present)')) %>%
  #set order of factor levels
  mutate(status = factor(status, levels = c('Pre-2009 BiOp\n(1994 to 2008)', '2009 & 2019 BiOp\n(2009 to present)'))) %>% 
  #join with wytype to designate wet/dry year (Hydrologic Classification Index?)
  left_join(select(wytype,WY, "hydro_type" = `Yr.type`) , by = 'WY') %>%
  # relabel wytype Yr.type to Wet/Dry -- add all?
  mutate( hydro_type = factor(hydro_type, levels = c("W", "AN", "BN", "D", "C"), labels = c("Wet", "Above Normal", "Below Normal", "Dry", "Critical")),
          hydro_type_grp = case_when(
            hydro_type %in% c("Wet", "Above Normal") ~ "Wet, Above Normal",
            hydro_type %in% c("Below Normal", "Dry", "Critical") ~ "Below Normal, Dry, & Critical" )
  )


# rename and save data
jpe_cumulative_loss_genetic_data <- cumulativeGEN
usethis::use_data(jpe_cumulative_loss_genetic_data, overwrite = TRUE)

