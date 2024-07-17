#' @title Import steelhead loss from SacPAS
#' @description This script imports steelhead loss data from the Sacramento River using the SacPAS website. Confirm link is correct
require(tidyverse)
require(janitor)
require(usethis)
require(here)

# load in data from url (confirm url is correct information)
url <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=default&year=all&species=2%3Aall&dnaOnly=no&age=no"

steelhead_loss_raw<- read.csv(url, header = TRUE, stringsAsFactors = FALSE)

# load hydrological classification data
wytype <- read.csv(here("data/WYtype.csv"))

steelhead_loss_data<- steelhead_loss_raw %>% 
  janitor::clean_names() %>% 
  select(sample_time, facility, adipose_clip, length, nfish, loss) %>%
  mutate(date = ymd_hms(sample_time)) %>%
  arrange(date) %>%
  mutate(WY = year(date) + (month(date) >= 10),
         wDay = if_else(month(date) >= 10, yday(date) - 273, yday(date) + 92),
         doy = yday(date),
         CY = year(date),
         wdate = if_else(month(date) >= 10, date + years(1), date)) %>% 
  group_by(WY, adipose_clip) %>% 
  mutate(total_loss = sum(loss)) %>%
  ungroup() %>% 
  group_by(WY) %>%
  mutate(cumloss = cumsum(loss)) %>%
  mutate(status = case_when(WY < 2009 ~ 'Pre-2009 BiOp\n(1994 to 2008)',
                            WY > 2008 ~ '2009 & 2019 BiOp\n(2009 to present)')) %>%
  #set order of factor levels
  mutate(status = factor(status, levels = c('Pre-2009 BiOp\n(1994 to 2008)', '2009 & 2019 BiOp\n(2009 to present)'))) %>% 
  left_join(select(wytype,WY, Yr.type) , by = "WY") %>%
  filter(!is.na(date)) %>%
  # create hydrological classification
  mutate( hydro_type = factor(Yr.type, 
                              levels = c("W", "AN", "BN", "D", "C"), 
                              labels = c("Wet", "Above Normal", "Below Normal", "Dry", "Critical")),
          hydro_type_grp = case_when( hydro_type %in% c("Wet", "Above Normal") ~ "Wet, Above Normal",
                                      hydro_type %in% c("Below Normal", "Dry", "Critical") ~ "Below Normal, Dry, & Critical"
                                     )
  ) %>% 
  select(-c(sample_time, doy, CY,Yr.type)) %>% 
  filter(adipose_clip != "") #remove blank adipose clips

# save data
usethis::use_data(steelhead_loss_data, overwrite = TRUE)

