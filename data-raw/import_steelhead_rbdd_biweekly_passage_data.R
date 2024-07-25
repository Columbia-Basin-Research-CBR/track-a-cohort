#' @title Import Steelhead Bi-Weekly Passage data from SacPAS generated URL
#' @description import Steelhead Bi-Weekly Passage data from SacPAS generated URL using the current day to extract current year data from the SacPAS website
#' @return .rda of biweekly data for current calendar year saved in `data` folder. 
require(tidyverse)
require(janitor)
require(usethis)




# load data via SacPAS URL

# select current year data 
current_year <- year(today())

# Concatenate the current year into the URL
url <- paste0("https://www.cbr.washington.edu/sacramento/data/php/rpt/redbluff_graph.php?sc=1&outputFormat=csv&byear=",current_year,"&species=Steelhead%3ANA&addData=biweek")

# Read in the data
rbdd_biweekly_raw <-read.csv(url)

# clean data
rbdd_biweekly <- rbdd_biweekly_raw %>% 
  janitor::clean_names() %>% 
  rename( biweek_total = biweek_total_2_3) %>% 
  mutate(date = ymd(date_yyyy_mm_dd)) %>% 
  filter(!is.na(date))

# save the data
rbdd_biweekly_passage_data <- rbdd_biweekly
usethis::use_data(rbdd_biweekly_passage_data, overwrite = TRUE)

