#' @title Import Steelhead Bi-Weekly Passage data from SacPas generated URL
#' @import dplyr
#' @import lubridate
#' @import janitor
#' @import usethis
#' @importFrom magrittr %>%


# load data via URL

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

