#' import JPE estimate data directly from SacPas
#'  @return writes a .rda to the data folder including wrangled annual jpe estimate data 
#'  @import tidyverse (dplyr)
#'  @import data.table
#'  @import usethis

#load dependencies
source(here("R/load_dependencies.R"))

# import JPE estimate data directly from SacPas
jpe_url<- "https://www.cbr.washington.edu/sacramento/data/graphics/jpedata_Natural_1.txt"
jpe_raw <- data.table::fread(jpe.url) 

# wrangle data
jpe_annual_data <- jpe_raw %>% 
  mutate(method = str_replace(references_contributors, "Method 2 .*", "Method 2 (O'Farrell et al., 2018)")) %>% 
  mutate(WY = brood_year + 1) 

# save data to data folder
usethis::use_data(jpe_annual_data, overwrite = TRUE)
