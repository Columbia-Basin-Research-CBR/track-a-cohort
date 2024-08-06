#' import JPE estimate data directly from SacPAS
#'  @return writes a .rda to the data folder including wrangled annual jpe estimate data 
require(tidyverse)
require(usethis)
require(here)
require(data.table)



# import JPE estimate data directly from SacPAS
jpe_url<- "https://www.cbr.washington.edu/sacramento/data/graphics/jpedata_Natural_1.txt"
jpe_raw <- data.table::fread(jpe_url) 

# wrangle data
jpe_annual_data <- jpe_raw %>% 
  mutate(method = stringr::str_replace(references_contributors, "Method 2 .*", "Method 2 (O'Farrell et al., 2018)")) %>% 
  mutate(WY = brood_year + 1) 

# save data to data folder
usethis::use_data(jpe_annual_data, overwrite = TRUE)
