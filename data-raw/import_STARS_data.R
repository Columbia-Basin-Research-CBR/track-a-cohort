#'import STARS data from ShinyApp
#'@description This script imports up to date STARS data from the ShinyApp and saves it to the data folder for use in other scripts. Code provided by Nick Beer.
#'@return STARS_data.rda
require(tidyverse)
require(usethis)
require(here)
require(xts)
require(zoo)


# grab.stars.R

# /var/httpd/www/shiny/apps/STARS/STARS.shinyinputs.Rdata
# heads up, this object has class: "xts" and "zoo"

#load data 
load(here::here("data-raw/STARS.shinyinputs.Rdata")) 


# Subset the data and convert to tibble
df_stars_raw <- tibble::as_tibble(WR_xts[,c("Survival Interior Delta Est", 
                                    "Survival Interior Delta LCL 80", 
                                    "Survival Interior Delta UCL 80",
                                    "Routing Probability Interior Delta Est",    
                                    "Routing Probability Interior Delta LCL 80",
                                    "Routing Probability Interior Delta UCL 80",
                                    "Survival Overall Est", 
                                    "Survival Overall LCL 80",
                                    "Survival Overall UCL 80")]) %>%
  # Add the first date as a new column
  mutate(date = zoo::index(WR_xts)) %>%
  # Make date the first column
  select(date, everything()) %>%
  rename( surv =  "Survival Overall Est", survL80 =  "Survival Overall LCL 80", survU80 =  "Survival Overall UCL 80", 
          idsurv = "Survival Interior Delta Est", idsurvL80 = "Survival Interior Delta LCL 80", idsurvU80 = "Survival Interior Delta UCL 80", 
          idRoute = "Routing Probability Interior Delta Est", idRouteL80 =  "Routing Probability Interior Delta LCL 80", idRouteU80 =  "Routing Probability Interior Delta UCL 80") %>%
  # convert date to WY, wday
  arrange(date) %>% 
  mutate(WY = year(date) + (month(date) >= 10),
         wDay = if_else(month(date) >= 10, yday(date) - 273, yday(date) + 92),
         doy = yday(date),
         CY = year(date),
         wDate = if_else(month(date) >= 10, date + years(1), date)) 


#rename and save to data folder for use in other scripts
STARS_data<-df_stars_raw

usethis::use_data(STARS_data, overwrite = TRUE)
