#' @title import steelhead loss and export data from SacPAS
#' @description This script is used to import daily loss, export per facility, and OMRI values to be used in the steelhead Figure 6 barplot and linegraph.
#'
#' @import dplyr
#' @import lubridate
#' @import janitor
#' @import usethis
#' @importFrom magrittr %>%
#' 

source(here("R/utils_fct_assign_current_water_year.R"))
current_year <- assign_current_water_year()

#import loss data from SacPAS -- see import_steelhead_loss_data.R for more details
load(here("data/steelhead_loss_data.rda"))

current_year_steelhead_loss_data <- steelhead_loss_data %>% 
  filter(WY == current_year,
         adipose_clip == "Unclipped") %>% 
  mutate( date = as_date(date)) %>% 
  group_by(facility,date) %>% 
  summarise(daily_total_loss = sum(loss))


# import export date from SacPAS river conditions
source(here("data-raw/utils_fct_import_river_data.R"))

  # select export sites
  code_list <- c("TRP", "HRO") # TRP site code for CVP and HRO site code for SWP facilities
  
  # set year of interest
  
  # To get current WY years of data:
  today <- Sys.Date()
  # Determine the current and previous water years based on today's date
  if (format(today, "%m") >= "10") {
    currentWY <- as.numeric(format(today, "%Y")) + 1
  } else {
    currentWY <- as.numeric(format(today, "%Y"))
  }
  previousWY <- currentWY - 1
  # return years of interest
  years <- previousWY:currentWY
  
  years <- c(years)
  
  # selected metrics of interest
  metrics <- c("PumpingDischarge")
  
  # return river data frame based on selected variables
  df_river_raw<-fct_import_SacPAS_river_conditions_query(sites = code_list, years = years, metrics = metrics)
  
  df_river <- df_river_raw %>% 
    mutate(WY = year(YMD) + if_else(month(YMD) >= 10, 1, 0)) %>% # Calculate Water Year
    filter(WY == current_year) %>%  # Filter for current water year
    mutate(site = case_when(
     location == "TRP" ~ "CVP",
     location == "HRO" ~ "SWP"
    )) %>%
    select(site, YMD, value)
  
  # import OMRI data from SacPAS river conditions
  df_OMRI_raw <- fct_import_SacPAS_river_conditions_query(sites = "Combined", years = years, metrics = "OMRIndex") 
  
  df_OMRI <- df_OMRI_raw %>% 
  mutate(WY = year(YMD) + if_else(month(YMD) >= 10, 1, 0)) %>% 
    filter(WY == current_year) %>%  
  select( YMD, "OMRI" = value)


# combine loss and export into final list data
steelhead_loss_export_data <- current_year_steelhead_loss_data %>% 
  left_join(select(df_river, site, YMD, "pumping_discharge_cfs" = value), by = c("facility" = "site", "date" = "YMD")) %>% 
  left_join(select(df_OMRI, OMRI, YMD), by = c( "date" = "YMD")) 
  

# Save the data
usethis::use_data(steelhead_loss_export_data, overwrite = TRUE)
