#' @title import steelhead loss and export data from SacPAS
#' @description This script is used to import data via query url for export per facility and loss per facility to be used in the steelhead Figure 6 barplot and linegraph.
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
source(here("R/utils_fct_import_river_data.R"))

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
years <- print(previousWY:currentWY)

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

# import OMR data from SacPAS river conditions-- need to confirm rule for which OMR values are retained to be plotted as verticle lines
df_OMR_raw <- fct_import_SacPAS_river_conditions_query(sites = "Combined", years = years, metrics = "OMRDailyUSGS") 

df_OMR <- df_OMR_raw %>% 
mutate(WY = year(YMD) + if_else(month(YMD) >= 10, 1, 0)) %>% # Calculate Water Year
  filter(WY == current_year) %>%  # Filter for current water year
  select( YMD, "OMR" = value)

df_OMR14d_raw <- fct_import_SacPAS_river_conditions_query(sites = "Combined", years = years, metrics = "OMR14DayUSGS") # "OMRDailyUSGS"

df_OMR_14d <- df_OMR14d_raw %>% 
  mutate(WY = year(YMD) + if_else(month(YMD) >= 10, 1, 0)) %>% # Calculate Water Year
  filter(WY == current_year) %>%  # Filter for current water year
  select( YMD, "OMR_14d" = value) %>% 
  arrange(YMD)

df_OMR_min14d_month <- df_OMR_14d %>% 
  # mutate(OMR_14d_avg_change = OMR_14d - lag(OMR_14d)) %>% 
  # filter(OMR_14d_avg_change < -2500) %>% 
  select(YMD, OMR_14d) %>% 
  mutate(month = month(YMD)) %>%
  group_by(month) %>% 
  slice(which.min(OMR_14d)) %>% 
  ungroup()



# combine loss and export into final list data
steelhead_loss_export_data <- current_year_steelhead_loss_data %>% 
  left_join(select(df_river, site, YMD, "pumping_discharge_cfs" = value), by = c("facility" = "site", "date" = "YMD")) %>% 
  left_join(select(df_OMR_min14d_month, OMR_14d, YMD), by = c( "date" = "YMD")) %>% 
  left_join(select(df_OMR, OMR, YMD), by = c( "date" = "YMD")) 
  

# Save the data
usethis::use_data(steelhead_loss_export_data, overwrite = TRUE)
