#' @title import steelhead loss and export data from SacPAS
#' @description This script is used to import daily loss, export per facility, and OMRI values to be used in the steelhead Figure 6 barplot and linegraph.
#' @return .rda of steelhead daily loss and export data saved in `data` folder.
require(tidyverse)
require(janitor)
require(usethis)
require(here)

source(here("R/utils_fct_assign_current_water_year.R"))
current_year <- assign_current_water_year()
previous_year <- current_year - 1

# Import loss data from SacPAS -- see import_steelhead_loss_data.R for more details (based on WY)
load(here("data/steelhead_loss_data.rda"))

# Filter to include current water year and unclipped fish only
current_year_steelhead_loss_data <- steelhead_loss_data %>% 
  filter(WY == current_year) %>% 
  mutate(date = as_date(date)) %>% 
  group_by(WY, facility, date) %>% 
  summarise(daily_total_loss = sum(loss))

# Check if there is data for the current water year
use_previous_year <- nrow(current_year_steelhead_loss_data) == 0

# If no data for the current year, use the previous year
if (use_previous_year) {
  current_year_steelhead_loss_data <- steelhead_loss_data %>% 
    filter(WY == previous_year) %>% 
    mutate(date = as_date(date)) %>% 
    group_by(WY,facility, date) %>% 
    summarise(daily_total_loss = sum(loss))
}

# Import export data from SacPAS river conditions (based on CY)
source(here("data-raw/utils_fct_import_river_data.R"))

# Select export sites
code_list <- c("TRP", "HRO") # TRP site code for CVP and HRO site code for SWP facilities

# Set years of interest
years <- if (use_previous_year) c(previous_year) else c(previous_year, current_year)

# Selected metrics of interest
metrics <- c("PumpingDischarge")

# Return river data frame based on selected variables
df_river_raw <- fct_import_SacPAS_river_conditions_query(sites = code_list, years = years, metrics = metrics)

df_river <- df_river_raw %>% 
  mutate(WY = year(YMD) + if_else(month(YMD) >= 10, 1, 0)) %>% # Calculate Water Year
  filter(WY == if (use_previous_year) previous_year else current_year) %>%  # Filter for the appropriate water year
  mutate(site = case_when(
    location == "TRP" ~ "CVP",
    location == "HRO" ~ "SWP"
  )) %>%
  select(site, YMD, value)

# Import OMRI data from SacPAS river conditions
df_OMRI_raw <- fct_import_SacPAS_river_conditions_query(sites = "Combined", years = years, metrics = "OMRIndex") 

df_OMRI <- df_OMRI_raw %>% 
  mutate(WY = year(YMD) + if_else(month(YMD) >= 10, 1, 0)) %>% 
  filter(WY == if (use_previous_year) previous_year else current_year) %>%  
  select(YMD, "OMRI" = value)

# Combine loss and export into final list data
steelhead_loss_export_data <- current_year_steelhead_loss_data %>% 
  left_join(select(df_river, site, YMD, "pumping_discharge_cfs" = value), by = c("facility" = "site", "date" = "YMD")) %>% 
  left_join(select(df_OMRI, OMRI, YMD), by = c("date" = "YMD"))

# Save the data
usethis::use_data(steelhead_loss_export_data, overwrite = TRUE)
