#' @title import steelhead loss and export data from SacPAS
#' @description This script is used to import daily loss, export per facility, and OMRI values to be used in the Winter-run Chinook 
#' barplot and linegraph showing daily loss, with pumping discharge per facility and specific OMRI dates/values. This script uses LAD loss date. 
#' @return .rda of winter-run daily loss and export data saved in `data` folder.
require(tidyverse)
require(janitor)
require(usethis)
require(here)

source(here("R/utils_fct_assign_current_water_year.R"))
current_year <- assign_current_water_year()

# import LAD loss directly from SacPAS 
url <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=default&year=all&species=1%3Af&dnaOnly=no&age=no"

# filter to include only winter-run chinook for LAD or winter-run for DNA
loss.wch <- read.csv(url) %>%
  filter(LAD.Race == "Winter") # DNA.Race == 'Winter'

# adjust imported loss to WY
df_lad_loss <- loss.wch %>%
  select(Sample.Time, Facility, LAD.Race, DNA.Race, nfish, Loss) %>%
  janitor::clean_names() %>%
  mutate(datetime = ymd_hms(sample_time)) %>%
  arrange(datetime) %>%
  mutate(WY = year(datetime) + (month(datetime) >= 10),
         date = as_date(datetime))

# Calculate total daily loss
current_year_winter_run_loss_data <- df_lad_loss %>%
  filter(WY == current_year) %>%
  group_by(WY,facility, date) %>%
  summarize(daily_total_loss = sum(loss))


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
    select(site, WY, YMD, value)
  
  # import OMRI data from SacPAS river conditions
  df_OMRI_raw <- fct_import_SacPAS_river_conditions_query(sites = "Combined", years = years, metrics = "OMRIndex") 
  
  df_OMRI <- df_OMRI_raw %>% 
  mutate(WY = year(YMD) + if_else(month(YMD) >= 10, 1, 0)) %>% 
    filter(WY == current_year) %>%  
  select( WY, YMD, "OMRI" = value)


# combine loss and export into final list data
winter_run_chinook_loss_export_data <- current_year_winter_run_loss_data %>% 
  full_join(select(df_river, site, WY,YMD, "pumping_discharge_cfs" = value), by = c("facility" = "site", "date" = "YMD")) %>% 
  full_join(select(df_OMRI, OMRI,WY, YMD), by = c( "date" = "YMD")) %>% 
  mutate(WY = coalesce(WY.x, WY.y, WY)) %>% # coalesce to keep WY for all datasets to plot  discharge and OMR through WY even without loss data
  select(-WY.x, -WY.y)
  

# Save the data
usethis::use_data(winter_run_chinook_loss_export_data, overwrite = TRUE)
