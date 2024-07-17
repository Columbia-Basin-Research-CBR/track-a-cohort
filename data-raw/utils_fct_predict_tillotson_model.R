#' @title Predict Tillotson model for Winter-run Chinook and Steelhead
#' @description This function processes the Tillotson model for Winter-run Chinook and Steelhead species and returns both predicted results for each species in a list. 
require(tidyverse)
require(janitor)
require(here)
require(fread)

source(here("R/utils_fct_assign_current_water_year.R"))
source(here("data-raw/utils_fct_import_river_data.R"))
source(here("R/brt.functions.r"))
load(here("R/ITMData.rda"))


current_year <- assign_current_water_year()

startOfWY <- function(date) {
  if (month(date) >= 10) {
    ymd(paste(year(date), "10-01", sep = "-"))
  } else {
    ymd(paste(year(date) - 1, "10-01", sep = "-"))
  }
}

calculateWYWeek <- function(date) {
  waterYearStart <- startOfWY(date)
  daysSinceStart <- as.integer(difftime(date, waterYearStart, units = "days"))
  weekNumber <- ceiling(daysSinceStart / 7)
  return(weekNumber)
}

currentWY <- 2024
previousWY <- current_year - 1

fct_process_and_run_tillotson_model <- function(species_url, species_filter, model_script, species.pw) {
  # Load the model script
  source(here("data-raw", model_script))

  # weekly loss totals differ from BOR supplied code -- need to verify discrepancies
  df_fish_raw <- read_csv(species_url) %>%
    janitor::clean_names()

  # Apply species filter based on the species_filter argument
  if (species_filter == "STL") {
    df_fish <- df_fish_raw %>% filter(species == "Rainbow / Steelhead Trout")
    species_column_name <- "stlhd_loss.pw"
    model_to_use <- Stlhd_Simple_Combined[[2]]
  } else if (species_filter == "WCH") {
    df_fish <- df_fish_raw %>% filter(lad_race == "Winter")
    species_column_name <- "winter.pw"
    model_to_use <- WR_Simple_Combined[[2]]
  }

  df_fish <- df_fish %>%
    mutate(
      sample_time = ymd_hms(sample_time),
      date = date(sample_time)
    ) %>%
    group_by(date) %>%
    summarise(total_daily_loss = sum(loss)) %>%
    ungroup() %>%
    mutate(week = sapply(date, calculateWYWeek)) %>%
    group_by(week) %>%
    summarise(total_weekly_loss = sum(total_daily_loss))




  # Set variables of interest for river data import function
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

  # select sites of interest: Flow, OMR, Export, Temperature
  code_list <- c("FPT", "VNS", "MAL", "TRP", "HRO")
  # set years of interest
  years <- c(years)
  # selected metrics of interest
  metrics <- c("Flow", "WaterTemperature", "PumpingDischarge")

  # Run the function with the code list, years, and metrics
  df_river_raw <- fct_import_SacPAS_river_conditions_query(sites = code_list, years = years, metrics = metrics)
  # for some reason function returns OMR data multiple times when run in a list above. For now running seperately
  df_OMR_raw <- fct_import_SacPAS_river_conditions_query(sites = "Combined", years = years, metrics = "OMRDailyUSGS")

  df_river <- df_river_raw %>%
    bind_rows(df_OMR_raw) %>%
    mutate(WY = year(YMD) + (month(YMD) >= 10)) %>%
    filter(WY == current_year) %>%
    mutate(week = sapply(YMD, calculateWYWeek)) %>%
    pivot_wider(names_from = c(location, parameter), values_from = value) %>%
    select(YMD, WY, week, FPT_flow, VNS_flow, Combined_OMRDailyUSGS, TRP_pumping, HRO_pumping, MAL_wtemp) %>%
    mutate(combined_export = TRP_pumping + HRO_pumping) %>%
    group_by(week) %>%
    summarise(
      weekly_avg_fpt_flow = mean(FPT_flow, na.rm = TRUE),
      weekly_avg_vns_flow = mean(VNS_flow, na.rm = TRUE),
      weekly_avg_omr = mean(Combined_OMRDailyUSGS, na.rm = TRUE),
      weekly_avg_export = mean(combined_export, na.rm = TRUE),
      weekly_avg_mal_wtemp = mean(MAL_wtemp, na.rm = TRUE)
    )


  df_combined <- df_fish %>%
    left_join(df_river, by = "week")

  # list for storing outputs
  tillotsonList <- list()

  for (i in 1:nrow(df_combined)) {
    NewData <- data.frame(
      wy_week = df_combined$week[i] + 1,
      mal_temp = df_combined$weekly_avg_mal_wtemp[i],
      precip = 1,
      OMR = df_combined$weekly_avg_omr[i],
      sac = df_combined$weekly_avg_fpt_flow[i],
      sjr = df_combined$weekly_avg_vns_flow[i],
      dcc = "closed",
      daily_exports = df_combined$weekly_avg_export[i],
      species.pw = df_combined$total_weekly_loss[i]
    )

    # Dynamically rename the species.pw column
    names(NewData)[names(NewData) == "species.pw"] <- species_column_name


    pred <- predict(model_to_use, newdata = NewData, what = c(0.05, 0.5, 0.95))
    predictions <- data.frame(
      lowerCI = pred[, 1], median = pred[, 2], upperCI = pred[, 3],
      week = df_combined$week[i],
      OMR = df_combined$weekly_avg_omr[i],
      Export = df_combined$weekly_avg_export[i],
      ObservedLoss = df_combined$total_weekly_loss[i],
      weekly_avg_mal_wtemp = df_combined$weekly_avg_mal_wtemp[i],
      weekly_avg_fpt_flow = df_combined$weekly_avg_fpt_flow[i],
      weekly_avg_vns_flow = df_combined$weekly_avg_vns_flow[i]
    )
    tillotsonList[[i]] <- predictions
  }

  return(bind_rows(tillotsonList))
}



# Winter-run
winter_run_url <- paste0("https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=", current_year, "&species=1%3Af&dnaOnly=no&age=no")
winter_run_script <- "WR_Model_Setup.R"
winter_run_tillotson_output <- fct_process_and_run_tillotson_model(species_url = winter_run_url, species_filter = "WCH", model_script = winter_run_script, species.pw = "winter.pw")

# Steelhead
steelhead_url <- paste0("https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=", current_year, "&species=2%3Af&dnaOnly=no&age=no")
steelhead_script <- "Steelhead_Model_Setup.R"
steelhead_tillotson_output <- fct_process_and_run_tillotson_model(species_url = steelhead_url, species_filter = "STL", model_script = steelhead_script, species.pw = "stlhd_loss.pw")

# Combine outputs
tillotson_prediction_output <- list(winter_run_tillotson_output = winter_run_tillotson_output, steelhead_tillotson_output = steelhead_tillotson_output)


usethis::use_data(tillotson_prediction_output, overwrite = TRUE)
