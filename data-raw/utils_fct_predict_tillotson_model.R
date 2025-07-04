#' @title Predict Tillotson model for Winter-run Chinook and Steelhead
#' @description This function calls the shared Tillotson model for Winter-run Chinook and Steelhead
#' and returns both predicted results for each species in a list that can be used for TAC plot and table. 
require(tidyverse)
require(janitor)
require(here)
require(data.table)

# assign current water year
source(here("R/utils_fct_assign_current_water_year.R"))
current_year <- assign_current_water_year()

# import fct to import river data from SacPAS
source(here("data-raw/utils_fct_import_river_data.R"))

#import fct to import precipitation data from CDEC
source(here("data-raw/utils_fct_import_CDEC_precipitation_data.R"))

# import functions needed to run Tillotson model (shared by BOR)
source(here("R/brt.functions.R"))
load(here("R/ITMData.rda"))

# Define the start if WY and weekstart to set dates
startOfWY <- function(date) {
  if (month(date) >= 10) {
    ymd(paste(year(date), "10-01", sep = "-"))
  } else {
    ymd(paste(year(date) - 1, "10-01", sep = "-"))
  }
}


weekStartDate <- function(weekNumber, waterYear) {
  waterYearStart <- ymd(paste(waterYear - 1, "10-01", sep = "-"))
  startDateOfWeek <- waterYearStart + days((weekNumber - 1) * 7)
  return(startDateOfWeek)
}



calculateWYWeek <- function(date) {
  waterYearStart <- startOfWY(date)
  daysSinceStart <- as.integer(difftime(date, waterYearStart, units = "days"))
  weekNumber <- floor(daysSinceStart / 7) +1
  return(weekNumber)
}


# Function to check if the data is valid
is_valid_data <- function(df, species_filter) {
  if(species_filter == "STL") {
    required_columns <- c("sample_time", "facility", "adipose_clip", "loss")
    } else if(species_filter == "WCH") {
    required_columns <- c("sample_time", "facility", "adipose_clip", "lad_race", "loss")
    }
  all(required_columns %in% colnames(df))
}

fct_process_and_run_tillotson_model <- function(species_url, species_filter, model_script, species.pw) {
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
  
  #set start date of WY
  start_date_wy <- ymd(paste0(previousWY, "-10-01"))
  
  # Load the model script
  source(here("data-raw", model_script))

  # weekly loss totals differ from BOR supplied code -- need to verify discrepancies
  df_fish_raw <- read_csv(species_url) %>%
    janitor::clean_names()
  
  # If the data is invalid, load data from the previous water year -- adjusted for if date is past 10/1--see code below
  if (!is_valid_data(df_fish_raw, species_filter)) {
    previous_url <- sub(paste0("year=", currentWY), paste0("year=", previousWY), species_url)
    df_fish_raw <- read_csv(previous_url) %>%
      janitor::clean_names()
    use_previous_year <- TRUE
  } else {
    use_previous_year <- FALSE
  }
  

  # Apply species filter based on the species_filter argument
  # Add check if new WY data is valid, but no species specific data is present yet, use previous year data
  # Adjusted 4/8/25 to check if date (with no loss) is in the current WY (start date 10/1), then return 0 in place of no data for the current WY and proceed.
  if (species_filter == "STL") {
    df_fish_check <- df_fish_raw %>% filter(species == "Rainbow / Steelhead Trout")
     if (nrow(df_fish_check) == 0) {
       if(today >= start_date_wy) {
         # generate week sequence up to current week
         current_wyweek <- calculateWYWeek(today)
         wyweeks <- 1:current_wyweek
         
         df_fish <- tibble(
           wyweek = wyweeks,
           total_weekly_loss = rep(0, length(wyweeks)),
           calendar_date = sapply(wyweeks, function(wk) weekStartDate(wk, currentWY))
         )
         use_previous_year <- FALSE  # We're using current year, with 0s
       } else {
      previous_url <- sub(paste0("year=", currentWY), paste0("year=", previousWY), species_url)
      df_fish_raw<- read_csv(previous_url) %>%
        janitor::clean_names()
      df_fish <- df_fish_raw %>% filter(species == "Rainbow / Steelhead Trout")
      use_previous_year <- TRUE
       }
     } else {
      df_fish <- df_fish_raw %>% filter(species == "Rainbow / Steelhead Trout")
     }
    species_column_name <- "stlhd_loss.pw"
    model_to_use <- Stlhd_Simple_Combined[[2]]
  } else if (species_filter == "WCH") {
    df_fish_check <- df_fish_raw %>% filter(lad_race == "Winter")
    if (nrow(df_fish_check) == 0) {
      if(today >= start_date_wy) {
        # generate week sequence up to current week
        current_wyweek <- calculateWYWeek(today)
        wyweeks <- 1:current_wyweek
        
        df_fish <- tibble(
          wyweek = wyweeks,
          total_weekly_loss = rep(0, length(wyweeks)),
          calendar_date = sapply(wyweeks, function(wk) weekStartDate(wk, currentWY))
        )
        use_previous_year <- FALSE  # We're using current year, with 0s
      } else {
      previous_url <- sub(paste0("year=", currentWY), paste0("year=", previousWY), species_url)
      df_fish_raw <- read_csv(previous_url) %>%
        janitor::clean_names()
      df_fish <- df_fish_raw %>% filter(lad_race == "Winter")
      use_previous_year <- TRUE
      }
    } else {
      df_fish <- df_fish_raw %>% filter(lad_race == "Winter")
    }
    species_column_name <- "winter.pw"
    model_to_use <- WR_Simple_Combined[[2]]
  }
  
  # Determine the water year to use for calendar date assignment
  waterYearToUse <- if (use_previous_year) previousWY else currentWY

  df_fish <- df_fish %>%
    mutate(
      sample_time = ymd_hms(sample_time),
      date = date(sample_time)
    ) %>%
    group_by(date) %>%
    summarise(total_daily_loss = sum(loss)) %>%
    ungroup() %>%
    mutate(wyweek = sapply(date, calculateWYWeek)) %>%
    group_by(wyweek) %>%
    summarise(total_weekly_loss = sum(total_daily_loss)) %>% 
    complete(wyweek = 1:calculateWYWeek(today), fill = list(total_weekly_loss = 0)) %>%  # Fill in missing weeks with 0
    mutate(calendar_date = weekStartDate(wyweek, waterYearToUse)) # Add calendar date column (adjusting week since the previous week is used for the current week prediction)


  # Set variables of interest for river data import function


  # select sites of interest: Flow, OMR, Export, Temperature
  code_list <- c("FPT", "VNS", "MAL", "TRP", "HRO")

  # Set years of interest based on the validity of df_fish
  years <- if (use_previous_year) c(previousWY-1, previousWY) else c(previousWY, currentWY)
  
  # selected metrics of interest
  metrics <- c("Flow", "WaterTemperature", "PumpingDischarge")

  # Run the function with the code list, years, and metrics
  df_river_raw <- fct_import_SacPAS_river_conditions_query(sites = code_list, years = years, metrics = metrics)
  # for some reason function returns OMR data multiple times when run in a list above. For now running seperately
  df_OMR_raw <- fct_import_SacPAS_river_conditions_query(sites = "Combined", years = years, metrics = "OMRDailyUSGS")

  df_river <- df_river_raw %>%
    bind_rows(df_OMR_raw) %>%
    mutate(WY = year(YMD) + (month(YMD) >= 10)) %>%
    filter(WY %in% if (use_previous_year) previousWY else currentWY) %>%
    mutate(wyweek = sapply(YMD, calculateWYWeek)) %>%
    select(-c(year,mm,dd,unit,datatype)) %>%
    pivot_wider(names_from = c(location, parameter), values_from = value) %>%
    select(YMD, WY, wyweek, FPT_flow, VNS_flow, Combined_OMRDailyUSGS, TRP_pumping, HRO_pumping, MAL_wtemp) %>%
    mutate(combined_export = TRP_pumping + HRO_pumping) %>%
    group_by(wyweek) %>%
    summarise(
      weekly_avg_fpt_flow = mean(FPT_flow, na.rm = TRUE),
      weekly_avg_vns_flow = mean(VNS_flow, na.rm = TRUE),
      weekly_avg_omr = mean(Combined_OMRDailyUSGS, na.rm = TRUE),
      weekly_avg_export = mean(combined_export, na.rm = TRUE),
      weekly_avg_mal_wtemp = mean(MAL_wtemp, na.rm = TRUE)
    ) %>% 
    drop_na() #remove if no data--consider inputting 0 or 1
  
  # import precipitation data from CDEC
  sfs_summed <- fct_import_CDEC_precipitation_data(start_date = start_date_wy, end_date = today)

  df_combined <- df_fish %>%
    left_join(df_river, by = "wyweek") %>% 
    left_join(sfs_summed, by = "wyweek") %>%
    drop_na() #remove if no data--consider inputting 0 or 1

  # list for storing outputs
  tillotsonList <- list()

  for (i in 1:nrow(df_combined)) {
    NewData <- data.frame(
      wy_week = df_combined$wyweek[i] + 1, #lagging data to use past week data for current water week.
      wyweek = df_combined$wyweek[i],
      mal_temp = df_combined$weekly_avg_mal_wtemp[i],
      precip = df_combined$weekly_5dsum_sfs_precip_cfs[i],
      OMR = df_combined$weekly_avg_omr[i],
      sac = df_combined$weekly_avg_fpt_flow[i],
      sjr = df_combined$weekly_avg_vns_flow[i],
      dcc = "closed",
      daily_exports = df_combined$weekly_avg_export[i],
      species.pw = df_combined$total_weekly_loss[i]
    )

    NewData<- NewData %>% 
      replace_na()

    # Dynamically rename the species.pw column
    names(NewData)[names(NewData) == "species.pw"] <- species_column_name


    pred <- predict(model_to_use, newdata = NewData, what = c(0.05, 0.5, 0.95))
    predictions <- data.frame(
      lowerCI = pred[, 1], median = pred[, 2], upperCI = pred[, 3],
      week = df_combined$wyweek[i],
      OMR = df_combined$weekly_avg_omr[i],
      Export = df_combined$weekly_avg_export[i],
      ObservedLoss = df_combined$total_weekly_loss[i],
      weekly_avg_mal_wtemp = df_combined$weekly_avg_mal_wtemp[i],
      weekly_avg_fpt_flow = df_combined$weekly_avg_fpt_flow[i],
      weekly_avg_vns_flow = df_combined$weekly_avg_vns_flow[i],
      weekly_avg_vns_flow = df_combined$weekly_avg_vns_flow[i],
      weekly_5dsum_sfs_precip_cfs = df_combined$weekly_5dsum_sfs_precip_cfs[i],
      calendar_date = df_combined$calendar_date[i], # Include calendar date in predictions
      wy = waterYearToUse  # Add water year to predictions
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

# Save each dataset as a CSV file in the data folder (for use in SacPAS table generated on webpage)
write.csv(tillotson_prediction_output$winter_run_tillotson_output, file = here::here("data", "winter_run_tillotson_output.csv"), row.names = FALSE)

write.csv(tillotson_prediction_output$steelhead_tillotson_output, file = here::here("data", "steelhead_tillotson_output.csv"), row.names = FALSE)
