#' @title Produce loss predictions using the Tillotson model for steelhead and
#' winter-run Chinook.
#' @description This script uses the Tillotson model to create predictions of
#' loss for steelhead and winter-run Chinook. It returns results for each
#'species in a list that can be used for both the T-a-C plot and table.

require(tidyverse)
require(janitor)
require(here)
require(data.table)

fct_process_and_run_tillotson_model <- function(species) {
  # TODO: Is janitor::clean_names() needed?
  # TODO: Do NA rows need to be removed? Is replace_na() needed on input data?
  # TODO: What to do when there is no data?
  # TODO: Handle the full water year.
  # TODO: Delete unused code.

  if (species == "STL") {
    # Load the model script
    source(here("data-raw", "Steelhead_Model_Setup.R"))
    model_to_use <- Stlhd_Simple_Combined[[2]]
  } else if (species == "WCH") {
    # Load the model script
    source(here("data-raw", "WR_Model_Setup.R"))
    model_to_use <- WR_Simple_Combined[[2]]
  }

  result_list <- list()

  tac_weekly_calib_data <- read.csv("https://cbr.washington.edu/sacramento/data/generated/tac_weekly_calib.csv")
  tac_weekly_calib_data$dcc <- as.factor(tac_weekly_calib_data$dcc)
  tac_weekly_calib_data <- tac_weekly_calib_data %>% mutate(dcc = if_else(dcc == 1, "open", "closed"))

  today <- Sys.Date()

  # Determine the current and previous water years based on today's date
  if (format(today, "%m") >= "10") {
    current_water_year <- as.numeric(format(today, "%Y")) + 1
  } else {
    current_water_year <- as.numeric(format(today, "%Y"))
  }

  current_water_year_data <- tac_weekly_calib_data[tac_weekly_calib_data$water_year == current_water_year,]
  for (i in 1:nrow(current_water_year_data)) {
    input_data <- data.frame(
      # TODO: Is this correct? "lagging data to use past week data for current water week."
      wy_week = current_water_year_data$wy_week[i] + 1,
      wyweek = current_water_year_data$wy_week[i],
      mal_temp = current_water_year_data$mal_temp[i],
      # TODO: What does this mean? "use sum mraw precip ONLY"
      precip = current_water_year_data$precip[i],
      OMR = current_water_year_data$OMR[i],
      sac = current_water_year_data$sac[i],
      sjr = current_water_year_data$sjr[i],
      dcc = "closed",
      # TODO: Is this correct? daily, not weekly exports?
      daily_exports = current_water_year_data$daily_exports[i]
    )
    if (species == "STL") {
      input_data$stlhd_loss.pw = current_water_year_data$stlhd_loss.pw[i]
    } else if (species == "WCH") {
      # TODO: Is this correct, one is _loss and the other isn't?
      input_data$winter.pw = current_water_year_data$winter.pw[i]
    }

    pred <- predict(model_to_use, newdata = input_data, what = c(0.05, 0.5, 0.95))
    predictions <- data.frame(
      lowerCI = pred[, 1], median = pred[, 2], upperCI = pred[, 3]
    )
    predictions <- predictions %>%
      bind_cols(input_data) %>%
      rename(
        week = wyweek,
        Export = daily_exports,
        weekly_avg_mal_wtemp = mal_temp,
        weekly_avg_fpt_flow = sac,
        weekly_avg_vns_flow = sjr,
        weekly_precip = precip
      ) %>%
      select(-c(wy_week, dcc)) %>%
      mutate(
        wy = current_water_year_data$water_year[i],
        calendar_date = current_water_year_data$date[i]
      )
    if (species == "STL") {
      predictions <- predictions %>% rename(ObservedLoss = stlhd_loss.pw)
    } else if (species == "WCH") {
      predictions <- predictions %>% rename(ObservedLoss = winter.pw)
    }
    result_list[[i]] <- predictions
  }

  return(bind_rows(result_list))
}

# TODO: Are these columns in the output needed? weekly_avg_vns_flow.1, weekly_5dsum_sfs_precip_cfs
# TODO: How do I check these results are correct?

# Winter-run
winter_run_tillotson_output <- fct_process_and_run_tillotson_model(species = "WCH")

# Steelhead
steelhead_tillotson_output <- fct_process_and_run_tillotson_model(species = "STL")

# Combine outputs
tillotson_prediction_output <- list(winter_run_tillotson_output = winter_run_tillotson_output, steelhead_tillotson_output = steelhead_tillotson_output)

usethis::use_data(tillotson_prediction_output, overwrite = TRUE)

# Save each dataset as a CSV file in the data folder (for use in SacPAS table generated on webpage)
write.csv(tillotson_prediction_output$winter_run_tillotson_output, file = here::here("data", "winter_run_tillotson_output.csv"), row.names = FALSE)
write.csv(tillotson_prediction_output$steelhead_tillotson_output, file = here::here("data", "steelhead_tillotson_output.csv"), row.names = FALSE)
