#' @title Winter-run Chinook Cumulative Loss for current water year -- Genetic
#' @details
#' Single-Year Loss Thresholds: .5004% of JPE

require(tidyverse)
require(here)
require(ggrepel)


# import data file
load(here("data/jpe_genetic_loss_data.rda"))


# import wDay to month function
source(here("R/utils_fct_wday_to_month.R"))

#set current year
source(here("R/utils_fct_assign_current_water_year.R"))
current_year <- assign_current_water_year()

# Get the current timestamp
timestamp <- format(Sys.time(), "%d %b %Y %H:%M:%S %Z")

#current date
date <- today()
#convert current date to water day
wDay_today <- if_else(month(date) >= 10, yday(date) - 273, yday(date) + 92)

# Function to convert wDay to actual date
wDay_to_date <- function(wDay, WY) {
  start_date <- ymd(paste0(WY - 1, "-10-01")) # Water year starts on Oct 1 of the previous calendar year
  return(start_date + days(wDay - 1))
}

# wrangle base data for plot

# extract cumulative loss data and add CY date column
genetic_cumulative_loss_data <- jpe_genetic_loss_data$genetic_cumulative_loss_data

#convert back to CY date
genetic_cumulative_loss_data$date <- as_date(mapply(wDay_to_date, genetic_cumulative_loss_data$wDay, genetic_cumulative_loss_data$WY))

# extract maximum cumloss for the current year
cumloss_current_year <- genetic_cumulative_loss_data %>%
  filter(WY == current_year) 


# extract current water year JPE and set 100% genetic loss of JPE -- return single value
jpe_current_year_100pct <- genetic_cumulative_loss_data %>%
  filter(WY == current_year, cumloss == max(cumloss)) %>%
  pull(jpe)*.005004

jpe_current_year_75pct <- jpe_current_year_100pct *.75
jpe_current_year_50pct <- jpe_current_year_100pct *.50


max_loss_threshold_2010_to_2018 <- genetic_cumulative_loss_data %>% 
  filter(between(WY, 2010, 2018)) %>%
  ungroup() %>% 
  summarise(max_loss = max(cumloss)) %>%
  pull(max_loss)

# set x-lim to start of water year. 10-01
startdate_WY <-as.Date(paste0(current_year-1, "-10-01"))


# add missing values to from start to todays date

# Identify the first known data point value for the current year
first_known_data <- cumloss_current_year %>%
  arrange(date) %>%
  slice(1)

# Create a sequence of dates from October 1st to the first known data point
start_date <- as.Date(paste0(current_year - 1, "-10-01"))
missing_dates_start <- seq.Date(from = start_date, to = first_known_data$date, by = "day")

# Create a new data frame with missing dates and the first known value
missing_data_start <- data.frame(
  date = missing_dates_start,
  cumloss = first_known_data$cumloss,
  WY = current_year
)

# Identify the last known data point value for the current year
last_known_data <- cumloss_current_year %>%
  filter(date <= Sys.Date()) %>%
  arrange(desc(date)) %>%
  slice(1)

# Create a sequence of dates from the last known data point to today's date
missing_dates_end <- seq.Date(from = last_known_data$date, to = Sys.Date(), by = "day")

# Create a new data frame with these dates and the last known value
missing_data_end <- data.frame(
  date = missing_dates_end,
  cumloss = last_known_data$cumloss,
  WY = current_year
)

# Combine these new data frames with the original data
cumloss_current_year_filled <- bind_rows(cumloss_current_year, missing_data_start, missing_data_end)

# Plot with the new data
p <- cumloss_current_year_filled %>%
  ggplot(aes(x = date, y = cumloss)) +
  geom_line(data = cumloss_current_year, aes(x = date, y = cumloss, color = "Reported Loss", linetype = "Reported Loss")) +
  geom_line(data = missing_data_start, aes(x = date, y = cumloss, color = "No Loss Reported", linetype = "No Loss Reported")) +
  geom_line(data = missing_data_end, aes(x = date, y = cumloss, color = "No Loss Reported", linetype = "No Loss Reported")) +
  geom_point(data = cumloss_current_year, aes(x = date, y = cumloss, color = "Reported Loss")) +
  geom_hline(yintercept = jpe_current_year_100pct, linetype = "dashed", color = "red4") +
  geom_text(aes(x = start_date, y = jpe_current_year_100pct, label = paste0("100% Single-Year Threshold (.5004% of JPE): ", round(jpe_current_year_100pct, 2))), hjust = 0, vjust = 2, color = "red4", size = 3) +
  geom_hline(yintercept = jpe_current_year_75pct, linetype = "dashed", color = "#CC7722") +
  geom_text(aes(x = start_date, y = jpe_current_year_75pct, label = paste0("75% Single-Year Threshold (.5004% of JPE): ", round(jpe_current_year_75pct, 2))), hjust = 0, vjust = 2, color = "#CC7722", size = 3) +
  geom_hline(yintercept = jpe_current_year_50pct, linetype = "dashed", color = "goldenrod3") +
  geom_text(aes(x = start_date, y = jpe_current_year_50pct, label = paste0("50% Single-Year Threshold (.5004% of JPE): ", round(jpe_current_year_50pct, 2))), hjust = 0, vjust = 2, color = "goldenrod3", size = 3) +
  geom_vline(aes(xintercept = as.numeric(wDay_to_date(wDay_today, current_year)), color = "Current Date", linetype = "Current Date")) +
  geom_label_repel(data = data.frame(date = max(cumloss_current_year$date), cumloss = max(cumloss_current_year$cumloss)),
                   aes(x = date, y = cumloss, label = paste0("Cumulative genetic loss: ", max(cumloss), "\n% loss of Single-Year Threshold: ", round((cumloss / jpe_current_year_100pct) * 100, 2), "%")),
                   size = 3,
                   nudge_x = 1,
                   nudge_y = 100,
                   hjust = 0,
                   color = "black") +
  scale_x_date(date_labels = "%m/%d", limits = c(start_date, NA)) +
  scale_y_continuous(expand = c(0, 30)) +
  scale_color_manual(values = c("Reported Loss" = "black", "No Loss Reported" = "grey", "Current Date" = "blue2")) +
  scale_linetype_manual(values = c("Reported Loss" = "solid", "No Loss Reported" = "solid", "Current Date" = "dotted")) +
  labs(title = paste0("Cumulative Genetic Loss for WY", current_year, " with Single-Year Thresholds"),
       subtitle = paste0("Species: Natural Winter-run Chinook\nCumulative genetic loss to date: ", max(cumloss_current_year$cumloss),
                         "\nPercent loss of Single-Year Threshold: ", round((max(cumloss_current_year$cumloss) / jpe_current_year_100pct) * 100, 2), "%"),
       caption = paste0("Genetic loss data provided by USBR before Water Year 2020 otherwise sourced from the CDFW Salvage Database.\n", timestamp),
       x = "Date",
       y = "Cumulative Genetic Loss", 
       color = NULL,
       linetype = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(linetype = "dotted"),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(size = 0.5),
    panel.background = element_rect(color = "black", fill = "transparent", size = 1),
    legend.position = "bottom",
    text = element_text(size = 15))

print(p)
