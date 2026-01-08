#' @title Figure 5: Steelhead Cumulative Loss for current water year 
#' @details
#' Plots reflect management actions post 2019 BiOp and are subject to revision.
#' Single Year Threshold for Steelhead:
#' Natural Central Valley Steelhead (loss = 5294) for a single WY year.
#' Previous management thresholds:
#' Single-Year Loss Thresholds (PA 4-69, 2019 BiOP)
#' In each year, typically January/February, Reclamation and DWR propose to avoid exceeding an annual loss threshold equal to 90% of the greatest annual loss that occurred in the historical record 2010-2018 for each of:
#' Natural Winter-Run Chinook Salmon (loss= 1.17% of JPE)
#' Natural Central Valley Steelhead from December through March (loss =1,414)
#' Natural Central Valley Steelhead from April through June 15 (loss = 1,552)
#' (More information on PA 4-70, 2019 BiOP)

require(tidyverse)
require(here)
require(ggrepel)


# import data file
#loss data from SacPAS

load(here("data/steelhead_loss_data.rda"))


# import wDay to month function
source(here("R/utils_fct_wday_to_month.R"))

#set current year
source(here("R/utils_fct_assign_current_water_year.R"))
        current_year <- assign_current_water_year()   # Assuming this gives the current water year
        previous_year <- current_year - 1
        current_date <- Sys.Date()
        
        # If the current year data exists for unclipped steelhead, use current year
        if (any(steelhead_loss_data$WY == current_year & steelhead_loss_data$adipose_clip == "Unclipped")) {
          use_previous_year <- FALSE
        } else {
          # Otherwise, use the previous year's data if no current year data
          use_previous_year <- TRUE
        }

       # Assign plot year and caption note based on the condition
       if (use_previous_year) {
         plot_year <- previous_year
         caption_note <- paste0("No data reported for WY", current_year, ". Data reflects last available data (WY", previous_year, ").\n")
       } else {
         plot_year <- current_year
         caption_note <- ""
       }
       

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

steelhead_loss_data_unclipped <- steelhead_loss_data %>% 
  filter(adipose_clip == "Unclipped") %>% 
  mutate(date = as_date(date))



# extract maximum cumloss for the current year
cumloss_current_year <- steelhead_loss_data_unclipped %>%
  filter(WY == plot_year) %>% 
  mutate(cumloss = cumsum(loss))


# Set loss threshold -- manually set 
current_year_100pct <- 5294
current_year_75pct <- current_year_100pct*.75
current_year_50pct <- current_year_100pct*.50


# set x-lim to start of water year. 10-01
startdate_WY <-as.Date(paste0(plot_year-1, "-10-01"))


# Identify the first known data point value for the current year
first_known_data <- cumloss_current_year %>%
  arrange(date) %>%
  slice(1)

# Create a sequence of dates from October 1st to the first known data point
start_date <- as.Date(paste0(plot_year - 1, "-10-01"))
missing_dates_start <- seq.Date(from = start_date, to = first_known_data$date, by = "day")

# Create a new data frame with missing dates and the first known value
missing_data_start <- data.frame(
  date = missing_dates_start,
  cumloss = c(rep(0, length(missing_dates_start) - 1), first_known_data$cumloss),  # Start from 0 and connect to the first known data point
  WY = plot_year
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
  WY = plot_year
)

# Combine these new data frames with the original data
cumloss_current_year_filled <- bind_rows(cumloss_current_year, missing_data_start, missing_data_end)



# plot
p <- cumloss_current_year_filled %>% 
  ggplot(aes(x= date, y = cumloss)) +
    geom_line(data = cumloss_current_year, aes(x = date, y = cumloss, color = "Reported Loss", linetype = "Reported Loss")) +
    geom_line(data = missing_data_start, aes(x = date, y = cumloss, color = "No Loss Reported", linetype = "No Loss Reported")) +
    geom_line(data = missing_data_end, aes(x = date, y = cumloss, color = "No Loss Reported", linetype = "No Loss Reported")) +
    geom_point(data = cumloss_current_year, aes(x = date, y = cumloss, color = "Reported Loss")) +
    geom_text(aes(x = start_date, y = current_year_100pct, label = paste0("100% Single-Year Threshold: ", round(current_year_100pct,2))), hjust = 0, vjust = 2, color = "red4", size = 3) +
    geom_hline(yintercept = current_year_100pct, linetype = "dashed", color = "red4") +
    geom_text(aes(x = start_date, y = current_year_75pct, label = paste0("75% Single-Year Threshold: ", round(current_year_75pct,2))), hjust = 0, vjust = 2, color = "#CC7722", size = 3) +
    geom_hline(yintercept = current_year_75pct, linetype = "dashed", color = "#CC7722") +
    geom_text(aes(x = start_date, y = current_year_50pct, label = paste0("50% Single-Year Threshold: ", round( current_year_50pct,2))), hjust = 0, vjust = 2, color = "goldenrod3", size = 3) +
    geom_hline(yintercept = current_year_50pct, linetype = "dashed", color = "goldenrod3") +
    geom_vline(aes(xintercept = as.numeric(wDay_to_date(wDay_today, current_year)), color = "Current Date", linetype = "Current Date")) +
    geom_label_repel(data = data.frame(date = max(cumloss_current_year$date), cumloss = max(cumloss_current_year$cumloss)),
                   aes(x = date, 
                       y = cumloss, 
                       label = paste0("Cumulative loss: ", round(max(cumloss),2),"\n% loss of Single-Year Threshold: ", 
                                      round((max(cumloss)/current_year_100pct)*100, 2), "%" )
                       ),
                   size = 3, 
                   nudge_x = 1, # Adjust nudge_x and nudge_y as needed to position the label
                   nudge_y = 400, 
                   hjust = 0,
                   color = "black") +
    scale_x_date(date_labels = "%m/%d", date_breaks = "1 month", limits = c(start_date,NA), expand = c(.01,.01)) +
    scale_y_continuous(expand = c(0,100)) +
      scale_color_manual(values = c("Reported Loss" = "black", "No Loss Reported" = "grey", "Current Date" = "blue2")) +
    scale_linetype_manual(values = c("Reported Loss" = "solid", "No Loss Reported" = "solid", "Current Date" = "dotted")) +
    labs(title = paste0("Cumulative Loss for WY", plot_year, " with Single-Year Loss Thresholds"),
         subtitle = paste0("Species: Unclipped Steelhead\nCumulative loss: ", round(max(cumloss_current_year$cumloss),2),"\nSingle-Year Loss Threshold: ", current_year_100pct,"\n"),
         caption = paste0(caption_note, "Data sources: Preliminary data from CDFW; subject to revision.\n", timestamp),
         x = "Date",
         y = "Cumulative Loss",
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

#Notes
#updated plot to reflect changes in mangement-- no longer using management periods-- plot is for a single water year.
