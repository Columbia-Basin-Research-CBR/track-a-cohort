#' @title Figure 5: Steelhead Cumulative Loss for current water year 
#' @details
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
        
        # If the current year data exists for unclipped steelhead and the date is after 12/31 of the previous year, use current year
        if (any(steelhead_loss_data$WY == current_year & steelhead_loss_data$adipose_clip == "Unclipped" & steelhead_loss_data$date >= as.Date(paste0(previous_year, "-12-31")))) {
          use_previous_year <- FALSE
        } else {
          # Otherwise, use the previous year's data if no current year data or if the date is before 12/31 of the previous year
          use_previous_year <- TRUE
        }

       # Assign plot year and caption note based on the condition
       if (use_previous_year) {
         plot_year <- previous_year
         caption_note <- paste0("No data reported for WY", current_year, ", or current date is prior to 12/31. Data reflects last available data (WY", previous_year, ").\n")
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
  mutate(management_period = case_when(
    (month(date) == 12 & day(date) >= 31) | 
      month(date) %in% c(1, 2) | 
      (month(date) == 3 & day(date) <= 31)  ~ "12/1 - 3/31",
    (month(date) == 4 & day(date) >= 1) | 
      (month(date) == 5) | 
      (month(date) == 6 & day(date) <= 15) ~ "4/1 - 6/15",
    TRUE ~ NA_character_
  )) %>% 
  filter(!is.na(management_period)) %>%
  group_by(management_period) %>%
  mutate(cum_loss_mgt = cumsum(loss))


# Set loss threshold for each management period
current_year_mgmt1_100pct <- 1414  
current_year_mgmt1_75pct <- current_year_mgmt1_100pct*.75
current_year_mgmt1_50pct <- current_year_mgmt1_100pct*.50

current_year_mgmt2_100pct <- 1552 
current_year_mgmt2_75pct <- current_year_mgmt2_100pct*.75
current_year_mgmt2_50pct <- current_year_mgmt2_100pct*.50

# set management threshold line
management_period_start <- as.Date(paste0(plot_year,"-03-31"))
management_period_end <- as.Date(paste0(plot_year,"-06-15"))

#set start date for xlim 
start_date <- as.Date(paste0(plot_year - 1, "-12-31"))

# Calculate maximum cumulative loss for each management period
max_loss_by_period <- cumloss_current_year %>%
  group_by(management_period) %>%
  summarize(
    max_date = max(date),
    max_cum_loss_mgt = max(cum_loss_mgt)
  ) %>%
  ungroup()

max_loss_by_period$management_period[1]


# plot
p <- cumloss_current_year %>% 
  ggplot(aes(x= date, y = cum_loss_mgt, group = management_period, color = management_period)) +
  geom_point(data = . %>% filter(management_period == management_period[1])) +
  geom_line(data = . %>% filter(management_period == management_period[1])) +
  geom_point(data = . %>% filter(management_period == management_period[2])) +
  geom_line(data = . %>% filter(management_period == management_period[2])) +
  geom_vline(xintercept = management_period_start, linetype = "dashed", color = "darkgrey", show.legend = TRUE) +
  #set thresholds for management period 1
  geom_segment(y = current_year_mgmt1_100pct, x = start_date, xend = management_period_start, linetype = "dashed", color = "red4") +
  geom_text(aes(x = start_date, y = current_year_mgmt1_100pct, label = paste0("100% Single-Year Threshold: ", round(current_year_mgmt1_100pct,2))), hjust = 0, vjust = 2, color = "red4", size = 3) +
  geom_segment(y = current_year_mgmt1_75pct, x = start_date, xend = management_period_start, linetype = "dashed", color = "#CC7722") +
  geom_text(aes(x = start_date, y = current_year_mgmt1_75pct, label = paste0("75% Single-Year Threshold: ", round(current_year_mgmt1_75pct,2))), hjust = 0, vjust = 2, color = "#CC7722", size = 3) +
  geom_segment(y = current_year_mgmt1_50pct, x = start_date, xend = management_period_start, linetype = "dashed", color = "goldenrod3") +
  geom_text(aes(x = start_date, y = current_year_mgmt1_50pct, label = paste0("50% Single-Year Threshold: ", round( current_year_mgmt1_50pct,2))), hjust = 0, vjust = 2, color = "goldenrod3", size = 3) +
  #set thresholds for management period 2
  geom_segment(y = current_year_mgmt2_100pct, x = management_period_start, xend = management_period_end, linetype = "dashed", color = "red4") +
  geom_text(aes(x = management_period_start, y = current_year_mgmt2_100pct, label = paste0("100% Single-Year Threshold: ", round(current_year_mgmt2_100pct,2))), hjust = 0, vjust = 2, color = "red4", size = 3) +
  geom_segment(y = current_year_mgmt2_75pct, x = management_period_start, xend = management_period_end, linetype = "dashed", color = "#CC7722") +
  geom_text(aes(x = management_period_start, y = current_year_mgmt2_75pct, label = paste0("75% Single-Year Threshold: ", round(current_year_mgmt2_75pct,2))), hjust = 0, vjust = 2, color = "#CC7722", size = 3) +
  geom_segment(y = current_year_mgmt2_50pct, x = management_period_start, xend = management_period_end,  linetype = "dashed", color = "goldenrod3") +
  geom_text(aes(x = management_period_start, y = current_year_mgmt2_50pct, label = paste0("50% Single-Year Threshold: ", round( current_year_mgmt2_50pct,2))), hjust = 0, vjust = 2, color = "goldenrod3", size = 3) +
  # set current date threshold
  # geom_vline(aes(xintercept = as.numeric(wDay_to_date(wDay_today, current_year)), color = "Current Date", linetype = "Current Date")) +
  geom_label_repel(data = max_loss_by_period,
                   aes(x = max_date, 
                       y = max_cum_loss_mgt, 
                       label = paste0("Cumulative loss: ", round(max_cum_loss_mgt,2),"\n% loss of Single-Year Threshold: ", ifelse(management_period == management_period[1], round((max_cum_loss_mgt/current_year_mgmt1_100pct)*100, 2), round((max_cum_loss_mgt/current_year_mgmt2_100pct)*100, 2)), "%" )),
                   size = 3, 
                   nudge_x = 0, # Adjust nudge_x and nudge_y as needed to position the label
                   nudge_y = 400, 
                   hjust = 0,
                   color = "black") +
  scale_x_date(date_labels = "%m/%d", date_breaks = "1 month", limits = c(start_date,management_period_end), expand = c(.01,.01)) +
  scale_y_continuous(expand = c(0,100)) +
  scale_color_manual(values = c("12/1 - 3/31" = "#0072B2", "4/1 - 6/15" = "#00BFFF"), name = "Management period:") +
  # scale_linetype_manual() +
  labs(title = paste0("Cumulative Loss for WY", plot_year, " with Single-Year Thresholds"),
       subtitle = paste0("Species: Unclipped Steelhead\nCumulative loss 12/31-3/31: ", round(filter(max_loss_by_period, management_period == 	"12/1 - 3/31")$max_cum_loss_mgt,2),
                         "\nCumulative loss 4/1-6/15: ", round(filter(max_loss_by_period, management_period == "4/1 - 6/15")$max_cum_loss_mgt,2)),
       caption = paste0(caption_note, "Data sources: Preliminary data from CDFW; subject to revision.\n", timestamp),
       x = "Date",
       y = "Cumulative Loss") +
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
#automatically generate dots for days with no cumloss reported up to today's date?
#need to find a automatic way to set x and y limit
