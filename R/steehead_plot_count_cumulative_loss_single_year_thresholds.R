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
       current_year <- assign_current_water_year()

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
  filter(WY == current_year) %>% 
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

# Calculate the first date minus 2 weeks to give space for labels - adjust as needed
set_text_date <- min(cumloss_current_year$date) - weeks(2)

# Update to set 100% loss limit when known
current_year_100pct <- 1414 + 1552 # setting 100% loss limit for steelhead at Dec-June loss reported on SacPAS
current_year_75pct <- current_year_100pct*.75
current_year_50pct <- current_year_100pct*.50

# set management threshold line
management_period <- as.numeric(ymd(paste0(current_year,"-03-31")))

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
  ggplot(aes(x= date, y = cum_loss_mgt)) +
  geom_point(data = . %>% filter(management_period == management_period[1])) +
  geom_line(data = . %>% filter( management_period == management_period[1])) +
  geom_point(data = . %>% filter( management_period == management_period[2])) +
  geom_line(data = . %>% filter(management_period == management_period[2])) +
  geom_vline(xintercept = management_period, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = current_year_100pct, linetype = "dashed", color = "red4") +
  geom_text(aes(x = set_text_date, y = current_year_100pct, label = paste0("100% Single-Year Threshold: ", round(current_year_100pct,2))), hjust = 0, vjust = 2, color = "red4", size = 3) +
  geom_hline(yintercept = current_year_75pct, linetype = "dashed", color = "#CC7722") +
  geom_text(aes(x = set_text_date, y = current_year_75pct, label = paste0("75% Single-Year Threshold: ", round(current_year_75pct,2))), hjust = 0, vjust = 2, color = "#CC7722", size = 3) +
  geom_hline(yintercept = current_year_50pct, linetype = "dashed", color = "goldenrod3") +
  geom_text(aes(x = set_text_date, y = current_year_50pct, label = paste0("50% Single-Year Threshold: ", round( current_year_50pct,2))), hjust = 0, vjust = 2, color = "goldenrod3", size = 3) +
  geom_vline(xintercept = as.numeric(wDay_to_date(wDay_today, current_year)), linetype = "dashed", color = "blue2") +
  geom_label_repel(data = max_loss_by_period,
                   aes(x = max_date, 
                       y = max_cum_loss_mgt, 
                       label = paste0("Cumulative loss: ", round(max_cum_loss_mgt,2),"\n% of Single-Year Threshold: ", round((max_cum_loss_mgt/current_year_100pct)*100, 2), "%")),
                   size = 3, 
                   nudge_x = 1, # Adjust nudge_x and nudge_y as needed to position the label
                   nudge_y = 400, 
                   hjust = 0,
                   color = "black") +
  scale_x_date(date_labels = "%m/%d", date_breaks = "1 month") +
  # scale_y_continuous(expand = c(0,NA), limits = c(0,5000)) +
  labs(title = "Cumulative LAD Loss for Current Water Year with Single-Year Thresholds",
       subtitle = paste0("Species: Steelhead\nCumulative loss 12/31-3/31: ", round(filter(max_loss_by_period, management_period == 	"12/1 - 3/31")$max_cum_loss_mgt,2),
                         "\nCumulative loss 4/1-6/15: ", round(filter(max_loss_by_period, management_period == "4/1 - 6/15")$max_cum_loss_mgt),2),
       x = "Date",
       y = "Cumulative Loss") +
  theme_minimal() +
  theme(
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "black", fill = "transparent"),
        # plot.background = element_rect(color = "black", fill = "white"),
        legend.position = "bottom", 
        text = element_text(size = 15))

print(p)

#Notes
#automatically generate dots for days with no cumloss reported up to today's date?
#need to find a automatic way to set x and y limit