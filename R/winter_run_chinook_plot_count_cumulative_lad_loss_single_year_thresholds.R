#' Figure 5: Winter-run Chinook Cumulative Loss for current water year -- LAD
#' @details
#' Single-Year Loss Thresholds (PA 4-69, 2019 BiOP)
#' In each year, typically January/February, Reclamation and DWR propose to avoid exceeding an annual loss threshold equal to 90% of the greatest annual loss that occurred in the historical record 2010-2018 for each of:
#' Natural Winter-Run Chinook Salmon (loss= 1.17% of JPE)
#' Natural Central Valley Steelhead from December through March (loss =1,414)
#' Natural Central Valley Steelhead from April through June 15 (loss = 1,552)
#' (More information on PA 4-70, 2019 BiOP)
#' @import ggplot2
#' @import dplyr
#' @import here
#' @import lubridate
#' @importFrom magrittr %>%

# import data file
#loss data from sacpas
load(here("data/jpe_lad_loss_data.rda"))


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

# extract lad cumulative loss data and add CY date column
lad_cumulative_loss_data <- jpe_lad_loss_data$lad_cumulative_loss_data 

#convert back to CY date
lad_cumulative_loss_data$date <- as_date(mapply(wDay_to_date, lad_cumulative_loss_data$wDay, lad_cumulative_loss_data$WY))

# extract maximum cumloss for the current year
cumloss_current_year <- lad_cumulative_loss_data %>%
  filter(WY == current_year) 

# Calculate the first date minus 2 weeks to give space for labels - adjust as needed
set_text_date <- min(cumloss_current_year$date) - weeks(2)

# extract current water year JPE and set single year threshold @ 2% of JPE -- return single value
jpe_current_year_2pct <- lad_cumulative_loss_data %>%
  filter(WY == current_year, cumloss == max(cumloss)) %>%
  pull(jpe)*.02

#extract current water year JPE and set single year threshold per PA 4-69 @ 1.17% of JPE -- return single value
jpe_current_year_1.17pct <- lad_cumulative_loss_data %>%
  filter(WY == current_year, cumloss == max(cumloss)) %>%
  pull(jpe)*.0117


max_loss_threshold_2010_to_2018 <- lad_cumulative_loss_data %>% 
  filter(between(WY, 2010, 2018)) %>%
  ungroup() %>% 
  summarise(max_loss = max(cumloss)) %>%
  pull(max_loss)

# plot
p <- cumloss_current_year %>% 
  ggplot(aes(x= date, y = cumloss)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = jpe_current_year_2pct, linetype = "dashed", color = "purple4") +
  geom_text(aes(x = set_text_date, y = jpe_current_year_2pct, label = paste0("Single-Year Threshold (2% of JPE): ", jpe_current_year_2pct)), hjust = 0, vjust = 2, color = "purple4", size = 3) +
  geom_hline(yintercept = jpe_current_year_1.17pct, linetype = "dashed", color = "red4") +
  geom_text(aes(x = set_text_date, y = jpe_current_year_1.17pct, label = paste0("100% Single-Year Threshold (1.17% of JPE): ", round(jpe_current_year_1.17pct,2))), hjust = 0, vjust = 2, color = "red4", size = 3) +
  geom_hline(yintercept = jpe_current_year_1.17pct*.75, linetype = "dashed", color = "orange3") +
  geom_text(aes(x = set_text_date, y = jpe_current_year_1.17pct*.75, label = paste0("75% Single-Year Threshold (1.17% of JPE): ", round(jpe_current_year_1.17pct*.75,2))), hjust = 0, vjust = 2, color = "orange3", size = 3) +
  geom_hline(yintercept = jpe_current_year_1.17pct*.50, linetype = "dashed", color = "goldenrod2") +
  geom_text(aes(x = set_text_date, y = jpe_current_year_1.17pct*.5, label = paste0("50% Single-Year Threshold (1.17% of JPE): ", round(jpe_current_year_1.17pct*.5,2))), hjust = 0, vjust = 2, color = "goldenrod1", size = 3) +
  geom_vline(xintercept = as.numeric(wDay_to_date(wDay_today, current_year)), linetype = "dashed", color = "blue2") +
  geom_label_repel(data = data.frame(date = max(cumloss_current_year$date), cumloss = max(cumloss_current_year$cumloss)),
                   aes(x = date, y = cumloss, label = paste0("Cumulative loss: ", max(cumloss),"\n% of Single-Year Threshold: ", round((cumloss/jpe_current_year_2pct)*100, 2), "%")),
                   size = 3, 
                   nudge_x = 1, # Adjust nudge_x and nudge_y as needed to position the label
                   nudge_y = 400, 
                   hjust = 0,
                   color = "black") +
  scale_x_date(date_labels = "%m/%d", date_breaks = "1 month") +
  scale_y_continuous(expand = c(0,NA), limits = c(0,5000)) +
  labs(title = "Cumulative LAD Loss for Current Water Year with Single-Year Thresholds",
       subtitle = paste0("Winter-run Chinook\nCumulative loss to date: ", max(cumloss_current_year$cumloss),
                         "\nProgress toward 2% Single-Year Threshold: ", round((max(cumloss_current_year$cumloss)/jpe_current_year_2pct)*100, 2), "%"),
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