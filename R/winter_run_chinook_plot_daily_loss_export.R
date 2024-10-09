#' @title  Winter-run Chinook Daily Loss and Export (per Facility pending)
#' @description This script is more of a holder space for plot that will include export per facility and loss per facility for steelhead
#' Current data shows total daily loss and export for both facilities
#' 
require(tidyverse)
require(here)
require(patchwork)

# Get the current timestamp
timestamp <- format(Sys.time(), "%d %b %Y %H:%M:%S %Z")

# Load the data
load(here::here("data/winter_run_chinook_loss_export_data.rda"))

#set current year
source(here("R/utils_fct_assign_current_water_year.R"))
current_year <- assign_current_water_year()

# Check if the current WY is present in the data
use_previous_year <- !any(winter_run_chinook_loss_export_data$WY == current_year)


# If no data for the current year, use the previous year for omrValues and add a caption
if (use_previous_year) {
  omriValues <- data.frame(value = c(-5000, -3500, -2000, -5000, -3500, -2500, -500, -1500, -2500, "COA 8.17"),
                          date = lubridate::ymd(c('2024-01-01', '2024-01-14', '2024-01-23', '2024-02-04', '2024-02-08',
                                                  '2024-02-17', '2024-03-11', '2024-02-26', '2024-04-01', '2024-04-09')))
  caption_note <- paste0("No data reported for Current WY", current_year, ". Data reflects last available data (WY", previous_year, ").\n")
  water_year_text <- paste("Water Year:", previous_year)
} else {
  omriValues <- NULL
  caption_note <- ""
  water_year_text <- paste("Current Water Year:", current_year)
}

# Calculate the ratio for the secondary axis
ratio <- max(winter_run_chinook_loss_export_data$daily_total_loss) / max(winter_run_chinook_loss_export_data$pumping_discharge_cfs)

p1 <- winter_run_chinook_loss_export_data %>% 
  ggplot(aes(x = date)) +
  geom_bar(aes(y = daily_total_loss, fill = facility), stat = "identity", position = position_dodge2(preserve = "single")) + # Changed color to fill for bar
  geom_line(aes(y = pumping_discharge_cfs * ratio, color = facility)) + # Apply ratio to y
  geom_vline(omriValues, mapping = aes(xintercept = date), color = 'black') +
  geom_text(omriValues, mapping = aes(x = date, y = 200, label = paste("OMRI:", value)), color = 'black', angle = -90, size = 3, vjust = 1) +
  labs(title = "Daily Loss and Export at CVP/SWP Facilities",
       subtitle = paste("Species: Natural Winter-run Chinook",
                        "\nCurrent Water Year:", water_year_text,
                        "\n\nComparison of facilities, CVP and SWP"),
       x = NULL,
       y = "Daily loss",
       y.sec = "Pumping Discharge (cfs)",
       fill = "Daily loss by facility: ",
       color = "Pumping discharge by facility:") +
  scale_x_date(date_breaks = "2 month", date_labels = "%m/%y") +
  scale_y_continuous(sec.axis = sec_axis(~./ratio, name = "Pumping Discharge (cfs)")) + # Add secondary axis
  scale_color_manual(values = c("#F5C767", "#00BFFF")) +
  scale_fill_manual(values = c("#CE900D","#0072B2" )) +
  theme_minimal() + 
  theme(legend.position = "none",
        plot.subtitle =  element_text(face = "plain", size = 13),
        text = element_text(size = 15),
        # axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.major = element_line(linetype = "dotted", color = "grey"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5))



p2 <- winter_run_chinook_loss_export_data %>% 
  ggplot(aes(x = date)) +
  geom_bar(aes(y = daily_total_loss, fill = facility), stat = "identity", position = position_dodge2(preserve = "single")) + # Changed color to fill for bar
  geom_line(aes(y = pumping_discharge_cfs * ratio, color = facility)) + # Apply ratio to y
  labs(title = NULL,
       caption = paste0(caption_note, "Data sources: Daily loss and Export Preliminary data from CDFW;\nCFS from the CDEC sites HRO (for SWP) and TRP (for CVP)\n", timestamp),
       x = "Date",
       y = "Daily Loss",
       y.sec = "Pumping Discharge (cfs)",
       fill = "Daily loss by facility: ",
       color = "Pumping by facility:") + 
  scale_x_date(date_breaks = "2 month", date_labels = "%m/%y") +
  scale_y_continuous(sec.axis = sec_axis(~./ratio, name = "Pumping Discharge (cfs)")) + # Add secondary axis
  scale_color_manual(values = c("#F5C767", "#00BFFF")) +
  scale_fill_manual(values = c("#CE900D","#0072B2" )) +
  theme_minimal() + 
  facet_grid(~facility) +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        # axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.major = element_line(linetype = "dotted"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5))

p <- p1  / p2

print(p)

