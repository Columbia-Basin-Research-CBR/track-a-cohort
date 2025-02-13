#' @title Figure 6. Steelhead Daily Loss and Export (per Facility pending)
#' @description This script is more of a holder space for plot that will include export per facility and loss per facility for steelhead
#' Current data shows total daily loss and export for both facilities
#'
require(tidyverse)
require(here)
require(patchwork)

# Get the current timestamp
timestamp <- format(Sys.time(), "%d %b %Y %H:%M:%S %Z")

#set current year
source(here("R/utils_fct_assign_current_water_year.R"))
current_year <- assign_current_water_year()
previous_year <- current_year - 1

# Load the data
load(here::here("data/steelhead_loss_export_data.rda"))


# Check if the current WY is present in the data
use_previous_year <- !any(steelhead_loss_export_data$WY == current_year)


# If no data for the current year, use the previous year for omrValues and add a caption
if (use_previous_year) {
  omrValues <- NULL
  caption_note <- paste0("No data reported for Current WY", current_year, ". Data reflects last available data (WY", previous_year, ").\n")
  water_year_text <- paste("Water Year:", previous_year)
} else {
  omrValues <- NULL
  caption_note <- ""
  water_year_text <- paste("Current Water Year:", current_year)
}

# Calculate the ratio for the secondary axis- updated to use 1 if not steelheadloss data reported 
ratio <- ifelse(!any(!is.na(steelhead_loss_export_data$daily_total_loss)), 1, 
                max(steelhead_loss_export_data$daily_total_loss, na.rm = TRUE) / max(steelhead_loss_export_data$pumping_discharge_cfs))

# Define the start and end dates for the water year
start_date <- as.Date(paste0(previous_year, "-10-01"))
end_date <- as.Date(paste0(current_year, "-09-30"))

#set facility as factor to prevent dropping facet in grid
steelhead_loss_export_data$facility <- factor(steelhead_loss_export_data$facility, levels = c("CVP", "SWP"))


p1 <- steelhead_loss_export_data %>%
  ggplot(aes(x = date)) +
  geom_bar(aes(y = daily_total_loss, fill = facility), stat = "identity", position = position_dodge2(preserve = "single"), width = 2) + # Changed color to fill for bar
  geom_line(aes(y = pumping_discharge_cfs * ratio, color = facility)) + # Apply ratio to y
  geom_point(aes(y = pumping_discharge_cfs * ratio, color = facility)) + # Apply ratio to y
  {if (!is.null(omrValues)) geom_vline(data = omrValues, aes(xintercept = date), color = 'black')} +
  {if (!is.null(omrValues)) geom_text(data = omrValues, aes(x = date, y = 200, label = paste("OMR:", value)), color = 'black', angle = -90, size = 3, vjust = 1)} +
  labs(title = "Daily Loss and Export at CVP/SWP Facilities",
       subtitle = paste("Species: Clipped and Unclipped Steelhead",
                        "\n", water_year_text,
                        "\n\nComparison of facilities, CVP and SWP"),
       x = NULL,
       y = "Daily loss",
       y.sec = "Pumping Discharge (cfs)",
       fill = "Daily loss by facility: ",
       color = "Pumping discharge by facility:",
       caption = NULL) +
  scale_x_date(date_breaks = "2 month", date_labels = "%m/%y", limits = c(start_date, end_date)) +
  scale_y_continuous(sec.axis = sec_axis(~./ratio, name = "Pumping Discharge (cfs)")) + # Add secondary axis
  scale_color_manual(values = c("#F5C767", "#00BFFF")) +
  scale_fill_manual(values = c("#CE900D","#0072B2" )) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.subtitle =  element_text(face = "plain", size = 13),
        text = element_text(size = 15),
        axis.ticks.y.right = element_line(color = "grey"),
        panel.grid.major = element_line(linetype = "dotted", color = "grey"),
        panel.grid.minor = element_blank(),
        
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5))

p2 <- steelhead_loss_export_data %>%
  ggplot(aes(x = date)) +
  geom_bar(aes(y = daily_total_loss, fill = facility), stat = "identity", position = position_dodge2(preserve = "single"), width = 7) + # Changed color to fill for bar
  geom_line(aes(y = pumping_discharge_cfs * ratio, color = facility)) + # Apply ratio to y
  geom_point(aes(y = pumping_discharge_cfs * ratio, color = facility)) + # Apply ratio to y
  labs(title = NULL,
       x = "Date",
       y = "Daily Loss",
       y.sec = "Pumping Discharge (cfs)",
       fill = "Daily loss by facility: ",
       color = "Pumping by facility:",
       caption = paste0(caption_note, "Data sources: Daily loss and Export Preliminary data from CDFW;\nCFS from the CDEC sites HRO (for SWP) and TRP (for CVP)\n", timestamp)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%m/%y", limits = c(start_date, end_date)) +
  scale_y_continuous(sec.axis = sec_axis(~./ratio, name = "Pumping Discharge (cfs)")) + # Add secondary axis
  scale_color_manual(values = c("#F5C767", "#00BFFF")) +
  scale_fill_manual(values = c("#CE900D","#0072B2" )) +
  theme_minimal() +
  facet_grid(~facility, drop = FALSE) +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        axis.ticks.y.right = element_line(color = "grey"),
        panel.grid.major = element_line(linetype = "dotted"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5))

p <- p1 / p2

print(p)
