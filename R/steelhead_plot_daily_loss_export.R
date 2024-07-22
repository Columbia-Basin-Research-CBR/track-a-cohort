#' @title Figure 6. Steelhead Daily Loss and Export (per Facility pending)
#' @description This script is more of a holder space for plot that will include export per facility and loss per facility for steelhead
#' Current data shows total daily loss and export for both facilities
#' 
require(tidyverse)
require(here)


# Load the data
load(here::here("data/steelhead_loss_export_data.rda"))

# adding horizontal lines -- provided by BOR. Confirm how these are designated
omrValues <- data.frame(value = c(-5000,-3500,-2000,-5000,-3500,-2500,-500,-1500,-2500, "COA 8.17"),#,'COA 8.17'
                        date = lubridate::ymd(c('2024-01-01', '2024-01-14', '2024-01-23', '2024-02-04', '2024-02-08',
                                      '2024-02-17', '2024-03-11', '2024-02-26', '2024-04-01', '2024-04-09')))

#Determine addition of OMR values -- how to set? Based on OMR management rule?
# omrValues<- steelhead_loss_export_data %>% 
#   filter(!is.na(OMR_14d))

# Calculate the ratio for the secondary axis
ratio <- max(steelhead_loss_export_data$daily_total_loss) / max(steelhead_loss_export_data$pumping_discharge_cfs)

p1 <- steelhead_loss_export_data %>% 
  ggplot(aes(x = date)) +
  geom_bar(aes(y = daily_total_loss, fill = facility), stat = "identity", position = position_dodge2(preserve = "total", width = .6)) + # Changed color to fill for bar
  geom_line(aes(y = pumping_discharge_cfs * ratio, color = facility)) + # Apply ratio to y
  geom_vline(omrValues, mapping = aes(xintercept = date), color = 'black') +
  geom_text(omrValues, mapping = aes(x = date, y = 200, label = paste("OMR:", value)), color = 'black', angle = -90, size = 3, vjust = 1) +
  labs(title = "Daily Loss and Export",
       subtitle = paste("Species: Steelhead",
                        "\nCurrent Water Year:", current_year,
                        "\n\nComparison of facilities, CVP and SWP"),
       x = NULL,
       y = "Daily loss",
       y.sec = "Pumping Discharge (cfs)",
       color = "Facility",
       fill = "Facility") +
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%y") +
  scale_y_continuous(sec.axis = sec_axis(~./ratio, name = "Pumping Discharge (cfs)")) + # Add secondary axis
  scale_color_manual(values = c("#E1BE6A", "#40B0A6")) +
  scale_fill_manual(values = c("#E1BE6A", "#40B0A6")) +
  theme_minimal() + 
  theme(legend.position = "none",
        plot.subtitle =  element_text(face = "plain", size = 13),
        text = element_text(size = 15),
        # axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5))



p2 <- steelhead_loss_export_data %>% 
  ggplot(aes(x = date)) +
  geom_bar(aes(y = daily_total_loss, fill = facility), stat = "identity", width = .6) + # Changed color to fill for bar
  geom_line(aes(y = pumping_discharge_cfs * ratio, color = facility)) + # Apply ratio to y
  labs(title = NULL,
       x = "Date",
       y = "Daily Loss",
       y.sec = "Pumping Discharge (cfs)",
       color = "Facility",
       fill = "Facility") + 
  scale_x_date(date_breaks = "2 month", date_labels = "%m/%y") +
  scale_y_continuous(sec.axis = sec_axis(~./ratio, name = "Pumping Discharge (cfs)")) + # Add secondary axis
  scale_color_manual(values = c("#E1BE6A", "#40B0A6")) +
  scale_fill_manual(values = c("#E1BE6A", "#40B0A6")) +
  theme_minimal() + 
  facet_grid(~facility) +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        # axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5))

p <- p1  / p2

print(p)

