#' @title Current and Historical Cumulative Steelhead Loss
#' @description This script generates a plot of current and historical cumulative steelhead loss data. The plot includes the cumulative loss data for each water year (WY) from 1994 to the current year. The data is faceted by hydrologic type and status. The current year's cumulative loss data is plotted in black, while historical data is plotted in color. The plot also includes a dashed line indicating the maximum cumulative loss for the current year. The maximum cumulative loss for each year is labeled on the plot.
require(tidyverse)
require(scales)
require(gghighlight)
require(ggrepel)
require(here)
require(ggh4x)


# Load data
load(here("data/steelhead_loss_data.rda"))
# Load wDay to month function
source(here("R/utils_fct_wday_to_month.R"))

# Set current year
source(here("R/utils_fct_assign_current_water_year.R"))
current_year <- assign_current_water_year()
previous_year <- current_year - 1

# Get the current timestamp
timestamp <- format(Sys.time(), "%d %b %Y %H:%M:%S %Z")

# Check if there is data for the current water year
current_year_data <- steelhead_loss_data %>% filter(WY == current_year)
use_previous_year <- nrow(current_year_data) == 0

# If no data for the current year, use the previous year
if (use_previous_year) {
  current_year_data <- steelhead_loss_data %>% filter(WY == previous_year)
  current_year_label <- paste0("WY", previous_year, " cumulative loss")
  caption_note <- paste0("Data reflects last available data, WY", previous_year)
} else {
  current_year_label <- paste0("WY", current_year, " cumulative loss")
  caption_note <- ""
}

# Extract max cumloss of the current year to adjust color
maxLAD <- current_year_data %>% 
  summarise(max_cumloss = max(cumloss)) %>% 
  pull(max_cumloss)

# Extract maximum cumloss for each year (for labels)
max_cumloss_per_year <- steelhead_loss_data %>%
  filter(!is.na(hydro_type_grp)) %>% # Currently removes WY2023 or year prior to current year since no hydro assigned
  group_by(WY) %>%
  filter(cumloss == max(cumloss)) %>% 
  distinct()

# Appending current water year results for each facet regardless of year
loss_current_year <- current_year_data %>% select(wDay, cumloss)

# Plot
p <- steelhead_loss_data %>%
  group_by(WY, wDay, status, hydro_type_grp) %>% 
  filter(!is.na(hydro_type_grp)) %>% 
  ggplot() +
  geom_line(data = . %>% filter(WY == current_year), aes(x = wDay, y = cumloss, group = WY), color = "black") +
  geom_line(data = . %>% filter(WY != current_year), aes(x = wDay, y = cumloss, group = WY, color = hydro_type_grp)) +
  geom_hline(yintercept = maxLAD, linetype = "dashed", color = "black") +
  geom_text(aes(x = 330, y = maxLAD), label = current_year_label, size = 2.5, vjust = -0.5, fontface = "plain") +
  gghighlight::gghighlight() +
  ggrepel::geom_text_repel(data = subset(max_cumloss_per_year, cumloss >= maxLAD), 
                           aes(x = wDay, y = cumloss, label = WY), 
                           size = 3, direction = "y", vjust = 1, nudge_y = 1, min.segment.length = 0, force = 15) +
  labs(x = 'Date', 
       y = 'Cumulative Loss', 
       title = 'Cumulative Loss by BiOp Status and Hydrologic Classification Index',
       subtitle = paste0("Species: Clipped and Unclipped Steelhead\nData Years: WY", min(steelhead_loss_data$WY), " to WY", max(steelhead_loss_data$WY),
                         "\nCurrent Cumulative Loss: ", round(maxLAD, 2)),
       caption = paste0(caption_note, "\nData source: Preliminary data from CDFW; subject to revision. Hydrological Classification Index from CDEC\n", timestamp, "\n")) +
  scale_x_continuous(breaks = seq(1, 365, by = 61), labels = wDay_to_month(seq(1, 365, by = 61))) + 
  scale_y_continuous(labels = scales::comma, limits = c(0, 40000), expand = c(0, 0)) +
  scale_color_manual(values = c("#D95F02", "#00BFFF")) +
  ggh4x::facet_nested(hydro_type_grp ~ status) + 
  geom_line(data = loss_current_year, aes(x = wDay, y = cumloss), color = "black", size = 1) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(linetype = "dotted"),
    panel.border = element_rect(color = "grey", fill = NA),
    panel.spacing = unit(.5, "cm"),
    axis.ticks = element_line(size = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    text = element_text(size = 15)
  )

print(p)
