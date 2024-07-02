#' @title Current and Historical Cumulative Steelhead Loss
#' @description This script generates a plot of current and historical cumulative steelhead loss data. The plot includes the cumulative loss data for each water year (WY) from 1994 to the current year. The data is faceted by hydrologic type and status. The current year's cumulative loss data is plotted in black, while historical data is plotted in color. The plot also includes a dashed line indicating the maximum cumulative loss for the current year. The maximum cumulative loss for each year is labeled on the plot.
#' @import ggplot2
#' @import dplyr
#' @import lubridate
#' @import scales
#' @import gghighlight
#' @import ggrepel
#' @importFrom ggh4x facet_nested
#' @import here
#' @importFrom magrittr %>% 

# load data
load(here("data/steelhead_loss_data.rda"))
# load wDay to month function
source(here("R/utils_fct_wday_to_month.R"))


#wrangle data

#set current year
source(here("R/utils_fct_assign_current_water_year.R"))
current_year <- assign_current_water_year()

#extract max cumloss of 2024 to adjust color 
max2024LAD <- steelhead_loss_data %>% 
  group_by(WY) %>% 
  filter(WY == current_year) %>% 
  summarise(max_cumloss = max(cum_loss)) %>% 
  pull(max_cumloss)

#extract maximum cumloss for each year (for labels)
max_cumloss_per_year <- steelhead_loss_data %>%
  filter(!is.na(hydro_type_grp)) %>% #currently removes WY2023 or year prior to current year since no hydro assigned
  group_by(WY) %>%
  filter(cum_loss == max(cum_loss)) %>% 
  distinct()

#appending current water year results for each facet regardless of year
loss_current_year <- steelhead_loss_data %>% filter(WY == current_year) %>% select(wday, cum_loss)

# plot
p<- steelhead_loss_data %>%
  group_by(WY, wday, status, hydro_type_grp) %>% 
  # filter(cumloss < 10000) %>%
  #currently removes WY2023 or year prior to current year since no hydro assigned - consider alternative
  filter(!is.na(hydro_type_grp)) %>% 
  ggplot() +
  geom_line( data = . %>% filter(WY == current_year ), aes(x = wday, y = cum_loss, group = WY), color = "black") +
  geom_line( data = . %>% filter(WY != current_year ), aes(x = wday, y = cum_loss, group = WY, color =  hydro_type_grp)) +
  geom_hline(yintercept = max2024LAD, linetype = "dashed", color = "black") +
  geom_text(aes(x = 330, y = max2024LAD), label = "WY2024 cumulative loss", size = 2.5, vjust = -0.5, fontface = "plain" ) +
  gghighlight::gghighlight() +
  ggrepel::geom_text_repel(data = subset(max_cumloss_per_year, cum_loss >= max2024LAD ), #& WY !=2001
                           aes(x = wday, y = cum_loss, label = WY), 
                           size = 3, direction = "y",vjust = 1,  nudge_y = 1, min.segment.length = 0, force = 15) +
  # geom_text(data = year2001_data, 
  #           aes(x = 170, y = 9500, label = "*2001\nmax value = 20,062"), 
  #           size = 3, fontface = "plain") +
  labs(x = 'Date \n(Water Year: Oct-Dec of year [t-1], Jan-Sep of year [t])', 
       y = 'Cumulative Loss', 
       title = 'Cumulative Loss by BiOp Status and Hydrologic Classification Index',
       subtitle = "Species: Steelhead\nData Years: WY1994 to WY2024") +
  scale_x_continuous(breaks = seq(1, 365, by = 61), labels = wDay_to_month( seq(1, 365, by = 61))) + 
  scale_y_continuous(labels = scales::comma,limits = c(0, 40000),  expand = c(0, 0)) +
  scale_color_manual(values = c( "sienna4", "steelblue4"))+
  ggh4x::facet_nested(hydro_type_grp ~ status ) + 
  geom_line(data = loss_current_year, aes(x = wday, y = cum_loss), color = "black") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "grey"),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    panel.border = element_rect(color = "grey", fill = NA),
    panel.spacing = unit(.5, "cm"),
    axis.ticks.x = element_line(color = "black"), 
    text = element_text(size = 15))

print(p)
