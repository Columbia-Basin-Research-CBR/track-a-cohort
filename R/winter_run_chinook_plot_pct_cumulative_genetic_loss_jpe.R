#' Figure 3b: Winter-run Chinook JPE Cumulative Genetic Percent Loss
#' @description This script includes shared genetic data from BOR to plot the cumulative genetic loss of the JPE for the current water year compared to historical years. The current water year is highlighted and the past years are colored by hydrologic water year type.
#' @details The data is sourced from `data` > `jpe_cumulative_loss_genetic_data.rda` and is wrangled in `data-raw` > `import_winter_run_chinook_cumulative_genetic_loss_data.R`.
#' @return A static plot comparing the cumulative genetic loss of the JPE for the current water year compared to historical years.
#' @import ggplot2
#' @import dplyr
#' @import here
#' @noRd

# import data file
load(here("data/jpe_cumulative_loss_genetic_data.rda"))

# import wDay to month function
source(here("R/utils_fct_wday_to_month.R"))

# extract current year
current_year<-year(today())

# calculate the maximum pct_cumloss for the current year
max_pct_cumloss_current_year <- jpe_cumulative_loss_genetic_data %>%
  filter(WY == current_year) %>%
  group_by(WY) %>% 
  summarise(max_pct_cumloss = max(pct_genetic_cumloss_jpe)) %>%
  pull(max_pct_cumloss)

#extract maximum pct_cumloss for each year (for labels)
max_pct_cumloss_per_year <-jpe_cumulative_loss_genetic_data %>%
  group_by(WY) %>%
  filter(pct_genetic_cumloss_jpe == max(pct_genetic_cumloss_jpe))

# filter data for the year 2001 (for year labels excluding >10,000)
year2001_data <- jpe_cumulative_loss_genetic_data %>% filter(WY == 2001, pct_genetic_cumloss_jpe < .01)
maxvalue2001<- jpe_cumulative_loss_genetic_data %>% 
  filter(WY == 2001) %>% 
  summarise(maxvalue = max(pct_genetic_cumloss_jpe)) %>% 
  pull(maxvalue)

#plot all years together  
p <- jpe_cumulative_loss_genetic_data %>% 
  #remove values greater than 1% for better visualization
  filter(pct_genetic_cumloss_jpe < .01) %>%
  ggplot() +
  geom_line( data = . %>% filter( WY != current_year), aes(x=wDay, y = pct_genetic_cumloss_jpe, group = WY, color = hydro_type_grp ), lwd = .5)+
  geom_line( data = . %>% filter( WY == current_year), aes( x=wDay, y = pct_genetic_cumloss_jpe, group = WY, linetype = as.factor(WY)), color = "black", lwd = 2)+
  labs(x = 'Date \n(Water Year: Oct-Dec of year [t-1], Jan-Sep of year [t])', 
       y = 'Percent Cumulative Loss', 
       title = 'Winter-run Chinook salmon - Current and Historical Percent Cumulative Genetic Loss of JPE',
       subtitle = " Data includes: WY1994 to WY2024",
       color = "Hydrological Year Type:",
       linetype = "Current Water Year:") +
  scale_x_continuous(breaks = seq(1, 365, by = 61), labels = wDay_to_month( seq(1, 365, by = 61))) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("sienna4", "steelblue4", "grey"), 
                     labels = c("Below Normal, Dry, & Critical", "Wet, Above Normal", "WY2023, unassigned"))+ #look into automating naming for NA
  ggrepel::geom_text_repel(data = max_pct_cumloss_per_year %>% 
                                  filter(pct_genetic_cumloss_jpe >= max_pct_cumloss_current_year & WY !=2001), 
                           aes(x = wDay, y = pct_genetic_cumloss_jpe, label = WY, color = hydro_type_grp), 
                           size = 3,
                           force = 1,
                           nudge_x = 50,
                           nudge_y = .0005,
                           hjust = 0,
                           direction = "y",
                           segment.size = .2 ) +
  geom_text(data = year2001_data, 
            aes(x = 160, y = .0098, label = paste0("*2001\nmax value = ", round(maxvalue2001*100, 2), " %")), 
            size = 3, fontface = "plain", color = "sienna4") +
  guides(linetype = guide_legend(order = 1), color = guide_legend(order = 2, override.aes = aes(label = ""))) + #override to remove "a" from geom_text_repel in legend
  theme_minimal() +
  theme(legend.position = "right",
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        axis.line.x = element_line(color = "black", .5),
        axis.line.y = element_line(color = "black", .5),
        axis.ticks = element_line(color = "black"), 
        text = element_text(size = 15))

print(p)
