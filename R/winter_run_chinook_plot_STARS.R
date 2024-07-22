#' @title Figure 2: Survival and Routing Probabilities of Winter-run Chinook Salmon in the Sacramento River
#' @description static ggplot of STARS model survival and routing probabilities for winter-run Chinook salmon in the Sacramento River-- also used in Steelhead figures as a surrogate
#' @details To access the data file, navigate to the `data` directory located at the
#' project's root. The file is named `STARS_data.rda` and is sourced from `data-raw >import_STARS_data.R` which includes `data-raw >STARS.shinyinputs.Rdata`. 
#' @noRd
#' 
require(tidyverse)
require(here)
require(gghighlight)
require(patchwork)


#fct to convert water day to month
source(here("R/utils_fct_wday_to_month.R"))

#load data file
load(here::here("data", "STARS_data.rda"))

#set current year
source(here("R/utils_fct_assign_current_water_year.R"))
       current_year <- assign_current_water_year()
       
       # Generate a dynamic color palette
       unique_years <- unique(STARS_data$WY)
       colors <- c("#E69F00", "#56B4E9","#009E73","#999999","#0072B2" ,"#D55E00", "#CC79A7", "#F0E442")#grDevices::palette.colors(min(length(unique_years), 8), "Okabe-Ito")  
       names(colors) <- as.character(unique_years)
       
       # Override color for current year to black
       colors[as.character(current_year)] <- "black"


#plot STARS data
# Overall Survival
p1 <- ggplot(STARS_data, aes(x = wDay, group = WY )) +
  geom_line(aes(y = surv, color = as.factor(WY))) +
  geom_ribbon(data = STARS_data %>% filter(WY == current_year), aes(ymin = survL80, ymax = survU80), alpha = 0.25) +
  labs(x = 'Month', 
       y = 'Apparent survival probability', 
       title = 'Overall Survival', 
       subtitle = "Median survival of daily cohorts for all routes combined.") +
  scale_x_continuous(breaks = seq(1, 365, by = 61), labels = wDay_to_month( seq(1, 365, by = 61))) + 
  # gghighlight(WY == current_year, use_direct_label = FALSE) +
  scale_color_manual(values = colors, name = "Water Year") +
  theme_minimal() +
  theme(legend.position = c(.9, 0.7),
        panel.border = element_rect(color = "grey", fill = NA, linewidth = .25),
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(),
        text = element_text(size = 15))

# Interior Delta Route-specific Survival Probability
p2 <- ggplot(STARS_data, aes(x = wDay, group = WY)) +
  geom_line(aes(y = idsurv, color = as.factor(WY))) +
  geom_ribbon(data = STARS_data %>% filter(WY == current_year), aes(ymin = idsurvL80, ymax = idsurvU80), alpha = 0.25) +
  labs(x = 'Month', 
       y = 'Apparent survival probability', 
       title = 'Interior Delta Route-specific Survival Probability', 
       subtitle = "Median route-specific survival of daily cohorts using the Interior Delta route") +
  scale_x_continuous(breaks = seq(1, 365, by = 61), labels = wDay_to_month( seq(1, 365, by = 61))) + 
  scale_color_manual(values = colors, name = "Water Year") +
  theme_minimal() +
  theme(legend.position = c(.9, 0.7),
        panel.border = element_rect(color = "grey", fill = NA, linewidth = .25),
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(),
        text = element_text(size = 15))


# Interior Delta Route-specific Probability
p3 <- ggplot(STARS_data, aes(x = wDay, group = WY)) +
  geom_line(aes(y = idRoute, color = as.factor(WY))) +
  geom_ribbon(data = STARS_data %>% filter(WY == current_year), aes(ymin = idRouteL80, ymax = idRouteU80 ), alpha = 0.25) +
  labs(x = 'Month', 
       y = 'Route probability', 
       title = 'Interior Delta Route-specific Probability', 
       subtitle = "Proportion of daily cohorts using the Interior Delta route") +
  scale_x_continuous(breaks = seq(1, 365, by = 61), labels = wDay_to_month( seq(1, 365, by = 61))) + 
  scale_color_manual(values = colors, name = "Water Year") +
  theme_minimal() +
  theme(legend.position = c(.9, 0.3),
        panel.border = element_rect(color = "grey", fill = NA, linewidth = .25),
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(),
        text = element_text(size = 15)) 


# Combined plots
p <- p1 / p2 / p3

# Add title and caption
p_stars_combined <- p +  
  plot_annotation(title = "STARS Model - Predicted Winter Run Chinook Salmon daily cohorts passage from Knights Landing to Chipps Island",
                  caption = "Data source: STARS Shiny App, Years of data: 2018 to 2024")


print(p_stars_combined)

