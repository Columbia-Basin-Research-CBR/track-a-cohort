#' @title Delta STARS Survival and Routing Probabilities of Winter-run Chinook Salmon in the Sacramento River
#' @description static ggplot of STARS model survival and routing probabilities for winter-run Chinook salmon in the Sacramento River-- also used in Steelhead figures as a surrogate
#' @details To access the data file, navigate to the `data` directory located at the
#' project's root. The file is named `STARS_data.rda` and is sourced from `data-raw >import_STARS_data.R` which includes `data-raw >STARS.shinyinputs.Rdata`. 
#' @noRd
#' 
require(tidyverse)
require(here)
require(patchwork)


#fct to convert water day to month
source(here("R/utils_fct_wday_to_month.R"))

#load data file
load(here::here("data", "STARS_data.rda"))

# filter to include data needed for this plot type (not shinyapp)
STARS_data <- STARS_data %>% 
  dplyr::filter(use_case == "STARS_plot_type_1") 

# Get the current timestamp
timestamp <- format(Sys.time(), "%d %b %Y %H:%M:%S %Z")

#set current year
source(here("R/utils_fct_assign_current_water_year.R"))
       current_year <- assign_current_water_year()
       
       # Generate a dynamic color palette and linetype for new years
       get_color_linetype <- function(unique_years) {
         
         # assign max colors available in Okabe-Ito palette
         max_colors <-  c("#E69F00", "#56B4E9","#009E73","#999999", "#0072B2", "#D55E00", "#CC79A7", "#F0E442") #pulled from: grDevices::palette.colors(9, "Okabe-Ito") w/o black
         
         n <- length(unique_years)

         colors <- rep(max_colors, length.out = n) #repeat paletter if n > max_colors
         names(colors) <- as.character(unique_years)
        
         
         lty_styles <- c("solid", "dashed", "dotted", "dotdash", "longdash")
         cycle_index <- floor((seq_len(n) - 1) / length(max_colors))
         linetypes <- lty_styles[(cycle_index %% length(lty_styles)) + 1]
         names(linetypes) <- as.character(unique_years)   
         
         
         return(list(colors = colors, linetypes = linetypes))
       }
       
       
       # Apply to unique years
       unique_years <- unique(STARS_data$WY)
       color_linetype <- get_color_linetype(unique_years)
       
       # Extract colors and line types
       colors <- color_linetype$colors
       linetypes <- color_linetype$linetypes
       
       # Override the color and line type for the current year
       colors[as.character(current_year)] <- "black"
       linetypes[as.character(current_year)] <- "solid"
       



#plot STARS data
# Overall Survival
p1 <- ggplot(STARS_data, aes(x = wDay, group = WY )) +
  geom_line(aes(y = surv, color = as.factor(WY), linetype = as.factor(WY), size = ifelse(WY == current_year, .75, 0.5))) +
  geom_ribbon(data = STARS_data %>% filter(WY == current_year), aes(ymin = survL80, ymax = survU80), alpha = 0.25) +
  geom_ribbon(data = STARS_data %>% filter(WY != current_year), aes(ymin = survL80, ymax = survU80, fill = as.factor(WY)), alpha = 0.08) +
  labs(x = 'Month', 
       y = 'Apparent survival probability', 
       subtitle = "Delta STARS Model -\nPredicted Natural Winter-run Chinook Daily Cohorts Passage, Knights Landing to Chipps Island", 
       title = "Overall Survival: Median survival of daily cohorts for all routes combined",
       caption = paste0("Data source: Delta STARS developed by USGS Quantitative Fisheries Ecology Section and deployed by SacPAS.\n", timestamp),
       color = "Water Year,\nmedian with 80% CI",
       linetype = "Water Year,\nmedian with 80% CI") +
  scale_x_continuous(breaks = seq(1, 365, by = 61), labels = wDay_to_month( seq(1, 365, by = 61))) + 
  scale_y_continuous( expand = c(0,0)) +
  scale_color_manual(values = colors) +
  scale_linetype_manual(values = linetypes) +
  scale_fill_manual(values = colors) +
  scale_size_identity() +
  guides(fill = "none") +
  theme_minimal() +
  theme(legend.position = c(.9, 0.7),
        text = element_text(size = 15),
        panel.grid.major = element_line(linetype = "dotted"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()

       )

print(p1)

# Interior Delta Route-specific Survival Probability
p2 <- ggplot(STARS_data, aes(x = wDay, group = WY)) +
  geom_line(aes(y = idsurv, color = as.factor(WY), linetype = as.factor(WY), size = ifelse(WY == current_year, .75, 0.5))) +
  geom_ribbon(data = STARS_data %>% filter(WY == current_year), aes(ymin = idsurvL80, ymax = idsurvU80, fill = as.factor(WY)), alpha = 0.25) +
  geom_ribbon(data = STARS_data  %>% filter(WY != current_year), aes(ymin = idsurvL80, ymax = idsurvU80, fill = as.factor(WY)), alpha = 0.08) +
  labs(x = 'Month', 
       y = 'Apparent survival probability', 
       subtitle = "Delta STARS Model -\nPredicted Natural Winter-run Chinook Daily Cohorts Passage, Knights Landing to Chipps Island", 
       title = "Interior Delta Route-specific Survival Probability: Median survival of daily cohorts using the Interior Delta route",
       caption = paste0("Data source: Delta STARS developed by USGS Quantitative Fisheries Ecology Section and deployed by SacPAS.\n", timestamp),
       color = "Water Year,\nmedian with 80% CI",
       linetype = "Water Year,\nmedian with 80% CI") +
  scale_x_continuous(breaks = seq(1, 365, by = 61), labels = wDay_to_month( seq(1, 365, by = 61))) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_color_manual(values = colors) +
  scale_linetype_manual(values = linetypes) +
  scale_fill_manual(values = colors) +
  scale_size_identity() +
  guides(fill = "none") +
  theme_minimal() +
  theme(legend.position = c(.9, 0.7),
        text = element_text(size = 15),
        panel.grid.major = element_line(linetype = "dotted"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
       
  )


print(p2)

# Interior Delta Route-specific Probability
p3 <- ggplot(STARS_data, aes(x = wDay, group = WY)) +
  geom_line(aes(y = idRoute, color = as.factor(WY), linetype = as.factor(WY), size = ifelse(WY == current_year, .75, 0.5))) +
  geom_ribbon(data = STARS_data %>% filter(WY == current_year), aes(ymin = idRouteL80, ymax = idRouteU80 ), alpha = 0.25) +
  geom_ribbon(data = STARS_data %>% filter(WY != current_year), aes(ymin = idRouteL80, ymax = idRouteU80, fill = as.factor(WY)), alpha = 0.08) +
  labs(x = 'Month', 
       y = 'Route probability', 
       subtitle = "Delta STARS Model -\nPredicted Natural Winter-run Chinook Daily Cohorts Passage, Knights Landing to Chipps Island", 
       title = "Interior Delta Route-specific Probability: Proportion of daily cohorts using the Interior Delta route",
       caption = paste0("Data source: Delta STARS developed by USGS Quantitative Fisheries Ecology Section and deployed by SacPAS.\n", timestamp),
       color = "Water Year,\nmedian with 80% CI",
       linetype = "Water Year,\nmedian with 80% CI") +
  scale_x_continuous(breaks = seq(1, 365, by = 61), labels = wDay_to_month( seq(1, 365, by = 61))) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_color_manual(values = colors) +
  scale_linetype_manual(values = linetypes) +
  scale_fill_manual(values = colors) +
  scale_size_identity() +
  guides(fill = "none") +
  theme_minimal() +
  theme(legend.position = c(.9, 0.3),
        text = element_text(size = 15),
        panel.grid.major = element_line(linetype = "dotted"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
        
  )

print(p3)


# if wanting to print as a single plot, follow grayed out code below using patchwork:
# # Combined plots
# p <- p1 / p2 / p3

# # Add title and caption
# p_stars_combined <- p +  
#   plot_annotation(title = "STARS Model - Predicted Winter Run Chinook Salmon daily cohorts passage from Knights Landing to Chipps Island",
#                   caption = "Data source: STARS Shiny App, Years of data: 2018 to 2024")
# 



