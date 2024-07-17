#' Figure 2: Survival and Routing Probabilities of Winter-run Chinook Salmon in the Sacramento River
#' @description static ggplot of STARS model survival and routing probabilities for winter-run Chinook salmon in the Sacramento River-- also used in Steelhead figures as a surrogate
#' @details To access the data file, navigate to the `data` directory located at the
#' project's root. The file is named `STARS_data.rda` and is sourced from `data-raw >import_STARS_data.R` which includes `data-raw >STARS.shinyinputs.Rdata`. 
#' @return ggplot barplot of annual JPE estimate with highlighted methods used for annual estimate
#' @seealso \code{\link{wDay_to_month}} for converting water day to month
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

#plot STARS data
    # Overall Survival
    p1 <- ggplot(STARS_data, aes(x = wDay, group = WY)) +
      geom_line(aes(y = surv)) +
      labs(x = 'Month\n(Water Year: Oct-Dec of year [t-1], Jan-Sep of year [t])', 
           y = 'Apparent survival probability', 
           title = 'Overall Survival', 
           subtitle = "Median survival of daily cohorts for all routes combined.") +
      scale_x_continuous(breaks = seq(1, 365, by = 61), labels = wDay_to_month( seq(1, 365, by = 61))) + 
      gghighlight(use_direct_label = FALSE) +
      facet_grid(WY~.) +
      geom_ribbon(aes(ymin = survL80, ymax = survU80), alpha = 0.5) +
      theme_minimal() +
      theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = .25),
            panel.grid.minor = element_blank(), 
            panel.grid.major.y = element_blank(),
            text = element_text(size = 15))
    
    # Interior Delta Route-specific Survival Probability
    p2 <- ggplot(STARS_data, aes(x = wDay, group = WY)) +
      geom_line(aes(y = idsurv)) +
      labs(x = 'Month\n(Water Year: Oct-Dec of year [t-1], Jan-Sep of year [t])', 
           y = 'Apparent survival probability', 
           title = 'Interior Delta Route-specific Survival Probability', 
           subtitle = "Median route-specific survival of daily cohorts using the Interior Delta route") +
      scale_x_continuous(breaks = seq(1, 365, by = 61), labels = wDay_to_month( seq(1, 365, by = 61))) + 
      gghighlight(use_direct_label = FALSE) +
      facet_grid(WY~.) +
      geom_ribbon(aes(ymin = idsurvL80, ymax = idsurvU80), alpha = 0.5) +
      theme_minimal() +
      theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = .25),
            panel.grid.minor = element_blank(), 
            panel.grid.major.y = element_blank(),
            text = element_text(size = 15))
    
    
    # Interior Delta Route-specific Probability
    p3 <- ggplot(STARS_data, aes(x = wDay, group = WY)) +
      geom_line(aes(y = idsurv)) +
      labs(x = 'Month\n(Water Year: Oct-Dec of year [t-1], Jan-Sep of year [t])', 
           y = 'Route probability', 
           title = 'Interior Delta Route-specific Probability', 
           subtitle = "Proportion of daily cohorts using the Interior Delta route") +
      scale_x_continuous(breaks = seq(1, 365, by = 61), labels = wDay_to_month( seq(1, 365, by = 61))) + 
      gghighlight(use_direct_label = FALSE) +
      facet_grid(WY~.) +
      geom_ribbon(aes(ymin = idsurvL80, ymax = idsurvU80), alpha = 0.5) +
      theme_minimal() +
      theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = .25),
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
