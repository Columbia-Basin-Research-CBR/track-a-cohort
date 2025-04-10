#' @title Winter-run Chinook Cumulative LAD Loss with Hydrologic Classification Index and BiOp Status
#' @description This script includes the wrangling and plotting of the cumulative LAD loss data for winter-run chinook salmon. 
#' It compares the cumulative LAD loss for the current water year compared to historical years, 
#' along with separating the plots by hydrological classification index and pre/post 2009 BiOp. The blue = wet, red = dry, black = current water year, and grey is past years. 
#' @details The data is sourced from `data` > `jpe_lad_loss_data.rda > lad_cumulative_loss_data` and is wrangled in `data-raw` > `import_winter_run_chinook_lad_loss_data.R`.
#' @return A static plot comparing the cumulative genetic loss of the JPE for the current water year compared to historical years.
require(tidyverse)
require(here)
require(scales)
require(gghighlight)
require(ggrepel)
require(ggh4x)


# import data file
load(here("data/jpe_lad_loss_data.rda"))
# extract genetic cumulative loss data
lad_cumulative_loss_data <- jpe_lad_loss_data$lad_cumulative_loss_data


# import wDay to month function
source(here("R/utils_fct_wday_to_month.R"))
    
    #set current year
    source(here("R/utils_fct_assign_current_water_year.R"))
           current_year <- assign_current_water_year()
           previous_year <- current_year - 1
           
           # Check if there is any data for the current year
           use_previous_year <- !any(lad_cumulative_loss_data$WY == current_year)
           if (use_previous_year) {
             plot_year <- previous_year
             caption_note <- paste0("No data reported for WY", current_year, ". Data reflects last available data (WY", previous_year, ").\n")
           } else {
             plot_year <- current_year
             caption_note <- ""
           }
   
    # Get the current timestamp
    timestamp <- format(Sys.time(), "%d %b %Y %H:%M:%S %Z")
    
    # extract maximum cumloss for the current year
    max_cumloss_current_year <- lad_cumulative_loss_data %>%
      filter(WY == plot_year) %>%
      group_by(WY) %>% 
      summarise(max_cumloss = max(cumloss)) %>%
      pull(max_cumloss)
    
    #appending current water year results for each facet regardless of year
    loss_current_year <- lad_cumulative_loss_data %>% filter(WY == plot_year) %>% select(wDay, cumloss)
    
    #extract maximum cumloss for each year (for labels)
    max_cumloss_per_year <-lad_cumulative_loss_data %>%
      filter(!is.na(hydro_type_grp)) %>% #currently removes WY2023 or year prior to current year since no hydro assigned
      group_by(WY) %>%
      filter(cumloss == max(cumloss))
    
    # filter data for the year 2001 (for year label and max value label)
    year2001_data <- lad_cumulative_loss_data %>% filter(WY == 2001, cumloss < 10000, !is.na(hydro_type_grp))
    maxvalue2001<- lad_cumulative_loss_data %>% 
      filter(WY == 2001) %>% 
      summarise(maxvalue = max(cumloss)) %>% 
      pull(maxvalue)

#plot  
  p <- lad_cumulative_loss_data %>% 
    filter(cumloss < 10000) %>%
    #currently removes WY2023 or year prior to current year since no hydro assigned - consider alternative
    filter(!is.na(hydro_type_grp)) %>% 
    ggplot() +
    geom_line( data = . %>% filter(WY == plot_year ), aes(x = wDay, y = cumloss, group = WY), color = "black") +
    geom_line( data = . %>% filter(WY != plot_year ), aes(x = wDay, y = cumloss, group = WY, color =  hydro_type_grp)) +
    geom_hline(yintercept = max_cumloss_current_year, linetype = "dashed", color = "black") +
    geom_text(aes(x = 250, y = max_cumloss_current_year), label = paste0("WY",plot_year," cumulative loss"), size = 2.5, vjust = -0.5, fontface = "plain" ) +
    gghighlight() +
    ggrepel::geom_text_repel(data = subset(max_cumloss_per_year, cumloss >= max_cumloss_current_year & WY !=2001), 
                             aes(x = wDay, y = cumloss, label = WY), 
                             size = 3,  nudge_y = 20, nudge_x = 2) +
    geom_text(data = year2001_data, 
              aes(x = 170, y = 9500, label = paste0("*2001\nmax value = ", round(maxvalue2001))), 
              size = 3, fontface = "plain") +
    labs(x = 'Date', 
         y = 'Cumulative LAD Loss', 
         title = 'Cumulative LAD Loss by BiOp Status and Hydrologic Classification Index',
         subtitle = paste0("Species: Natural Winter-run Chinook\nData Years: WY", min(lad_cumulative_loss_data$WY), " to WY", plot_year,
                           "\nCurrent Cumulative Loss: ", round(max_cumloss_current_year,2)),
         caption = paste0(caption_note, "LAD loss: CDFW Salvage Database; Hydrologic Classification Index: Water Supply Information, CDEC\n", timestamp)) +
    scale_x_continuous(breaks = seq(1, 365, by = 61), labels = wDay_to_month( seq(1, 365, by = 61))) + 
    scale_y_continuous(labels = scales::comma, breaks = seq(0, 10000, by = 2000), limits = c(0, 10000), expand = c(0, 0)) +
    scale_color_manual(values = c("#D95F02", "#00BFFF")) +
    ggh4x::facet_nested(hydro_type_grp ~ status ) + 
    geom_line(data = loss_current_year, aes(x = wDay, y = cumloss), color = "black", size = 1) +
    theme_minimal() +
    theme(
      # axis.line = element_line(color = "grey"),
      panel.grid.major = element_line(linetype = "dotted"),

      panel.border = element_rect(color = "grey", fill = NA),
      panel.spacing = unit(.5, "cm"),
      axis.ticks = element_line(size = 0.5),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      text = element_text(size = 15))


print(p)
