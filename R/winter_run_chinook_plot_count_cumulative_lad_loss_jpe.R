#' @title Figure 4b: Winter-run Chinook JPE Cumulative LAD Loss
#' @description This script includes the wrangling and plotting of the cumulative LAD loss data for winter-run chinook salmon. 
#' It compares the cumulative LAD loss of the JPE for the current water year compared to historical years, 
#' along with separating the plots by hydrological classification index and pre/post 2009 BiOp. The blue = wet, red = dry, black = current water year, and grey is past years. 
#' @details The data is sourced from `data` > `jpe_lad_loss_data.rda > lad_cumulative_loss_data` and is wrangled in `data-raw` > `import_winter_run_chinook_lad_loss_data.R`.
#' @return A static plot comparing the cumulative genetic loss of the JPE for the current water year compared to historical years.
#' @import ggplot2
#' @import dplyr
#' @import here
#' @import scales
#' @import gghighlight
#' @import ggrepel
#' @importFrom ggh4x facet_nested
#' @import lubridate
#' @importFrom magrittr %>%
#' @noRd

# import data file
#loss data from sacpas
load(here("data/jpe_lad_loss_data.rda"))
# extract genetic cumulative loss data
lad_cumulative_loss_data <- jpe_lad_loss_data$lad_cumulative_loss_data

#water year provided by BOR -- to be updated
wytype <- read.csv(here::here('data/WYtype.csv')) %>% filter(Basin == "SacramentoValley")

# import wDay to month function
source(here("R/utils_fct_wday_to_month.R"))


# wrangle base data for plot

    lad_cumulative_loss_data <- lad_cumulative_loss_data %>% 
      #designate pre/post 2009 BiOp
      mutate(status = case_when(WY < 2009 ~ 'Pre-2009 BiOp\n(1994 to 2008)',
                                WY > 2008 ~ '2009 & 2019 BiOp\n(2009 to present)')) %>%
      #set order of factor levels
      mutate(status = factor(status, levels = c('Pre-2009 BiOp\n(1994 to 2008)', '2009 & 2019 BiOp\n(2009 to present)'))) %>% 
      #join with wytype to designate wet/dry year (Hydrologic Classification Index?)
      left_join(select(wytype,WY, "hydro_type" = `Yr.type`) , by = 'WY') %>%
      #remove date na's
      # assign hydro_type grouping
      mutate( hydro_type = factor(hydro_type, levels = c("W", "AN", "BN", "D", "C"), labels = c("Wet", "Above Normal", "Below Normal", "Dry", "Critical")),
              hydro_type_grp = case_when(
                hydro_type %in% c("Wet", "Above Normal") ~ "Wet, Above Normal",
                hydro_type %in% c("Below Normal", "Dry", "Critical") ~ "Below Normal, Dry, & Critical" )
      )
    
    #set current year
    source(here("R/utils_fct_assign_current_water_year.R"))
           current_year <- assign_current_water_year()
    
    # extract maximum cumloss for the current year
    max_cumloss_current_year <- lad_cumulative_loss_data %>%
      filter(WY == current_year) %>%
      group_by(WY) %>% 
      summarise(max_cumloss = max(cumloss)) %>%
      pull(max_cumloss)
    
    #appending current water year results for each facet regardless of year
    loss_current_year <- lad_cumulative_loss_data %>% filter(WY == current_year) %>% select(wDay, cumloss)
    
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
    geom_line( data = . %>% filter(WY == current_year ), aes(x = wDay, y = cumloss, group = WY), color = "black") +
    geom_line( data = . %>% filter(WY != current_year ), aes(x = wDay, y = cumloss, group = WY, color =  hydro_type_grp)) +
    geom_hline(yintercept = max_cumloss_current_year, linetype = "dashed", color = "black") +
    geom_text(aes(x = 250, y = max_cumloss_current_year), label = "WY2024 cumulative loss", size = 2.5, vjust = -0.5, fontface = "plain" ) +
    gghighlight() +
    ggrepel::geom_text_repel(data = subset(max_cumloss_per_year, cumloss >= max_cumloss_current_year & WY !=2001), 
                             aes(x = wDay, y = cumloss, label = WY), 
                             size = 3,  nudge_y = 20, nudge_x = 2) +
    geom_text(data = year2001_data, 
              aes(x = 170, y = 9500, label = paste0("*2001\nmax value = ", round(maxvalue2001))), 
              size = 3, fontface = "plain") +
    labs(x = 'Date \n(Water Year: Oct-Dec of year [t-1], Jan-Sep of year [t])', 
         y = 'Cumulative Loss', 
         title = 'Winter-run Chinook salmon -  Current and Historical Cumulative Loss',
         subtitle = paste0(" Data includes: WY", min(lad_cumulative_loss_data$WY), " to WY", current_year)) + 
    scale_x_continuous(breaks = seq(1, 365, by = 61), labels = wDay_to_month( seq(1, 365, by = 61))) + 
    scale_y_continuous(labels = scales::comma, breaks = seq(0, 10000, by = 2000), limits = c(0, 10000), expand = c(0, 0)) +
    scale_color_manual(values = c("sienna4", "steelblue4")) +
    ggh4x::facet_nested(hydro_type_grp ~ status ) + 
    geom_line(data = loss_current_year, aes(x = wDay, y = cumloss), color = "black") +
    theme_minimal() +
    theme(
      axis.line = element_line(color = "grey"),
      panel.grid.major.x = element_blank(), 
      panel.border = element_rect(color = "grey", fill = NA),
      panel.spacing = unit(1, "cm"),
      axis.ticks.x = element_line(color = "black"), 
      text = element_text(size = 15))


print(p)
