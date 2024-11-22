#'import STARS data and plot all survival/routing probabilities
#'@description This script imports up-to-date STARS data from the STARS ShinyApp through a single link
#'and plots all survival and routing probabilities for a specific water year. 

require(tidyverse)
require(usethis)
require(here)
require(xts)
require(zoo)

#load STARS data
# url of .Rdata file generated for STARS ShinyApp on SacPAS server (in-house)
url <- "https://www.cbr.washington.edu/sacramento/cohort/include_wrc/STARS.shinyinputs.Rdata"
# make url connection live
source <-url(url)
# load live url 
load(source)
#close url connection
close(source)

#Once the url is loaded, the data is stored in a xts object. 
#The data is then converted to a tibble and the column names are adjusted to match the STARS data featured the track-a-cohort static plot and interactive plot. 
#Select/view the `WR_xts` loaded into the environment to see all columns available and adjust tibble call below as needed.

# Subset the data and convert to tibble
df_stars_raw <- tibble::as_tibble(WR_xts[, c(
  "Survival Overall Est",                     
  "Survival Overall LCL 80",                  
  "Survival Overall UCL 80",                  
  "Survival Sacramento Est",                  
  "Survival Sacramento LCL 80",               
  "Survival Sacramento UCL 80",               
  "Survival Yolo Est",                        
  "Survival Yolo LCL 80",                     
  "Survival Yolo UCL 80",                     
  "Survival Sutter Est",                      
  "Survival Sutter LCL 80",                   
  "Survival Sutter UCL 80",                   
  "Survival Steamboat Est",                   
  "Survival Steamboat LCL 80",                
  "Survival Steamboat UCL 80",                
  "Survival Interior Delta Est",              
  "Survival Interior Delta LCL 80",           
  "Survival Interior Delta UCL 80",           
  "Routing Probability Sacramento Est",       
  "Routing Probability Sacramento LCL 80",    
  "Routing Probability Sacramento UCL 80",    
  "Routing Probability Yolo Est",             
  "Routing Probability Yolo LCL 80",          
  "Routing Probability Yolo UCL 80",          
  "Routing Probability Sutter Est",           
  "Routing Probability Sutter LCL 80",        
  "Routing Probability Sutter UCL 80",        
  "Routing Probability Steamboat Est",        
  "Routing Probability Steamboat LCL 80",     
  "Routing Probability Steamboat UCL 80",     
  "Routing Probability Interior Delta Est",   
  "Routing Probability Interior Delta LCL 80",
  "Routing Probability Interior Delta UCL 80"
)]) %>%
  # Add the first date as a new column
  mutate(date = zoo::index(WR_xts)) %>%
  # Make date the first column
  select(date, everything()) %>%
  # Pivot longer
  pivot_longer(
    cols = -date,
    names_to = c("metric", "route", "stat"),
    names_pattern = "(Survival|Routing Probability) (.*) (Est|LCL 80|UCL 80)",
    values_to = "value"
  ) %>%
  # Rename the stat column values
  mutate(stat = recode(stat, "Est" = "median", "LCL 80" = "lowerCI", "UCL 80" = "upperCI")) %>%
  # Pivot wider to have median, lowerCI, and upperCI as separate columns
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  # Convert date to WY, wDay
  arrange(date) %>% 
  mutate(WY = year(date) + (month(date) >= 10),
         wDay = if_else(month(date) >= 10, yday(date) - 273, yday(date) + 92),
         doy = yday(date),
         CY = year(date),
         wDate = if_else(month(date) >= 10, date + years(1), date)) 

#based on Nick Beer feedback, the model only forecasts through 7/31 and then returns 0 since no fish are running. Therefore, currently filtering dates beyond 7/31 in the CY to only show WY: 10-01 to 07-31
filtered_dates <- df_stars_raw %>%
  filter(!(year(wDate) == CY & (month(wDate) > 7 | (month(wDate) == 7 & day(wDate) > 31))))

# Select a specific year to plot -- currently set WY 2021, adjust as needed
year_selected <- 2021
year_specific_data <- filtered_dates %>% 
  filter(WY == year_selected)

  
# Plot the data

# Define colors for each route
colors <- c("Overall" = "black", "Yolo" = "lightblue", "Sutter" = "blue", "Steamboat" = "purple", "Sacramento" = "darkgrey", "Interior Delta" = "orange")
# Get the current timestamp
timestamp <- format(Sys.time(), "%d %b %Y %H:%M:%S %Z")


survival_plot <- year_specific_data %>% 
  dplyr::filter(metric == "Survival") %>% 
  group_by(route) %>% 
  ggplot(aes(x = date)) +
    geom_ribbon( aes(ymin = lowerCI, ymax = upperCI,  fill = route), alpha = .08) +
    geom_line(aes(y = median, color = route), size = 1) +
    labs(x = 'Month', 
         y = 'Apparent survival probability', 
         subtitle = "Delta STARS Model -\nPredicted Natural Winter-run Chinook Daily Cohorts Passage, Knights Landing to Chipps Island", 
         title = paste0("WY",year_selected, " Survival: Median survival of daily cohorts by route"),
         caption = paste0("Data source: Delta STARS developed by USGS Quantitative Fisheries Ecology Section and deployed by SacPAS.\n", timestamp),
         color = "Route survival,\nmedian with 80% CI",
         fill = "Route survival,\nmedian with 80% CI") +
    scale_y_continuous( expand = c(0,0)) +
    scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
    theme_minimal() +
    theme(
          text = element_text(size = 15),
          panel.grid.major = element_line(linetype = "dotted"),
          panel.border = element_rect(colour = "black", fill = NA, size = .5),
          axis.ticks = element_line(size = 0.5),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank()
        
  )

survival_plot


route_probability_plot <- year_specific_data %>% 
  dplyr::filter(metric == "Routing Probability") %>% 
  group_by(route) %>% 
  ggplot(aes(x = date)) +
  geom_ribbon( aes(ymin = lowerCI, ymax = upperCI,  fill = route), alpha = .08) +
  geom_line(aes(y = median, color = route), size = 1) +
  labs(x = 'Month', 
       y = 'Routing Probability', 
       subtitle = "Delta STARS Model -\nPredicted Natural Winter-run Chinook Daily Cohorts Passage, Knights Landing to Chipps Island", 
       title = paste0("WY",year_selected, " Route-specific probability: Proportion of daily cohorts using a specific route"),
       caption = paste0("Data source: Delta STARS developed by USGS Quantitative Fisheries Ecology Section and deployed by SacPAS.\n", timestamp),
       color = "Route probability,\nmedian with 80% CI",
       fill = "Route probability,\nmedian with 80% CI") +
  scale_y_continuous( expand = c(0,0)) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(
    text = element_text(size = 15),
    panel.grid.major = element_line(linetype = "dotted"),
    panel.border = element_rect(colour = "black", fill = NA, size = .5),
    axis.ticks = element_line(size = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
    
  )

route_probability_plot

