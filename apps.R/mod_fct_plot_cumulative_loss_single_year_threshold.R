wrangle_plot_data<- function(data, selected_species){

  
  # import wDay to month function
  source(here("utils_fct_wday_to_month.R"))

  #set current year
  source(here("utils_fct_assign_current_water_year.R"))
  current_year <- assign_current_water_year()
  
  # #use if running from local computer
  # # import wDay to month function
  # source(here("apps.R/utils_fct_wday_to_month.R"))
  # 
  # #set current year
  # source(here("apps.R/utils_fct_assign_current_water_year.R"))
  # current_year <- assign_current_water_year()
  
  
  #adjust water year date back to CY
  #current date
  date <- today()
  #convert current date to water day
  wDay_today <- if_else(month(date) >= 10, yday(date) - 273, yday(date) + 92)
  
  # Function to convert wDay to actual date
  wDay_to_date <- function(wDay, WY) {
    start_date <- ymd(paste0(WY - 1, "-10-01")) # Water year starts on Oct 1 of the previous calendar year
    return(start_date + days(wDay - 1))
  }
  #convert back to CY date
  data$date <- as_date(mapply(wDay_to_date, data$wDay, data$WY))
  

  # Generate month labels and tick positions
  # Assuming wDay ranges from 1 to 365, adjust as necessary for your data
  tick_positions <- seq(1, 365, by = 61)  # Adjust the sequence as needed
  month_labels <- sapply(tick_positions, wDay_to_month)
  
  # update threshold based on species selected 
  
  if (selected_species == "Winter-run Chinook"){
    

    # extract maximum cumloss for the current year
    cumloss_current_year <- data %>%
      filter(WY == current_year) 
    

  
  # extract current water year JPE and set single year threshold @ 2% of JPE -- return single value
  jpe_current_year_2pct <- data %>%
    filter(WY == current_year, cumloss == max(cumloss)) %>%
    pull(jpe)*.02
  
  #extract current water year JPE and set single year threshold per PA 4-69 @ 1.17% of JPE -- return single value
  jpe_current_year_1.17pct <- data %>%
    filter(WY == current_year, cumloss == max(cumloss)) %>%
    pull(jpe)*.0117
  
  
  max_loss_threshold_2010_to_2018 <- data %>% 
    filter(between(WY, 2010, 2018)) %>%
    ungroup() %>% 
    summarise(max_loss = max(cumloss)) %>%
    pull(max_loss)
  
  # plot
  
  p <- plot_ly(cumloss_current_year, x = ~date, y = ~cumloss, type = 'scatter', mode = 'lines+markers', line = list(color = "black"), name = "Cumulative Loss") %>%
    add_segments(x = ~min(date), xend = ~max(date), y = ~jpe_current_year_2pct, yend = ~jpe_current_year_2pct, line = list(dash = 'dash', color = '#702963'), name = "2% JPE") %>%
    add_annotations(x = ~min(date), y = jpe_current_year_2pct, text = paste0("Single-Year Threshold (2% of JPE): ", jpe_current_year_2pct), showarrow = F, xanchor = 'left', yanchor = 'bottom', font = list(color = '#702963')) %>%
    add_segments(x = ~min(date), xend = ~max(date), y = ~jpe_current_year_1.17pct, yend = ~jpe_current_year_1.17pct, line = list(dash = 'dash', color = '#8B0000'), name = "100% SYT") %>%
    add_annotations(x = ~min(date), y = jpe_current_year_1.17pct, text = paste0("100% Single-Year Threshold (1.17% of JPE): ", round(jpe_current_year_1.17pct,2)), showarrow = F, xanchor = 'left', yanchor = 'bottom', font = list(color = '#8B0000')) %>%
    add_segments(x = ~min(date), xend = ~max(date), y = ~jpe_current_year_1.17pct * 0.75, yend = ~jpe_current_year_1.17pct * 0.75, line = list(dash = 'dash', color = '#D27D2D'), name = "75% SYT") %>%
    add_annotations(x = ~min(date), y = jpe_current_year_1.17pct * 0.75, text = paste0("75% Single-Year Threshold (1.17% of JPE): ", round(jpe_current_year_1.17pct * 0.75,2)), showarrow = F, xanchor = 'left', yanchor = 'bottom', font = list(color = '#D27D2D')) %>%
    add_segments(x = ~min(date), xend = ~max(date), y = ~jpe_current_year_1.17pct * 0.5, yend = ~jpe_current_year_1.17pct * 0.5, line = list(dash = 'dash', color = '#daa520'), name = "50% SYT") %>%
    add_annotations(x = ~min(date), y = jpe_current_year_1.17pct * 0.5, text = paste0("50% Single-Year Threshold (1.17% of JPE): ", round(jpe_current_year_1.17pct * 0.5,2)), showarrow = F, xanchor = 'left', yanchor = 'bottom', font = list(color = '#daa520')) %>%
    layout(title = 'Cumulative Loss for Current Water Year with Single-Year Thresholds',
           xaxis = list(title = 'Date'),
           yaxis = list(title = 'Cumulative Loss'),
           legend = list(orientation = "h", y = -0.1))
  
  p
  
  } else if(selected_species == "Steelhead") {
    
    cumloss_current_year <- data 
    
    # Calculate the first date minus 2 weeks to give space for labels - adjust as needed
    set_text_date <- min(cumloss_current_year$date) - weeks(2)
    
    cumloss_current_year_mgt1 <- cumloss_current_year %>% 
      filter(management_period == "12/1 - 3/31")
    cumloss_current_year_mgt2 <- cumloss_current_year %>% 
      filter(management_period == "4/1 - 6/15")
    
    # Update to set 100% loss limit when known
    current_year_100pct <- 1414 + 1552 # setting 100% loss limit for steelhead at Dec-June loss reported on SacPAS
    current_year_75pct <- current_year_100pct*.75
    current_year_50pct <- current_year_100pct*.50
    
    # set management threshold line
    management_period <-ymd(paste0(current_year,"-03-31"))
    
    
    p <- plot_ly() %>%
      add_lines(data = cumloss_current_year_mgt1, x = ~date, y = ~cum_loss_mgt, name = "12/1 - 3/31", type = 'scatter', mode = 'lines+markers') %>%
      add_lines(data = cumloss_current_year_mgt2, x = ~date, y = ~cum_loss_mgt, name = "4/1 - 6/15", type = 'scatter', mode = 'lines+markers') %>%
      add_segments(x = ~set_text_date, xend = ~max(date), y = ~current_year_100pct, yend = ~current_year_100pct, line = list(dash = 'dash', color = '#8B0000'), name = "100% SYT") %>%
      add_annotations(x = ~set_text_date, y = current_year_100pct, text = paste0("100% Single-Year Threshold: ", current_year_100pct), showarrow = F, xanchor = 'left', yanchor = 'bottom', font = list(color = '#8B0000')) %>%
      add_segments(x = ~set_text_date, xend = ~max(date), y = ~current_year_75pct, yend = ~current_year_75pct, line = list(dash = 'dash', color = '#D27D2D'), name = "75% SYT") %>%
      add_annotations(x = ~set_text_date, y = current_year_75pct, text = paste0("75% Single-Year Threshold: ", round(current_year_75pct,2)), showarrow = F, xanchor = 'left', yanchor = 'bottom', font = list(color = '#D27D2D')) %>%
      add_segments(x = ~set_text_date, xend = ~max(date), y = ~current_year_50pct , yend = ~current_year_50pct, line = list(dash = 'dash', color = '#daa520'), name = "50% SYT") %>%
      add_annotations(x = ~set_text_date, y = current_year_50pct, text = paste0("50% Single-Year Threshold: ", round(current_year_50pct,2)), showarrow = F, xanchor = 'left', yanchor = 'bottom', font = list(color = '#daa520')) %>%
      add_segments(x = management_period, xend = management_period, y = -Inf, yend = Inf, line = list(dash = 'dash', color = 'grey'), name = "Management Period") %>%
      layout(title = 'Cumulative Loss for Current Water Year with Single-Year Thresholds',
             xaxis = list(title = 'Date'),
             yaxis = list(title = 'Cumulative Loss'),
             legend = list(orientation = "h", y = -0.1))
    
    p


  }
  

  return(p)
  
}

