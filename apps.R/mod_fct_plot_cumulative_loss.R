wrangle_plot_data<- function(data, selected_loss, selected_species){
  # import wDay to month function
  source(here("R/utils_fct_wday_to_month.R"))
  
  #set current year
  source(here("R/utils_fct_assign_current_water_year.R"))
  current_year <- assign_current_water_year()
  
  # if(selected_species == "Steelhead"){
  #   selected_loss <- "FALSE" #this way it will plot cumloss(count) for Steelhead only
  # }
  
  max_current_year <- if(selected_loss == "TRUE"){
    data %>%
      filter(WY == current_year) %>%
      group_by(WY) %>% 
      summarise(max_cumloss = max(pct_cumloss)) %>%
      pull(max_cumloss)
  } else {
    data %>%
      filter(WY == current_year) %>%
      group_by(WY) %>% 
      summarise(max_cumloss = max(cumloss)) %>%
      pull(max_cumloss)
  }
  
  
  # Plot initialization with layout settings
  # Use switch to select the y-axis data and set formatting based on user selection
  list_y_data <- switch(selected_loss,
                        "TRUE" = list(data = data$pct_cumloss, title = "Cumulative Loss (%)", format = ".1%"),
                        "FALSE" = list(data = data$cumloss, title = "Cumulative Loss (Count)", format = "")
  )
  
  
  p <- plot_ly() %>%
    layout(title = paste("Daily",list_y_data$title, "from Water Years", min(data$WY), "to" , max(data$WY)),
           subtitle = "Species: WY1994 to WY2024",
           xaxis = list(title = "Day of Year"),
           yaxis = list(title = list_y_data$title, tickformat = list_y_data$format)) %>% 
    add_lines(x = c(1, 365), y = c(max_current_year, max_current_year), 
              line = list(color = "black", dash = "dash"), showlegend = FALSE) %>% 
    add_annotations(x = 350, y = max_current_year, 
                    text = paste0("Max Cumulative Loss WY", current_year), 
                    showarrow = F, xanchor = 'right', yanchor = 'bottom', 
                    font = list(size = 12))
  
  # Iterate over each unique year to add traces
  unique_years <- unique(data$WY)
  for(year in unique_years) {
    # Filter data for the current iteration year
    year_data <- data %>% filter(WY == year)

    # Dynamically select the y-axis data based on loss_selected
    y_data <- if(selected_loss == "TRUE") {
      year_data$pct_cumloss
    } else {
      year_data$cumloss
    }

    # Set color based on whether it's the current year or not
    color <- ifelse(year == current_year, "black", "grey")

    # Add trace for the current year
    p <-  add_trace(p, data = year_data, x = ~wDay, y = y_data, type = 'scatter', mode = 'lines',
                    line = list(color = color), name = as.character(year),
                    hoverinfo = 'text', text = paste("Water Year:", year_data$WY, "<br>Day:", year_data$wDay, "<br>Cumulative Loss:", round(y_data,2)))

  }
  
  # Display the plot
  p

  
}

