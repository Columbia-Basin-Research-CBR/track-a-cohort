wrangle_plot_data<- function(data, selected_loss, selected_hydro = "TRUE", selected_biop = "TRUE"){

  
  # import wDay to month function
  source(here("R/utils_fct_wday_to_month.R"))
  
  #set current year
  source(here("R/utils_fct_assign_current_water_year.R"))
  current_year <- assign_current_water_year()
  
  #set grouping variables as factors, renaming NA values to "Unassigned"
  data <- data %>%
    mutate(hydro_type_grp = ifelse(is.na(hydro_type_grp), "Unassigned", as.character(hydro_type_grp)),
           hydro_type_grp = factor(hydro_type_grp, levels = c("Wet, Above Normal", "Below Normal, Dry, & Critical", "Unassigned")),
           status = factor(status, levels = c('Pre-2009 BiOp\n(1994 to 2008)', '2009 & 2019 BiOp\n(2009 to present)')))
  

  # Generate month labels and tick positions
  # Assuming wDay ranges from 1 to 365, adjust as necessary for your data
  tick_positions <- seq(1, 365, by = 61)  # Adjust the sequence as needed
  month_labels <- sapply(tick_positions, wDay_to_month)
  
  # Select the y-axis data based on user selection
  y_col <- ifelse(selected_loss == "TRUE", "pct_cumloss", "cumloss")
  
  
  # extract max cumulative loss for the current year-- pct or count
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
  

  # Use switch to select the y-axis data and set formatting based on user selection
  selected_loss <- as.character(selected_loss)
  list_y_data <- switch(selected_loss,
                        "TRUE" = list(data = data$pct_cumloss, title = "Cumulative Loss (%)", format = ".1%"),
                        "FALSE" = list(data = data$cumloss, title = "Cumulative Loss (Count)", format = "")
  )

  
  # Plot 
  #base plot
  p <- plot_ly() %>%
    layout(title = paste("Daily",list_y_data$title, "from Water Years", min(data$WY), "to" , max(data$WY)),
           subtitle = "Species: WY1994 to WY2024",
           xaxis = list(title = "Month (Water Year)", tickvals = tick_positions, ticktext = month_labels),
           yaxis = list(title = list_y_data$title, tickformat = list_y_data$format)) %>% 
    add_lines(x = c(1, max(data$wDay)+5), y = c(max_current_year, max_current_year), 
              line = list(color = "black", dash = "dash"), showlegend = FALSE) %>% 
    add_annotations(x = (max(data$wDay)+5), y = max_current_year, 
                    text = paste0("Max Cumulative Loss WY", current_year), 
                    showarrow = F, xanchor = 'right', yanchor = 'bottom', 
                    font = list(size = 12))
  
  
  # modified plot

    # Check conditions and adapt the plot accordingly
  if(selected_hydro == "TRUE" && selected_biop == "FALSE") {
      # Color lines by hydro_type_grp
      color <-  c("Wet, Above Normal"  = "#c3d7e2", "Below Normal, Dry, & Critical" = "#bcb18b", "Unassigned" = "#000000")
      
      p <- p %>% add_trace(data = data, x = ~wDay, y = as.formula(paste0("~", y_col)), type = 'scatter', mode = 'lines',
                           color = ~hydro_type_grp, colors = color,
                           line = list(dash = "solid"), name = ~as.character(WY))
      
      print(color)
  } else if(selected_hydro == "FALSE" && selected_biop == "TRUE") { 
      
      # Set color based on whether it's the current year or not
      color <- ifelse(data$WY == current_year, "black", "grey")
      
    # Subset data based on 'status'
    data_pre_2009 <- data %>% filter(status == 'Pre-2009 BiOp\n(1994 to 2008)')
    data_post_2009 <- data %>% filter(status == '2009 & 2019 BiOp\n(2009 to present)')
    
    # Define a function to create a plot for a given subset of data
    create_plot <- function(subset_data, title_suffix) {
      plot_ly(subset_data, x = ~wDay, y = as.formula(paste0("~", y_col)), type = 'scatter', mode = 'lines',
              color = ~hydro_type_grp, colors = color,
              line = list(dash = "solid"), name = ~as.character(WY)) %>%
        layout(title = title_suffix,
               xaxis = list(title = "Month (Water Year)", tickvals = tick_positions, ticktext = month_labels),
               yaxis = list(title = list_y_data$title, tickformat = list_y_data$format)) #%>%
      #currently causing error due to uneven columns of xy and namecolor - troubleshoot later
      # add_lines(x = range(subset_data$wDay), y = rep(max_current_year, 2), #c(max_current_year, max_current_year),
      #           line = list(color = "black", dash = "dash"), showlegend = FALSE)
    }
    
    # Create plots for each subset
    plot_pre_2009 <- create_plot(data_pre_2009, "Pre-2009 BiOp (1994 to 2008)")
    plot_post_2009 <- create_plot(data_post_2009, "2009 & 2019 BiOp (2009 to present)")
    
    # Combine plots using subplot, ensuring both titles are kept
    p <- subplot(plot_pre_2009, plot_post_2009, nrows = 2, shareX = TRUE, shareY = FALSE, titleX = TRUE, titleY = TRUE)
    
    # Print the combined plot
    p
      
  } else if(selected_hydro == "TRUE" && selected_biop == "TRUE") {
      # Color lines by hydro_type_grp 
    # Future update: look into assigning 2023 as grey and 2024 as black
      color <-  c("Wet, Above Normal"  = "#c3d7e2", "Below Normal, Dry, & Critical" = "#bcb18b", "Unassigned" = "grey")
      
      # Subset data based on 'status'
      data_pre_2009 <- data %>% filter(status == 'Pre-2009 BiOp\n(1994 to 2008)' & WY != current_year)
      data_post_2009 <- data %>% filter(status == '2009 & 2019 BiOp\n(2009 to present)' & WY != current_year)
      data_current_year <- data %>% filter(WY == current_year)
      
      # Define a function to create a plot for a given subset of data
      create_plot <- function(subset_data, title_suffix) {
        plot_ly(subset_data, x = ~wDay, y = as.formula(paste0("~", y_col)), type = 'scatter', mode = 'lines',
                color = ~hydro_type_grp, colors = color,
                line = list(dash = "solid"), name = ~as.character(WY)) %>%
          layout(
                 xaxis = list(title = "Month (Water Year)", tickvals = tick_positions, ticktext = month_labels),
                 yaxis = list(title = list_y_data$title, tickformat = list_y_data$format), 
                 margin = .2) %>% 
          add_lines(data_current_year, x = ~wDay, y = as.formula(paste0("~", y_col)), type = 'scatter', mode = 'lines',
                    line = list(dash = "solid", color = "black"), name = ~as.character(WY))
        #currently causing error due to uneven columns of xy and namecolor - troubleshoot later
          # add_lines(x = range(subset_data$wDay), y = rep(max_current_year, 2), #c(max_current_year, max_current_year),
          #           line = list(color = "black", dash = "dash"), showlegend = FALSE)
      }
      
      # Create plots for each subset
      plot_pre_2009 <- create_plot(data_pre_2009, "Pre-2009 BiOp (1994 to 2008)")
      plot_post_2009 <- create_plot(data_post_2009, "2009 & 2019 BiOp (2009 to present)") #title_suffix still being dropped- remove if not using
      
      # Combine plots using subplot, ensuring both titles are kept
      p <- subplot(plot_pre_2009, plot_post_2009, nrows = 2, shareX = TRUE, shareY = FALSE, titleX = TRUE, titleY = TRUE) 
      
      # Manually add annotations for each subplot title
      biop_designations <- c("Pre-2009 BiOp (1994 to 2008)", "2009 & 2019 BiOp (2009 to present)")
      annotations <- list()
      for (i in 1:length(biop_designations)) {
        annotations[[i]] <- list(
          x = 0.5, # Adjust based on subplot position
          y = 1 - (i-1) / length(biop_designations), # Adjust based on subplot position
          text = paste("BiOp Designation:", biop_designations[i]),
          showarrow = FALSE,
          xref = 'paper',
          yref = 'paper',
          xanchor = 'center',
          yanchor = 'bottom',
          font = list(size = 12)
        )
      }
      
      p <- p %>% layout(annotations = annotations, 
                        margin = list(l = 50, r = 50, t = 50, b = 50, pad = 4))
      
      
      # Print the combined plot
      p
      print(head(data_current_year))
  } else if(selected_hydro == "FALSE" && selected_biop == "FALSE") {
    
    # Set color based on whether it's the current year or not
    unique_years <- unique(data$WY)
    data <- data %>%
      mutate(line_color = ifelse(WY == current_year, "black", "grey"))
    
    # Iterate over unique years to add each as a separate trace
    for(year in unique_years) {
      year_data <- data[data$WY == year,]
      p <- add_trace(p, data = year_data, x = ~wDay, y = as.formula(paste0("~", y_col)), type = 'scatter', mode = 'lines',
                     line = list(color = unique(year_data$line_color)), name = as.character(year),
                     hoverinfo = 'text', text = paste0("Water Year: ", year_data$WY, "<br>Day: ", year_data$wDay, "<br>Cumulative Loss: ", if(selected_loss == "TRUE"){paste(round(year_data[[y_col]]*100,3), "%")} else{paste(round(year_data[[y_col]],3), "")}))
    }
    }
  

  
  # Display the plot
  p

  
}

