library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinydashboard)
library(lubridate)
library(here)


source(here::here("utils_SacPAStheme.R"))
# Load the data
load(here::here("steelhead_loss_export_data.rda"))
# adding horizontal lines -- provided by BOR. Confirm how these are designated
omrValues <- data.frame(value = c(-5000,-3500,-2000,-5000,-3500,-2500,-500,-1500,-2500, "COA 8.17"),#,'COA 8.17'
                        date = lubridate::ymd(c('2024-01-01', '2024-01-14', '2024-01-23', '2024-02-04', '2024-02-08',
                                         '2024-02-17', '2024-03-11', '2024-02-26', '2024-04-01', '2024-04-09')))


ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Daily Loss and Exports"),
  shinydashboard::dashboardSidebar(disable = TRUE),
  shinydashboard::dashboardBody(
    #add CSS SacPAS global theme
    fresh::use_theme(SacPAStheme),
    
              fluidRow(
                shinydashboard::box(
                  title = "Pumping Facilities:", 
                  status = "info", 
                  width = 3,
                  solidHeader = TRUE,
                  selectInput(inputId = "select_plot", 
                              label = NULL,
                              choices = c("Combined, CVP & SWP", unique(steelhead_loss_export_data$facility), "Compare All")),
                  checkboxInput(inputId = "showLines", 
                                label = "Include approximate changes in OMR values by date", 
                                value = TRUE) 
                ),
                shinydashboard::box(
                  title = "Steelhead Daily Total Loss and Exports Per Pumping Facility", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 9,
                  plotlyOutput(outputId = "interactivePlot"),
                  uiOutput(outputId = "comparePlots")
                )
              )
    )
  )


server <- function(input, output) {
  # Helper function to create plots
  createPlot <- function(data_to_plot) {
    # Create initial plot with bars for CVP and SWP daily total loss, specifying names for legend
    p <- plot_ly(data = data_to_plot, x = ~date) %>%
      add_bars(data = subset(data_to_plot, facility == "CVP"), y = ~daily_total_loss, name = "CVP Daily Loss", marker = list(color = '#E1BE6A')) %>%
      add_bars(data = subset(data_to_plot, facility == "SWP"), y = ~daily_total_loss, name = "SWP Daily Loss", marker = list(color = '#40B0A6')) %>%
      layout(
        barmode = "group",
        yaxis = list(title = "Daily Loss"),
        xaxis = list(title = "Date", type = "date"),
        legend = list(title = list(text = "Facility"), x = 1.1),
        margin = list(l = 60, r = 200, t = 50, b = 50)
      )
    
    # Add line plot for CVP and SWP pumping discharge on secondary y-axis, specifying names for legend
    p <- p %>%
      add_lines(data = subset(data_to_plot, facility == "CVP"), y = ~pumping_discharge_cfs, name = "CVP Export", line = list(color = '#E1BE6A'), yaxis = "y2") %>%
      add_lines(data = subset(data_to_plot, facility == "SWP"), y = ~pumping_discharge_cfs, name = "SWP Export", line = list(color = '#40B0A6'), yaxis = "y2") %>%
      layout(
        yaxis2 = list(overlaying = "y", side = "right", title = "Pumping Discharge (cfs)")
      )
    
    
    
    # Conditionally add vertical lines and text if input$showLines is TRUE
    if (input$showLines) {
      # Ensure omrValues$date is in Date format and convert to character in ISO 8601 format
      omrValues$date_iso <- format(omrValues$date, "%Y-%m-%d")
      
      # Explicitly set the x-axis type to 'date' in the layout
      p <- p %>%
        layout(
          xaxis = list(
            title = "",
            type = "date"  # Explicitly set x-axis type to 'date'
          ),
          yaxis2 = list(overlaying = "y", side = "right", title = "Pumping Discharge (cfs)")
        )
      for (val in omrValues$date_iso) {
        # Find the corresponding value for the current date
        corresponding_value <- omrValues$value[omrValues$date == val]
        
        p <- p %>%
          add_segments(x = val, xend = val, y = 0, yend = max(data_to_plot$daily_total_loss, na.rm = TRUE), yaxis = "y", line = list(color = "grey", width = 1), showlegend = FALSE ) %>%
          add_annotations(x = val, y = max(data_to_plot$daily_total_loss, na.rm = TRUE), text = paste("OMR:", corresponding_value), showarrow = FALSE, yanchor = "bottom", yaxis = "y", textangle = 90)
      }
    }
    
    return(p)
  }
  
  # Render the appropriate plot based on the selection
  output$interactivePlot <- renderPlotly({
    # Do not render this plot if "Compare All" is selected
    req(input$select_plot != "Compare All")
    
    if (input$select_plot == "Combined, CVP & SWP") {
      data_to_plot <- steelhead_loss_export_data
    } else {
      data_to_plot <- filter(steelhead_loss_export_data, facility == input$select_plot)
    }
    
    createPlot(data_to_plot)
  })
  
  # Render additional plots for "Compare All" option
  output$comparePlots <- renderUI({
    if (input$select_plot == "Compare All") {
      plot_output_list <- list(
        h3("Combined, CVP & SWP"), # Descriptive text for the combined plot
        plotlyOutput(outputId = "plot_combined"),
        h3("CVP"), # Descriptive text for the CVP plot
        plotlyOutput(outputId = "plot_cvp"),
        h3("SWP"), # Descriptive text for the SWP plot
        plotlyOutput(outputId = "plot_swp")
      )
      
      do.call(tagList, plot_output_list)
    }
  })
  
  # Define rendering for each plot when "Compare All" is selected
  observe({
    if (input$select_plot == "Compare All") {

      # Combined, CVP & SWP plot
      output$plot_combined <- renderPlotly({
        data_to_plot <- steelhead_loss_export_data
        createPlot(data_to_plot)
      })
      
      # CVP plot
      output$plot_cvp <- renderPlotly({
        data_to_plot <- filter(steelhead_loss_export_data, facility == "CVP")
        createPlot(data_to_plot)
      })
      
      # SWP plot
      output$plot_swp <- renderPlotly({
        data_to_plot <- filter(steelhead_loss_export_data, facility == "SWP")
        createPlot(data_to_plot)
      })
    }
  })
}

shinyApp(ui = ui, server = server)