#' @title Daily Loss and Exports Shiny App
#' @description This Shiny app allows users to visualize daily total loss for Natural Winter-run Chinook and Steelhead and daily exports at CVP/SWP pumping facilities. 
#' data sourced from CDFW Salvage Database, with Length-at-Date (LAD) run assignment used for Winter-run Chinook.

require(shiny)
require(ggplot2)
require(plotly)
require(dplyr)
require(shinydashboard)
require(lubridate)
require(here)

# Load helper functions
source(here::here("apps.R/utils_SacPAStheme.R"))

# Load pre-generated data
load(here::here("data/steelhead_loss_export_data.rda"))
load(here::here("data/winter_run_chinook_loss_export_data.rda"))

combined_df <- bind_rows(
  steelhead_loss_export_data %>% mutate(Species = "Steelhead"),
  winter_run_chinook_loss_export_data %>% mutate(Species = "Winter-run Chinook")
)

ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "SacPAS: Interactive Plot"),
  shinydashboard::dashboardSidebar(disable = TRUE),
  shinydashboard::dashboardBody(
    # Add CSS SacPAS global theme
    fresh::use_theme(SacPAStheme),
    fluidRow(
      shinydashboard::box(
        title = "Daily Total Loss and CVP/SWP Exports", 
        status = "primary", 
        width = 12,
        solidHeader = TRUE,
        "Visualize daily total loss for Natural Winter-run Chinook and Steelhead and daily exports at CVP/SWP pumping facilities. 
        In the `Select Inputs` section, select a species and a pumping facility to view specific data. To view without approximate OMRI controlling factor lines, deselect the checkbox option. 
        On each figure, hover over the plot to view specific data points of interest and remove information from the plot by clicking on the legend entry."
      )
    ),
    fluidRow(
      shinydashboard::box(
        title = "Select Inputs:", 
        status = "info", 
        width = 3,
        solidHeader = TRUE,
        selectInput(inputId = "select_species", 
                    label = "Select Species",
                    choices = c("Winter-run Chinook", "Steelhead")
        ),
        selectInput(inputId = "select_plot", 
                    label = "Select Pumping Facility",
                    choices = c("Combined, CVP & SWP", unique(combined_df$facility), "Compare All")),
        checkboxInput(inputId = "showLines", 
                      label = "Include approximate changes in OMRI values by date", 
                      value = TRUE),
        # Conditional OMRI controls
        conditionalPanel(
          condition = "input.showLines",
          # Add numeric input for OMRI value
          # HTML("<p><b>Enter OMRI values and dates:</b></p>"),
          numericInput(
            inputId = "omriValue",
            label = "Add OMRI Value:",
            value = -500
          ),
          # Add date input
          dateInput(
            inputId = "omriDate",
            label = "with OMRI Date:",
            value = Sys.Date()
          ),
          # Add buttons
          actionButton("addLine", "Add OMRI Line"),
          actionButton("resetLines", "Reset OMRI Lines"),
          br(),
          HTML("<p>Select 'Add OMRI Line' to add a new vertical line to the plot(s). 
               Additionally, a user can manually add/edit the values/dates in the text box area.</p>"),
          br(),
          br(),
          # Text area for editing
          textAreaInput(
            inputId = "omriTextArea",
            label = "Plotted OMRI Lines (Edit Values/Dates):",
            value = "",
            rows = 6,
            placeholder = "value,date e.g.,\n-5000,2024-01-01\n-2000,2024-12-03\n-500,2025-10-24"
          )
        ),
        br(),
        br(),
        br(),
        shinydashboard::box(
          width = 12,
          title = "Contact Information",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          HTML("<p>This ShinyApp is a product of Columbia Basin Research, School of Aquatic and Fishery Sciences, College of the Environment, University of Washington.</p>
               <p>Please direct general questions to: <a href='mailto:web@cbr.washington.edu'>web@cbr.washington.edu</a></p>"
          ),
          HTML("All code featured in this Shiny application is made publicly available through our organization's GitHub repository: 
               <a href='https://github.com/Columbia-Basin-Research-CBR/track-a-cohort'><i class='fab fa-github'></i> Columbia-Basin-Research-CBR</a>"
          )
        )
      ),
      shinydashboard::box(
        title = "Interactive plot: Daily Total Loss and Exports Per Pumping Facility", 
        status = "info", 
        solidHeader = TRUE,
        width = 9,
        conditionalPanel(
          condition = "input.select_plot != 'Compare All'",
          plotlyOutput(outputId = "interactivePlot")
        ),
        uiOutput(outputId = "comparePlots"),
        HTML("Data sources: Daily loss sourced from CDFW Salvage Database, with Length-at-Date (LAD) run assignment used for Winter-run Chinook.
             Export data sourced from CDFW and CFS from the CDEC sites HRO (for SWP) and TRP (for CVP). Approximate OMRI controlling values provided by USBR.")
      )
    )
  )
)

server <- function(input, output, session) {
  omriValues <- reactiveVal(data.frame(value = numeric(0), date = as.Date(character(0))))
  
  # Remove modal dialog code and update add line observer
  observeEvent(input$addLine, {
    req(input$omriValue, input$omriDate)
    new_value <- as.numeric(input$omriValue)
    new_date <- as.Date(input$omriDate)
    
    if (!is.na(new_value) && !is.na(new_date)) {
      current_values <- omriValues()
      updated_values <- rbind(current_values, 
                              data.frame(value = new_value, date = new_date))
      omriValues(updated_values)
      updateTextAreaInput(session, "omriTextArea", 
                          value = paste0(apply(updated_values, 1, 
                                               function(row) paste(row, collapse = ",")), 
                                         collapse = "\n"))
    }
  })
  
  observeEvent(input$resetLines, {
    omriValues(data.frame(value = numeric(0), date = as.Date(character(0))))
    updateTextAreaInput(session, "omriTextArea", value = "")
  })
  
  observeEvent(input$omriTextArea, {
    lines <- strsplit(input$omriTextArea, "\n")[[1]]
    parsed_values <- do.call(rbind, lapply(lines, function(line) {
      parts <- strsplit(line, ",")[[1]]
      if (length(parts) == 2) {
        tryCatch({
          value <- as.numeric(parts[1])
          date <- as.Date(parts[2], format = "%Y-%m-%d")
          if (!is.na(value) && !is.na(date)) {
            data.frame(value = value, date = date)
          } else {
            NULL
          }
        }, error = function(e) NULL)
      } else {
        NULL
      }
    }))
    if (!is.null(parsed_values)) {
      omriValues(parsed_values)
    }
  })
  
  # Helper function to create plots
  createPlot <- function(data_to_plot, species_name = input$select_species) {
    # Create initial plot with bars for CVP and SWP daily total loss
    p <- plot_ly(data = data_to_plot, x = ~date) %>%
      add_bars(data = subset(data_to_plot, facility == "CVP"), y = ~daily_total_loss, 
               name = if (species_name == "Winter-run Chinook")  "CVP LAD Loss" else "CVP Daily Loss", 
               marker = list(color = '#CE900D')) %>%
      add_bars(data = subset(data_to_plot, facility == "SWP"), y = ~daily_total_loss, 
               name = if (species_name == "Winter-run Chinook")  "SWP LAD Loss" else "SWP Daily Loss", 
               marker = list(color = '#0072B2')) %>%
      layout(
        title = if (species_name == "Winter-run Chinook") paste("Daily Total LAD Loss and Exports for Natural", species_name) else paste("Daily Total Loss and Exports for", species_name),
        barmode = "group",
        yaxis = list(title = "Daily Loss"),
        xaxis = list(title = "Date", type = "date"),
        legend = list(title = list(text = NULL), x = 1.1),
        margin = list(l = 60, r = 200, t = 50, b = 50)
      )
    
    # Add line plot for CVP and SWP pumping discharge on secondary y-axis
    p <- p %>%
      add_lines(data = subset(data_to_plot, facility == "CVP"), y = ~pumping_discharge_cfs, name = "CVP Export", line = list(color = '#F5C767'), yaxis = "y2") %>%
      add_lines(data = subset(data_to_plot, facility == "SWP"), y = ~pumping_discharge_cfs, name = "SWP Export", line = list(color = '#00BFFF'), yaxis = "y2") %>%
      layout(
        yaxis2 = list(overlaying = "y", side = "right", title = "Pumping Discharge (cfs)")
      )
    
    # Conditionally add vertical lines and text if input$showLines is TRUE
    current_lines <- omriValues()
    if (input$showLines && nrow(current_lines) > 0) {
      for (i in seq_len(nrow(current_lines))) {
        p <- p %>%
          add_segments(
            x = current_lines$date[i], xend = current_lines$date[i],
            y = 0, yend = max(data_to_plot$daily_total_loss, na.rm = TRUE),
            line = list(color = "grey", width = 1), showlegend = FALSE
          ) %>%
          add_annotations(
            x = current_lines$date[i],
            y = max(data_to_plot$daily_total_loss, na.rm = TRUE),
            text = paste("OMRI:", current_lines$value[i]),
            showarrow = FALSE, yanchor = "bottom", textangle = 90
          )
      }
    }
    
    return(p)
  }
  
  # Render the appropriate plot based on the selection
  output$interactivePlot <- renderPlotly({
    req(input$select_plot != "Compare All")
    
    if (input$select_plot == "Combined, CVP & SWP") {
      
      data_to_plot <- combined_df %>% filter(Species == input$select_species)
    } else {
      
      data_to_plot <- combined_df %>% 
        filter(facility == input$select_plot & Species == input$select_species)
      
    }
    
    createPlot(data_to_plot)
  })
  
  # Render additional plots for "Compare All" option
  output$comparePlots <- renderUI({
    if (input$select_plot == "Compare All") {
      plot_output_list <- list(
        h3(tags$span(style = "color: #45585E;", "Comparison of facilities, CVP & SWP")),
        plotlyOutput(outputId = "plot_combined"),
        h3(tags$span(style = "color: #45585E;", "Individual facility, CVP")),
        plotlyOutput(outputId = "plot_cvp"),
        h3(tags$span(style = "color: #45585E;", "Individual facility, SWP")),
        plotlyOutput(outputId = "plot_swp")
      )
      
      do.call(tagList, plot_output_list)
    } else {
      return(NULL)
    }
  })
  
  # Define rendering for each plot when "Compare All" is selected
  observe({
    if (input$select_plot == "Compare All") {
      output$plot_combined <- renderPlotly({
        
        data_to_plot <- combined_df %>% filter(Species == input$select_species)
        
        createPlot(data_to_plot)
      })
      
      output$plot_cvp <- renderPlotly({
        
        data_to_plot <- combined_df %>% 
          filter(facility == "CVP" & Species == input$select_species)
        createPlot(data_to_plot)
      })
      
      output$plot_swp <- renderPlotly({
        
        data_to_plot <- combined_df %>% 
          filter(facility == "SWP" & Species == input$select_species)
        createPlot(data_to_plot)
      })
    }
  })
}

shinyApp(ui = ui, server = server)