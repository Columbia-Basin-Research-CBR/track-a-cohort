library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(fresh)
library(here)

# Try loading supporting files and data
try({
  source(here::here("utils_SacPAStheme.R"))
  source(here::here("mod_fct_plot_cumulative_loss.R"))
  
  # Load data
  load(here("jpe_lad_loss_data.rda"))
  load(here("steelhead_loss_data.rda"))
  genetic_cumulative_loss_data <- jpe_genetic_loss_data$genetic_cumulative_loss_data
  lad_cumulative_loss_data <- jpe_lad_loss_data$lad_cumulative_loss_data
}, silent = TRUE)

# Check if the data was loaded successfully
if (!exists("genetic_cumulative_loss_data") || !exists("lad_cumulative_loss_data")) {
  # If not, try the alternative section
  source(here::here("apps.R/utils_SacPAStheme.R"))
  source(here::here("apps.R/mod_fct_plot_cumulative_loss.R"))
  
  load(here("apps.R/steelhead_loss_data.rda"))
  load(here("apps.R/jpe_genetic_loss_data.rda"))
  load(here("apps.R/jpe_lad_loss_data.rda"))
  genetic_cumulative_loss_data <- jpe_genetic_loss_data$genetic_cumulative_loss_data
  lad_cumulative_loss_data <- jpe_lad_loss_data$lad_cumulative_loss_data
}

# Check if the data was loaded successfully
if (exists("genetic_cumulative_loss_data") && exists("lad_cumulative_loss_data")) {
  print("Data loaded successfully.")
} else {
  stop("Failed to load data.")
}



ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Cumulative Loss Plot"),
  shinydashboard::dashboardSidebar(disable = TRUE),
  shinydashboard::dashboardBody(
    #add CSS SacPAS global theme
    fresh::use_theme(SacPAStheme),
    fluidRow(
      shinydashboard::box(
        width = 12,
        status = "info",
        fluidRow(
        column(width = 3,
        selectInput(
        inputId = "select_species",
        label = "Select Species:", 
        choices = c("Winter-run Chinook", "Steelhead"),
        multiple = FALSE)
        ),
        column(
          width = 3,
          selectInput(
                  inputId = "select_metric", 
                  label = "Select Run Designation Method:", 
                  choices = c("Genetic", "LAD", "Not Applicable"),
                  multiple = FALSE)
      )
      )
      ),
      shinydashboard::box(
      width = 12,
      status = "success",
      fluidRow(
        # column(
        #   width = 9
        # ),
        column(
          width = 3,
        shinyWidgets::materialSwitch(
          inputId = "select_loss", 
          label = "Select Percent or Count Loss", 
          value = TRUE,
          status = "primary"
        ),
        textOutput(outputId = "switch_message_loss")
      ),
      column(
        width = 3,
        shinyWidgets::materialSwitch(
          inputId = "select_hydro", 
          label = "Show Hydrologic Year Type", 
          value = FALSE,
          status = "primary"
        )
      ),
      column(
        width = 3,
        shinyWidgets::materialSwitch(
          inputId = "select_biop", 
          label = "Show BiOp Year Type", 
          value = FALSE,
          status = "primary"
        )
      ),
      column(
        width = 3,
        shinyWidgets::materialSwitch(
          inputId = "select_outlier", 
          label = "Remove outlier years", 
          value = FALSE,
          status = "primary"
        ),
        textOutput(outputId = "switch_message_outlier")
      )
      ),
      uiOutput("plot_caption"),
      plotly::plotlyOutput("plot", height = "100%")
      )
    )
    )
  )

server <- function(input, output, session) {
  
  #update data selection choices based on species input
  observe({
    if (input$select_species == "Winter-run Chinook") {
      updateSelectInput(session, "select_metric", choices = c("Genetic", "LAD"))
      # Optionally, reset the switch to its original state if needed
      shinyWidgets::updateMaterialSwitch(session, "select_loss", value = TRUE)
      output$switch_message_loss <- renderText({""})  # Clear the message
    } else if (input$select_species == "Steelhead") {
      updateSelectInput(session, "select_metric", choices = "Not Applicable")
      shinyWidgets::updateMaterialSwitch(session, "select_loss", value = FALSE)  # Force switch to FALSE
      output$switch_message_loss <- renderText({"Only count data available for Steelhead."})
    }
  })
  
  
  #plotly code
  output$plot <- plotly::renderPlotly({
  
    
    # Assign data based on species and metric selection
    if (input$select_species == "Winter-run Chinook") {
      if (input$select_metric == "Genetic") {
        data <- genetic_cumulative_loss_data
      } else {
        data <- lad_cumulative_loss_data
      }
    } else {
      data <- steelhead_loss_data
    }
    
   wrangle_plot_data<- wrangle_plot_data(data = data, selected_loss = input$select_loss, selected_hydro = input$select_hydro, selected_biop = input$select_biop, selected_outlier = input$select_outlier)
    
    

    
   return(wrangle_plot_data$plot)

  })

  observe({
    if (input$select_outlier == TRUE) {
       output$switch_message_outlier <- renderText({"Outlier years greater than double the mean were removed."})
    } else {
      output$switch_message_outlier <- renderText({""})
    }
  })
  
  
  # Display the message below the material switch
  output$switch_message_ui <- renderUI({
    textOutput("switch_message_loss")
  })
  # Display the message below the material switch
  output$switch_message_ui <- renderUI({
    textOutput("switch_message_outlier")
  })
  
  observe({
    if (input$select_outlier == TRUE) {
     # output$switch_message_outlier <- renderText({paste("Outlier years greater than double the mean were removed. Includes:", paste(wrangle_plot_data$outlier_years_reactive, collapse = ", "))})
      # outlier_years <- wrangle_plot_data$outlier_years_reactive  # Call it as a function
      # outlier_years()
      } else {
      output$switch_message_outlier <- renderText({""})
    }
  })

  
}

shinyApp(ui, server)