library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(fresh)
library(here)


source(here::here("utils_SacPAStheme.R"))
source(here::here("mod_fct_plot_cumulative_loss_single_year_threshold.R"))



ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Cumulative Loss - Single Year Threshold"),
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
                 shinyWidgets::pickerInput(
                   inputId = "select_species",
                   label = "Select Species", 
                   choices = c("Winter-run Chinook", "Steelhead")
                 )
                 # selectInput(
                 #   inputId = "select_species",
                 #   label = "Select Species:", 
                 #   choices = c("Winter-run Chinook", "Steelhead"),
                 #   multiple = FALSE)
          ),
          # Use conditionalPanel to conditionally display this input
          conditionalPanel(
            condition = "input.select_species === 'Winter-run Chinook'",
            column(
              width = 3,
              selectInput(
                inputId = "select_metric", 
                label = "Select Run Designation Method:", 
                choices = c("Genetic", "LAD"),
                selected = "LAD",
                multiple = FALSE)
            )
          )
          # column(
          #   width = 3,
          #   selectInput(
          #     inputId = "select_metric", 
          #     label = "Select Run Designation Method:", 
          #     choices = c("Genetic", "LAD", "Not Applicable"),
          #     multiple = FALSE)
          # )
        )
      ),
      shinydashboard::box(
        width = 12,
        status = "success",
        uiOutput("plot_caption"),
        plotly::plotlyOutput("plot", height = "100%")
      )
    )
  )
)


server <- function(input, output, session) {
  
  
  #plotly code
  output$plot <- plotly::renderPlotly({
    
    # Load data
    load(here("jpe_genetic_loss_data.rda"))
    genetic_cumulative_loss_data <- jpe_genetic_loss_data$genetic_cumulative_loss_data
    load(here("jpe_lad_loss_data.rda"))
    lad_cumulative_loss_data <- jpe_lad_loss_data$lad_cumulative_loss_data
    load(here("steelhead_loss_current_year_data.rda"))
    
    # Assign data based on species and metric selection
    if (input$select_species == "Winter-run Chinook") {
      if (input$select_metric == "Genetic") {
        data <- genetic_cumulative_loss_data

      } else {
        data <- lad_cumulative_loss_data

      }
    } else if (input$select_species == "Steelhead") {
      data <- steelhead_loss_current_year_data

    }
    
    
    plot<- wrangle_plot_data(data = data, selected_species = input$select_species)
    
    return(plot)
    
  })
}

shinyApp(ui, server)
  