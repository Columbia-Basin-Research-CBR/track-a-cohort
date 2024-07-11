library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(xts)
library(zoo)
library(gghighlight)
library(fresh)
library(here)

source(here::here("utils_SacPAStheme.R"))

# Function to convert DOY to month name
doy_to_month <- function(doy, year_type) {
  # Adjust the DOY based on the year type
  adjusted_doy <- if (year_type == "WY") {
    (doy + 273) %% 365 + 1
  } else {
    doy
  }
  # Convert the adjusted DOY to a date
  date <- as.Date(paste(2000, adjusted_doy), format = "%Y %j")
  # Return the month name
  return(format(date, "%b"))
}

# Function to plot figures 
fct_stars_survival_plot <- function(data, x_var, year, metric, hydro, hydro_type) {
  
  # Define y variable and ribbon variables based on metric
  y_var <- metric
  ymin_var <- paste0(metric, "L80")
  ymax_var <- paste0(metric, "U80")
  
  # # Define title based on metric
  # rct_title <- switch(metric,
  #                     surv = 'STARS model - Overall Survival',
  #                     idsurv = 'STARS model -Interior Delta Route-specific Survival Probability',
  #                     idRoute = 'STARS model - Interior Delta Route-specific Probability',
  #                     'STARS model - Survival')
  
  # # Define title based on metric
  # rct_caption <- switch(metric,
  #                       surv = "The solid line shows median survival of daily cohorts of winter run Chinook Salmon\n through the Delta from Knights Landing to Chipps Island for all routes combined.",
  #                       idsurv = "Route-specific survival of daily cohorts of winter run Chinook Salmon\nthrough the Delta from Knights Landing to Chipps Island.",
  #                       idRoute = "Proportion of daily cohorts of winter run Chinook Salmon\nthrough the Delta (Knights Landing to Chipps Island) using the Interior Delta route.")
  
  # Define title based on metric
  rct_x_var <- switch(x_var,
                      wDay =  'Month\n(Water Year: Oct-Dec of year [t-1], Jan-Sep of year [t])',
                      doy =  'Month\n(Calendar Year: Jan-Dec of year [t])')
  
  # Generate the labels for the x-axis
  labels <- sapply(seq(1, 365, by = 60), function(doy) doy_to_month(doy, year))

  
  # Define color based on hydro
  color_var <- if (hydro == "TRUE") {
    hydro_type
  } else {
    NULL
  }
  
  # Define all possible levels of hydro_type
  all_levels <- c("Wet", "Above Normal", "Below Normal", "Dry", "Critical")
  
  # Create a color palette that includes all possible levels
  color_palette <- setNames(c("darkblue", "cadetblue", "honeydew3", "navajowhite3", "salmon4"), all_levels)
  
  
  
  # Plot
  
  p <- ggplot(data, aes_string(x = x_var, group = year)) +
    geom_line(aes_string(y = y_var, color = color_var)) +
    labs(x = rct_x_var,
         y = 'Probability',
         title =  NULL,#rct_title,
         subtitle = "Years of data : 2018 to 2024",
         #losing caption with plotly--see plotly annotation instead
         # caption = rct_caption
         color = "Hydrologic Classification Indices"
         ) +
    scale_color_manual(values = color_palette, drop = FALSE) +
    gghighlight(use_direct_label = FALSE) +
    facet_wrap(as.formula(paste0("~", year))) +
    geom_ribbon(aes_string(ymin = ymin_var, ymax = ymax_var), alpha = 0.25) +
    scale_x_continuous(breaks = seq(1, 365, by = 60), labels = labels) +
    # scale_x_continuous(breaks = seq(1, 365, by = 60), labels = day_to_month) +
    theme_minimal()

  
return(p)
}

ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "STARS Survival Plot"),
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
        inputId = "select_yeartype",
        label = "Select year type:", 
        choices = c("Water Year", "Calendar Year"),
        multiple = FALSE)
        ),
        column(
          width = 3,
          selectInput(
                  inputId = "select_metric", 
                  label = "Select Probability:", 
                  choices = c("Overall Survival" = "surv",
                              "Interior Delta Route-specific Survival Probability" = "idsurv",
                              "Interior Delta Route-specific Probability" = "idRoute"),
                  multiple = FALSE)
        ),
      column(
        width = 3,
        selectInput(
          inputId = "select_year", 
          label = "View years:", 
          choices = c("All years", 2017:2024),
          selected = "All years"
        )
      )
      )
      ),
      shinydashboard::box(
      width = 12,
      status = "success",
      fluidRow(
        column(
          width = 9
        ),
      column(
        width = 3,
        shinyWidgets::materialSwitch(
          inputId = "select_hydro", 
          label = "Show Hydrologic Year Type", 
          value = TRUE,
          status = "primary"
        )
      )
      ),
      uiOutput("plot_caption"),
      plotly::plotlyOutput("plot")
      )
    )
    )
  )

server <- function(input, output, session) {
  output$plot <- plotly::renderPlotly({
    # Load data
    load(here::here("STARS.shinyinputs.Rdata")) #COB removed verbose=T to run with here::here
    
    # Subset the data and convert to tibble
    df_stars_raw <- tibble::as_tibble(WR_xts[,c("Survival Interior Delta Est", 
                                    "Survival Interior Delta LCL 80", 
                                    "Survival Interior Delta UCL 80",
                                    "Routing Probability Interior Delta Est",    
                                    "Routing Probability Interior Delta LCL 80",
                                    "Routing Probability Interior Delta UCL 80",
                                    "Survival Overall Est", 
                                    "Survival Overall LCL 80",
                                    "Survival Overall UCL 80")]) %>%
      # Add the first date as a new column
      dplyr::mutate(date = zoo::index(WR_xts)) %>%
      # Make date the first column
      dplyr::select(date, dplyr::everything()) %>%
      dplyr::rename( surv =  "Survival Overall Est", survL80 =  "Survival Overall LCL 80", survU80 =  "Survival Overall UCL 80", 
              idsurv = "Survival Interior Delta Est", idsurvL80 = "Survival Interior Delta LCL 80", idsurvU80 = "Survival Interior Delta UCL 80", 
              idRoute = "Routing Probability Interior Delta Est", idRouteL80 =  "Routing Probability Interior Delta LCL 80", idRouteU80 =  "Routing Probability Interior Delta UCL 80") %>%
      # convert date to WY, wday
      dplyr::arrange(date) %>% 
      dplyr::mutate(WY = lubridate::year(date) + (lubridate::month(date) >= 10),
             wDay = if_else(lubridate::month(date) >= 10, lubridate::yday(date) - 273, lubridate::yday(date) + 92),
             doy = lubridate::yday(date),
             CY = lubridate::year(date),
             wDate = if_else(lubridate::month(date) >= 10, date + lubridate::years(1), date))
    
    #pull in shared wytype.csv
    wytype <- read.csv(here::here('WYtype.csv')) %>%  dplyr::filter(Basin == "SacramentoValley")
    
    #append wytype to stars results
    df_stars<-df_stars_raw %>% 
      dplyr::inner_join(select(wytype, WY, hydro_type = `Yr.type`), by = "WY") %>% 
      dplyr::mutate( hydro_type = factor(hydro_type, levels = c("W", "AN", "BN", "D", "C"), labels = c("Wet", "Above Normal", "Below Normal", "Dry", "Critical")))
    
    # Add this reactive expression
    output$plot_caption <- renderUI({
      HTML(switch(input$select_metric,
             surv = "<b>STARS model - Overall Survival:</b><br>The solid line shows median survival of daily cohorts of winter run Chinook Salmon through the Delta (Knights Landing to Chipps Island) for all routes combined.",
             idsurv = "<b>STARS model -Interior Delta Route-specific Survival Probability:</b><br> Route-specific survival of daily cohorts of winter run Chinook Salmon through the Delta (Knights Landing to Chipps Island).",
             idRoute = "<b>STARS model - Interior Delta Route-specific Probability:</b> <br>Proportion of daily cohorts of winter run Chinook Salmon through the Delta (Knights Landing to Chipps Island) using the Interior Delta route."
             )
           )
    })
    
    # # Filter data
    # filtered_data <- df_stars %>% 
    #   filter(WY %in% input$select_years,
    #          CY %in% input$select_years)
    # Filter data
    if (input$select_year == "All years") {
           df_stars
    } else {
      filtered_data <- df_stars %>% 
        dplyr::filter(WY == input$select_year , CY == input$select_year)
    }
    
    # if(input$select_yeartype == "Water Year") {
    #   p<-fct_stars_survival_plot(data = filtered_data, x_var = "wDay", year = "WY", metric = input$select_metric, hydro = as.character(input$select_hydro), hydro_type = df_stars$hydro_type)
    # } else {
    #   p<-fct_stars_survival_plot(data =  filtered_data, x_var = "doy", year = "CY", metric = input$select_metric, hydro = as.character(input$select_hydro), hydro_type = df_stars$hydro_type)
    # }
    
    if(input$select_yeartype == "Water Year") {
      if (input$select_year == "All years") {
        p <- fct_stars_survival_plot(data = df_stars, x_var = "wDay", year = "WY", metric = input$select_metric, hydro = as.character(input$select_hydro), hydro_type = df_stars$hydro_type)
      } else {
        p <- fct_stars_survival_plot(data = filtered_data, x_var = "wDay", year = "WY", metric = input$select_metric, hydro = as.character(input$select_hydro), hydro_type = filtered_data$hydro_type)
      }
    } else {
      if (input$select_year == "All years") {
        p <- fct_stars_survival_plot(data = df_stars, x_var = "doy", year = "CY", metric = input$select_metric, hydro = as.character(input$select_hydro), hydro_type = df_stars$hydro_type)
      } else {
        p <- fct_stars_survival_plot(data = filtered_data, x_var = "doy", year = "CY", metric = input$select_metric, hydro = as.character(input$select_hydro), hydro_type = filtered_data$hydro_type)
      }
    }
  
    
   return(p)
    
  })
}

shinyApp(ui, server)