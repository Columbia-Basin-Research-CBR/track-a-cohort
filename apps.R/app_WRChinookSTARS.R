library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(xts)
library(zoo)
library(fresh)
library(here)

try({
  source(here::here("utils_SacPAStheme.R"))
  source(here("utils_fct_wday_to_month.R"))
  # load(here::here("data", "STARS_data.rda"))
  load(here::here("STARS.shinyinputs.Rdata")) 
  source(here("utils_fct_assign_current_water_year.R"))
  source(here::here("utils_import_hydrological_classification_index.R"))
}, silent = TRUE)

if(!exists("SacPAStheme") || !exists("STARS_data") || !exists("assign_current_water_year")|| !exists("wDay_to_month") || !exists("hydrological_classification_index")) {
  source(here::here("apps.R/utils_SacPAStheme.R"))
  source(here("apps.R/utils_fct_wday_to_month.R"))
  # load(here::here("apps.R/STARS_data.rda"))
  load(here::here("apps.R/STARS.shinyinputs.Rdata"))
  source(here("apps.R/utils_fct_assign_current_water_year.R"))
  source(here::here("apps.R/utils_import_hydrological_classification_index.R"))
}

current_year <- assign_current_water_year()


fct_stars_survival_plot <- function(data, metric, y_L80, y_U80, hydro, hydro_type){
  # Define y variable and ribbon variables based on metric
  y_var <- paste0(metric)
  ymin_var <- paste0(metric, "L80")
  ymax_var <- paste0(metric, "U80")
  
  # Define all possible levels of hydro_type
  all_levels <- c("Wet", "Above Normal", "Below Normal", "Dry", "Critical", "Unassigned")
  
  # Generate a dynamic color palette based on hydro -- using SI SacPAS colors on hydro webpage
  if (hydro == "TRUE") {
    color_palette <- setNames(c("#00008b", "#74a9cf", "#FDECC7", "#f1ac1c", "#cc4c02", "grey"), all_levels)
    # Override color for current year to black if hydro_type is "Unassigned"
    if (any(data$WY == current_year & data$hydro_type == "Unassigned")) {
      color_palette["Unassigned"] <- "black"
    }
  } else {
    unique_years <- unique(data$WY)
    color_palette <- c("#E69F00", "#56B4E9","#009E73","#999999","#0072B2" ,"#D55E00", "#CC79A7", "#F0E442")
    names(color_palette) <- as.character(unique_years)
    # Override color for current year to black
    color_palette[as.character(current_year)] <- "black"
  }
  
  # Create a plotly object
  p <- plot_ly(data = data, x = ~wDay)
  
  # Add lines for each year with specific line width for current year
  for (wy in unique(data$WY)) {
    p <- p %>%
      add_lines(
        data = data %>% filter(WY == wy),
        y = ~.data[[y_var]], 
        line = list(width = ifelse(wy == current_year, 3, 1.5)), 
        color = if (hydro == "TRUE") ~hydro_type else ~as.factor(WY),
        colors = color_palette,
        name = if (hydro == "TRUE") ~paste0(hydro_type, " - WY", WY) else ~paste0("WY", WY)
      )
  }
  
  # Add ribbons for the current year
  p <- p %>%
    add_ribbons(
      data = data %>% filter(WY == current_year),
      ymin = ~.data[[ymin_var]], 
      ymax = ~.data[[ymax_var]], 
      fillcolor = 'rgba(0,0,0,0.1)', 
      line = list(color = 'rgba(0,0,0,0)'),
      name = ~paste0("WY",current_year, ", 80% CI"),
      showlegend = TRUE
    ) %>%
    layout(
      xaxis = list(
        title = "Month",
        tickvals = seq(1, 365, by = 61),
        ticktext = wDay_to_month(seq(1, 365, by = 61))
      ),
      yaxis = list(
        title = "Apparent survival probability",
        rangemode = "tozero"
      ),
      legend = list(
        title = list(text = if (hydro == "TRUE") "Hydrologic Year Type" else "Water Year"),
        traceorder = "grouped"
      ),
      margin = list(l = 50, r = 50, t = 50, b = 50),
      paper_bgcolor = 'white',
      plot_bgcolor = 'white'
    )
  
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
      plotly::plotlyOutput("plot"),
      h5("Data source: Delta STARS developed by USGS Quantitative Fisheries Ecology Section and deployed by SacPAS.")
      )
    )
  )
)

server <- function(input, output, session) {
  output$plot <- plotly::renderPlotly({

    
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
    
    #append wytype to stars results
    df_stars<-df_stars_raw %>% 
      dplyr::left_join(select(hydrological_classification_index, WY, hydro_type = Classification), by = "WY") %>% 
      dplyr::mutate(hydro_type = factor(
                      ifelse(is.na(hydro_type), "Unassigned", hydro_type), 
                      levels = c("Wet", "Above Normal", "Below Normal", "Dry", "Critical", "Unassigned")
                    )
                  )
    
    # Add this reactive expression
    output$plot_caption <- renderUI({
      HTML(switch(input$select_metric,
             surv = "<b>STARS model - Overall Survival:</b><br>The solid line shows median survival of daily cohorts of winter run Chinook Salmon through the Delta (Knights Landing to Chipps Island) for all routes combined.",
             idsurv = "<b>STARS model -Interior Delta Route-specific Survival Probability:</b><br> Route-specific survival of daily cohorts of winter run Chinook Salmon through the Delta (Knights Landing to Chipps Island).",
             idRoute = "<b>STARS model - Interior Delta Route-specific Probability:</b> <br>Proportion of daily cohorts of winter run Chinook Salmon through the Delta (Knights Landing to Chipps Island) using the Interior Delta route."
             )
           )
    })
  
    
    if (input$select_year == "All years") {
      p <- fct_stars_survival_plot(data = df_stars, metric = input$select_metric, hydro = as.character(input$select_hydro), hydro_type = df_stars$hydro_type)
    } else {
      filtered_data <- df_stars %>% 
        dplyr::filter(WY == input$select_year)
      
      p <- fct_stars_survival_plot(data = filtered_data, metric = input$select_metric, hydro = as.character(input$select_hydro), hydro_type = filtered_data$hydro_type)
    }
  
    
   return(p)
    
  })
}

shinyApp(ui, server)