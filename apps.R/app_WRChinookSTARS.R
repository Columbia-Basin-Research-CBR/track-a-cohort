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
  load(here::here("STARS.shinyinputs.Rdata")) 
  source(here("utils_fct_assign_current_water_year.R"))
  source(here::here("utils_import_hydrological_classification_index.R"))
}, silent = TRUE)

# if(!exists("SacPAStheme") || !exists("assign_current_water_year")|| !exists("wDay_to_month") || !exists("hydrological_classification_index")) {
#   source(here::here("apps.R/utils_SacPAStheme.R"))
#   source(here("apps.R/utils_fct_wday_to_month.R"))
#   load(here::here("apps.R/STARS.shinyinputs.Rdata"))
#   source(here("apps.R/utils_fct_assign_current_water_year.R"))
#   source(here::here("apps.R/utils_import_hydrological_classification_index.R"))
# }

current_year <- assign_current_water_year()

fct_stars_survival_plot <- function(data, metric, hydro, hydro_type){
  y_var <- paste0(metric)
  ymin_var <- paste0(metric, "L80")
  ymax_var <- paste0(metric, "U80")
  
  all_levels <- c("Wet", "Above Normal", "Below Normal", "Dry", "Critical", "Unassigned")
  
  if (hydro == "TRUE") {
    color_palette <- setNames(c("#00008b", "#74a9cf", "#FDECC7", "#f1ac1c", "#cc4c02", "grey"), all_levels)
    if (any(data$WY == current_year & data$hydro_type == "Unassigned")) {
      color_palette["Unassigned"] <- "black"
    }
  } else {
    unique_years <- unique(data$WY)
    color_palette <- c("#E69F00", "#56B4E9","#009E73","#999999","#0072B2" ,"#D55E00", "#CC79A7", "#F0E442")
    names(color_palette) <- as.character(unique_years)
    color_palette[as.character(current_year)] <- "black"
  }
  
  p <- plot_ly(data = data, x = ~wDay)
  
  for (wy in unique(data$WY)) {
    p <- p %>%
      add_lines(
        data = data %>% filter(WY == wy),
        y = ~.data[[y_var]], 
        line = list(width = ifelse(wy == current_year, 3, 1.5)), 
        color = if (hydro == "TRUE") ~hydro_type else ~as.factor(WY),
        colors = color_palette,
        name = if (hydro == "TRUE") ~paste0(hydro_type, " - WY", WY) else ~paste0("WY", WY),
        hoverinfo = "text",
        text = ~paste("Date:", wDate, "\nProbability:", round(.data[[y_var]],2), "\nWY:", WY)
      )
  }
  
  p <- p %>%
    add_ribbons(
      data = data %>% filter(WY == current_year),
      ymin = ~.data[[ymin_var]], 
      ymax = ~.data[[ymax_var]], 
      fillcolor = 'rgba(0,0,0,0.1)', 
      line = list(color = 'rgba(0,0,0,0)'),
      name = ~paste0("WY",current_year, ", 80% CI"),
      showlegend = TRUE,
      hoverinfo = "text",
      text = ~paste("Date:", wDate, "<br>Lower:", round(.data[[ymin_var]],2), "<br>Upper:", round(.data[[ymax_var]],2))
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
  shinydashboard::dashboardHeader(title = "SacPAS: Interactive Plot"),
  shinydashboard::dashboardSidebar(disable = TRUE),
  shinydashboard::dashboardBody(
    fresh::use_theme(SacPAStheme),
    fluidRow(
      shinydashboard::box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title = "STARS model - Winter-run Chinook Salmon",
        "This app allows you to visualize STARS model results for Winter-run Chinook Salmon in past years compared to the current water year. 
        Select a specific survival probability below to adjust the plot. Users can also adjust color coding to reflect Hydrological Classification Index by selecting the `Show Hydrological Year Type` switch below. 
        To add/remove years from plot, click the water year within the plot legend or select in the drop down menu below. 
        Data sourced from Delta STARS developed by USGS Quantitative Fisheries Ecology Section and deployed by SacPAS."
      )
    ),
    fluidRow(
      shinydashboard::box(
        width = 3,
        status = "info",
        solidHeader = TRUE,
        title = "Select Inputs:", 
        selectInput(
          inputId = "select_metric", 
          label = "Select Probability:", 
          choices = c("Overall Survival" = "surv",
                      "Interior Delta Route-specific Survival Probability" = "idsurv",
                      "Interior Delta Route-specific Probability" = "idRoute",
                      "Compare all probabilities" = "compare_all"),
          multiple = FALSE),
        selectInput(
          inputId = "select_year", 
          label = "View years:", 
          choices = c("All years", 2017:2024),
          selected = "All years"
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
          HTML("<p>This ShinyApp is a product of Columbia Basin Reasearch, School of Aquatic and Fishery Sciences, College of the Environment, University of Washington.</p>
               <p>Please direct general questions to: <a href='mailto:web@cbr.washington.edu'>web@cbr.washington.edu</a></p>"
          ),
          HTML("All code featured in this Shiny application is made publically available through our organizations GitHub repository: 
             <a href='https://github.com/Columbia-Basin-Research-CBR/track-a-cohort'><i class='fab fa-github'></i> Columbia-Basin-Research-CBR</a>"
          )
        )
      ),
      shinydashboard::box(
        width = 9,
        status = "success",
        solidHeader = TRUE,
        title = "Interactive plot: STARS model - Winter-run Chinook Salmon",
        fluidRow(
          column(
            width = 9
          ),
          column(
            width = 3,
            shinyWidgets::materialSwitch(
              inputId = "select_hydro", 
              label = HTML("<b>Show Hydrologic Year Type</b>"), 
              value = FALSE,
              status = "primary"
            )
          )
        ),
        uiOutput("plot_caption"),
        uiOutput("plots"),
        h5("Data source: Delta STARS developed by USGS Quantitative Fisheries Ecology Section and deployed by SacPAS.")
      )
    )
  )
)

server <- function(input, output, session) {
  df_stars_raw <- tibble::as_tibble(WR_xts[,c("Survival Interior Delta Est", 
                                              "Survival Interior Delta LCL 80", 
                                              "Survival Interior Delta UCL 80",
                                              "Routing Probability Interior Delta Est",    
                                              "Routing Probability Interior Delta LCL 80",
                                              "Routing Probability Interior Delta UCL 80",
                                              "Survival Overall Est", 
                                              "Survival Overall LCL 80",
                                              "Survival Overall UCL 80")]) %>%
    dplyr::mutate(date = zoo::index(WR_xts)) %>%
    dplyr::select(date, dplyr::everything()) %>%
    dplyr::rename(surv =  "Survival Overall Est", survL80 =  "Survival Overall LCL 80", survU80 =  "Survival Overall UCL 80", 
                  idsurv = "Survival Interior Delta Est", idsurvL80 = "Survival Interior Delta LCL 80", idsurvU80 = "Survival Interior Delta UCL 80", 
                  idRoute = "Routing Probability Interior Delta Est", idRouteL80 =  "Routing Probability Interior Delta LCL 80", idRouteU80 =  "Routing Probability Interior Delta UCL 80") %>%
    dplyr::arrange(date) %>% 
    dplyr::mutate(WY = lubridate::year(date) + (lubridate::month(date) >= 10),
                  wDay = if_else(lubridate::month(date) >= 10, lubridate::yday(date) - 273, lubridate::yday(date) + 92),
                  doy = lubridate::yday(date),
                  CY = lubridate::year(date),
                  wDate = if_else(lubridate::month(date) >= 10, date + lubridate::years(1), date))
  
  df_stars <- df_stars_raw %>% 
    dplyr::left_join(select(hydrological_classification_index, WY, hydro_type = Classification), by = "WY") %>% 
    dplyr::mutate(hydro_type = factor(
      ifelse(is.na(hydro_type), "Unassigned", hydro_type), 
      levels = c("Wet", "Above Normal", "Below Normal", "Dry", "Critical", "Unassigned")
    )
    )
  
  output$plots <- renderUI({
    if (input$select_metric == "compare_all") {
      tagList(
        br(),
        HTML("<b>Overall Survival:</b><br>The solid line shows median survival of daily cohorts of winter run Chinook Salmon through the Delta (Knights Landing to Chipps Island) for all routes combined."),
        plotly::plotlyOutput("plot1"),
        HTML("<b>Interior Delta Route-specific Survival Probability:</b><br> Route-specific survival of daily cohorts of winter run Chinook Salmon through the Delta (Knights Landing to Chipps Island)."),
        plotly::plotlyOutput("plot2"),
        HTML("<b>Interior Delta Route-specific Probability:</b> <br>Proportion of daily cohorts of winter run Chinook Salmon through the Delta (Knights Landing to Chipps Island) using the Interior Delta route."),
        plotly::plotlyOutput("plot3")
      )
    } else {
      plotly::plotlyOutput("plot")
    }
  })
  
  output$plot_caption <- renderUI({
    HTML(switch(input$select_metric,
                surv = "<b>STARS model - Overall Survival:</b><br>The solid line shows median survival of daily cohorts of winter run Chinook Salmon through the Delta (Knights Landing to Chipps Island) for all routes combined.",
                idsurv = "<b>STARS model -Interior Delta Route-specific Survival Probability:</b><br> Route-specific survival of daily cohorts of winter run Chinook Salmon through the Delta (Knights Landing to Chipps Island).",
                idRoute = "<b>STARS model - Interior Delta Route-specific Probability:</b> <br>Proportion of daily cohorts of winter run Chinook Salmon through the Delta (Knights Landing to Chipps Island) using the Interior Delta route.",
                compare_all = "<b>STARS model - Compare All Probabilities:</b><br>"
    )
    )
  })
  
  render_plot <- function(metric) {
    if (input$select_year == "All years") {
      fct_stars_survival_plot(data = df_stars, metric = metric, hydro = as.character(input$select_hydro), hydro_type = df_stars$hydro_type)
    } else {
      filtered_data <- df_stars %>% 
        dplyr::filter(WY == input$select_year)
      
      fct_stars_survival_plot(data = filtered_data, metric = metric, hydro = as.character(input$select_hydro), hydro_type = filtered_data$hydro_type)
    }
  }
  
  output$plot <- plotly::renderPlotly({
    render_plot(input$select_metric)
  })
  
  output$plot1 <- plotly::renderPlotly({
    render_plot("surv")
  })
  
  output$plot2 <- plotly::renderPlotly({
    
    
    render_plot("idsurv")
  })
  
  output$plot3 <- plotly::renderPlotly({
    render_plot("idRoute")
  })
}

shinyApp(ui, server)