require(shiny)
require(shinydashboard)
require(plotly)
require(tidyverse)
require(xts)
require(zoo)
require(fresh)
require(here)


# load helper functions
source(here::here("apps.R/utils_SacPAStheme.R"))
source(here("R/utils_fct_wday_to_month.R"))
source(here("R/utils_fct_assign_current_water_year.R"))

# load pre generated data
  load(here::here("data/STARS_data.rda")) 
  load(here::here("data/hydrological_classification_index.rda"))
  
current_year <- assign_current_water_year()

fct_stars_current_water_year_plot <- function(data, metric, hydro, hydro_type){
  y_var <- paste0(metric)
  ymin_var <- paste0(metric, "L80")
  ymax_var <- paste0(metric, "U80")
  
  all_levels <- c("Wet", "Above Normal", "Below Normal", "Dry", "Critical", "Unassigned")
  
  if (hydro == "TRUE") {
    color_palette <- setNames(c("#00008b", "#74a9cf", "#D2B48C", "#f1ac1c", "#cc4c02", "grey"), all_levels)
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
    if (hydro == "TRUE") {
      color <- color_palette[as.character(data$hydro_type[data$WY == wy][1])]
      transparency <- if (color %in% c("#D2B48C", "#f1ac1c")) "30" else "10"  # 0.4 transparency for specified colors, 0.2 for others
    } else {
      color <- color_palette[as.character(wy)]
      transparency <- if (color %in% c("#E69F00", "#F0E442")) "30" else "10"  # 0.4 transparency for specified colors, 0.2 for others
    }
    
    fillcolor <- if (wy == current_year) "rgba(0, 0, 0, .15)" else paste0(color, transparency) 
    p <- p %>%
      add_lines(
        data = data %>% filter(WY == wy),
        y = ~.data[[y_var]], 
        line = list(width = ifelse(wy == current_year, 3, 1.5)), 
        color = if (hydro == "TRUE") ~hydro_type else ~as.factor(WY),
        colors = color_palette,
        name = if (hydro == "TRUE") ~paste0(hydro_type, " - WY", WY) else ~paste0("WY", WY),
        hoverinfo = "text",
        text = ~paste("WY:", WY, "<br>Date:", wDate, "<br>Probability:", round(.data[[y_var]],2)),
        legendgroup = wy
        ) %>% 
    add_ribbons(
      data = data %>% filter(WY == wy),
      ymin = ~.data[[ymin_var]], 
      ymax = ~.data[[ymax_var]], 
      fillcolor = fillcolor, 
      line = list(color = 'rgba(0,0,0,0)'),
      name = ~paste0("WY",wy, ", 80% CI"),
      showlegend = FALSE,
      hoverinfo = "text",
      text = ~paste("WY:", WY, "<br>Date:", wDate, "<br>Lower:", round(.data[[ymin_var]],2), "<br>Upper:", round(.data[[ymax_var]],2)),
      legendgroup = wy
    )
  }
  p <- p %>%
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
        title = list(text = if (hydro == "TRUE") "Hydrologic Year Type with 80% CI" else "Water Year with 80% CI"),
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
        title = "Interactive plot: STARS model - Winter-run Chinook Salmon",
        HTML("Visualize STARS model results 
        (<a href = 'https://cdnsciencepub.com/doi/10.1139/cjfas-2021-0042'>Hance et al. 2022</a>; <a href = 'https://www.cbr.washington.edu/shiny/STARS/'>STARS Shiny app</a>) 
        for Winter-run Chinook Salmon in the current water year compared to past water years.
        Select a specific survival probability below to adjust the plot. Users can also adjust color coding to reflect <a href = 'https://cbr.washington.edu/sacramento/data/query_hci.html'>Hydrologic Classification Index</a> by selecting the `Show Hydrologic Year Type` switch below. 
        To add/remove years from plot, click the water year within the plot legend or select in the drop down menu below." )
      )
    ),
    #original TAC request 
    fluidRow(
      shinydashboard::box(
        width = 12,
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = FALSE,
        title = paste0("Compare Current Water Year (WY", current_year,") to Past Water Years"),
        fluidRow(
          column(
            width = 3,
            shinydashboard::box(
              width = 12,
              solidHeader = FALSE,
            selectInput(
              inputId = "select_metric", 
              label = "Select Probability:", 
              choices = c("Overall Survival" = "surv",
                          "Interior Delta Route-specific Survival Probability" = "idsurv",
                          "Interior Delta Route-specific Probability" = "idRoute",
                          "Compare probabilities" = "compare_all"),
              multiple = FALSE
            ),
            selectInput(
              inputId = "select_year", 
              label = "View years:", 
              choices = c("All years", 2018:current_year),
              selected = "All years"
            ),
            shinyWidgets::materialSwitch(
              inputId = "select_hydro", 
              label = HTML("<b>Show Hydrologic Year Type</b>"), 
              value = FALSE,
              status = "primary"
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
              HTML("All code featured in this Shiny application is made publicly available through our organization's GitHub repository: 
                 <a href='https://github.com/Columbia-Basin-Research-CBR/track-a-cohort'><i class='fab fa-github'></i> Columbia-Basin-Research-CBR</a>"
              )
            )
          )
        ),
          column(
            width = 9,
            uiOutput("plot_caption"),
            uiOutput("plots"),
            uiOutput("datasource")
          )
        )
      )
    ),
    # Select year of comparison
    fluidRow(
      shinydashboard::box(
        width = 12,
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Compare Survival Probabilities in a Specific Water Year",
        fluidRow(
          column(
            width = 3,
            shinydashboard::box(
              width = 12,
              solidHeader = FALSE,
              shinyWidgets::pickerInput(
                inputId = "select_metric_2",
                label = "Select Probability:",
                choices = unique(filtered_dates$route),
                multiple = TRUE,
                selected = c("Overall","Interior Delta", "Sacramento","Steamboat","Sutter", "Yolo"),
                options = list(`live-search` = TRUE)
              ),
              shinyWidgets::pickerInput(
                inputId = "specific_years_2",
                label = "Select Year",
                choices = unique(filtered_dates$WY),
                multiple = FALSE,
                selected = current_year,
                options = list(`live-search` = TRUE)
              ),
              shinyWidgets::materialSwitch(
                inputId = "select_hydro_2", 
                label = HTML("<b>Show Hydrologic Year Type</b>"), 
                value = FALSE,
                status = "primary"
              )
            )
          ),
          column(
            width = 9,
            uiOutput("plot_caption_2"),
            uiOutput("plots_2"),
            uiOutput("datasource_2")
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  output$datasource <- renderUI({
    if (input$select_hydro == "TRUE") {
      HTML("Data source: Delta STARS developed by USGS Quantitative Fisheries Ecology Section and deployed by SacPAS. 
           Reconstructed Water Year Hydrologic Classification courtesy of <a href='https://cdec.water.ca.gov/water_supply.html'>Water Supply Information</a>, CDEC. Reconstructed Year classifications based on measured unimpaired runoff (in million acre-feet), subject to revision.") 
     } else {
        HTML("Data source: Delta STARS developed by USGS Quantitative Fisheries Ecology Section and deployed by SacPAS.")
      }
  })
  
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
                compare_all = "<b>STARS model - Compare Probabilities:</b><br>"
    )
    )
  })
  
  render_plot <- function(metric) {
    if (input$select_year == "All years") {
      fct_stars_current_water_year_plot(data = STARS_data, metric = metric, hydro = as.character(input$select_hydro), hydro_type = STARS_data$hydro_type)
    } else {
      filtered_data <- STARS_data %>% 
        dplyr::filter(WY == input$select_year)
      
      fct_stars_current_water_year_plot(data = filtered_data, metric = metric, hydro = as.character(input$select_hydro), hydro_type = filtered_data$hydro_type)
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