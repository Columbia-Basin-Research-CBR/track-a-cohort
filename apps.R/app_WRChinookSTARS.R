require(shiny)
require(shinydashboard)
require(shinydashboardPlus)
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
  #filter data for specific plot type
  current_WY_data <- STARS_data %>% 
    dplyr::filter(use_case == "STARS_plot_type_1") #include data for this plot not specific years data
  
  # plot code for year-specific data
  year_specific_data <- STARS_data %>% 
    dplyr::filter(use_case == "STARS_plot_type_2") #include data for this plot not current water year plot
  
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

fct_stars_specific_water_year_plot <- function(data, selected_routes, selected_year){
  
  color_palette <- c("Overall" = "black", "Yolo" = "lightblue", "Sutter" = "blue", "Steamboat" = "purple", "Sacramento" = "darkgrey", "Interior Delta" = "orange")
  
  # Filter the data
  filtered_data <- data %>% 
    dplyr::filter(route %in% selected_routes, WY == selected_year)
  
  # Create the plot
  survival_plot <- plot_ly(filtered_data, x = ~date) 
  
  for (route in unique(filtered_data$route)) {
    route_data <- filtered_data %>% dplyr::filter(route == !!route)
    
    survival_plot <- survival_plot %>%
      add_ribbons(
        data = route_data,
        ymin = ~lowerCI,
        ymax = ~upperCI,
        fillcolor = scales::alpha(color_palette[route], .15),  # Add transparency to 10%
        line = list(color = 'rgba(0,0,0,0)'),
        name = paste0(route, " 80% CI"),
        showlegend = FALSE,
        hoverinfo = "text",
        text = ~paste("Route-specific Survival:", route, "<br>Date:", date, "<br>Lower 80%:", round(lowerCI, 2), "<br>Upper 80%:", round(upperCI, 2))
      ) %>%
      add_lines(
        data = route_data,
        y = ~median,
        line = list(color = color_palette[route], width = 1),
        name = route,
        hoverinfo = "text",
        text = ~paste("Route-specific Survival:", route, "<br>Date:", date, "<br>Median:", round(median, 2))
      )
  }
  
  survival_plot <- survival_plot %>%
    layout(
      title = list(
        # text = paste0("WY", selected_year, " Route-specific Survival:\nMedian survival of daily cohorts of winter run Chinook Salmon through the Delta (Knights Landing to Chipps Island)"),
        x = 0.5
      ),
      xaxis = list(
        title = "Month"
      ),
      yaxis = list(
        title = "Apparent survival probability",
        rangemode = "tozero"
      ),
      legend = list(
        title = list(text = "Route-specific survival,\nmedian with 80% CI")
      ),
      margin = list(l = 50, r = 50, t = 50, b = 100),
      paper_bgcolor = 'white',
      plot_bgcolor = 'white'
    )
  
  return(survival_plot)
}

ui <- shinydashboardPlus::dashboardPage(
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
        (<a href = 'https://cdnsciencepub.com/doi/10.1139/cjfas-2021-0042'  target='_blank' >Hance et al. 2022</a>; <a href = 'https://www.cbr.washington.edu/shiny/STARS/' target='_blank'>STARS Shiny app</a>) 
        for Winter-run Chinook Salmon in the current water year compared to past water years or select a specific year to compare all available survival probabilities.
        Select a specific survival probability below to adjust the plots.
        To add/remove selections from plot, click variables within the plot legend or adjust selections in the drop down menus." )
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
        title = paste0("Compare Current Water Year (WY", current_year,") Survival and Routing Probabilities to Past Water Years"),
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
              label = HTML("Adjust color to reflect <a href = 'https://cbr.washington.edu/sacramento/data/query_hci.html' target='_blank' style= 'color: #5E7880;'>Hydrologic Classification Index</a>:"),
              value = FALSE,
              status = "primary"
            ),
           
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
                choices = unique(year_specific_data$route),
                multiple = TRUE,
                selected = c("Overall","Interior Delta", "Sacramento","Steamboat","Sutter", "Yolo"),
                options = list(`live-search` = TRUE)
              ),
              shinyWidgets::pickerInput(
                inputId = "specific_years_2",
                label = "Select Year",
                choices = unique(year_specific_data$WY),
                multiple = FALSE,
                selected = current_year,
                options = list(`live-search` = TRUE)
              )
            )
          ),
          column(
            width = 9,
            uiOutput("plot_caption_2"),
            plotly::plotlyOutput("plot_year_specific"),
            HTML("Data source: Delta STARS developed by USGS Quantitative Fisheries Ecology Section and deployed by SacPAS.")
          )
        )
      )
    )
  ), 
  ## Footer content
  footer = shinydashboardPlus::dashboardFooter(
    left = HTML(
      '<div style="color: #5E7880;">
       <a href="https://cbr.washington.edu/" target="_blank" style="color: #5E7880; text-decoration: none;" onmouseover="this.style.textDecoration=\'underline\';" onmouseout="this.style.textDecoration=\'none\';">Columbia Basin Research</a> •
       <a href="https://fish.uw.edu/" target="_blank" style="color: #5E7880; text-decoration: none;" onmouseover="this.style.textDecoration=\'underline\';" onmouseout="this.style.textDecoration=\'none\';">School of Aquatic and Fishery Sciences</a> •
       <a href="https://environment.uw.edu/" target="_blank" style="color: #5E7880; text-decoration: none;" onmouseover="this.style.textDecoration=\'underline\';" onmouseout="this.style.textDecoration=\'none\';">College of the Environment</a> •
       <a href="https://www.washington.edu/" target="_blank" style="color: #5E7880; text-decoration: none;" onmouseover="this.style.textDecoration=\'underline\';" onmouseout="this.style.textDecoration=\'none\';">University of Washington</a>
     </div>'
    ),
    right = HTML(
      '<span class="footer-contact" style="color: #5E7880;">
       <a href="mailto:web@cbr.washington.edu" target="_blank" style="color: #5E7880; text-decoration: none;" onmouseover="this.style.textDecoration=\'underline\';" onmouseout="this.style.textDecoration=\'none\';">
         <i class="fa fa-envelope" style="color: #5E7880;"></i> web@cbr.washington.edu
       </a>
       &nbsp;&nbsp;
       <a href="https://github.com/Columbia-Basin-Research-CBR/track-a-cohort.git"
          target="_blank" style="color: #5E7880; text-decoration: none;" onmouseover="this.style.textDecoration=\'underline\';" onmouseout="this.style.textDecoration=\'none\';">
         <i class="fa fa-github" style="color: #5E7880;"></i> github.com/Columbia-Basin-Research-CBR
       </a>
     </span>'
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
        HTML("<b>Overall Survival:</b><br>Solid lines show median survival of daily cohorts of winter run Chinook Salmon through the Delta (Knights Landing to Chipps Island) for all routes combined."),
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
                surv = "<b>STARS model - Overall Survival:</b><br>Solid lines show median survival of daily cohorts of winter run Chinook Salmon through the Delta (Knights Landing to Chipps Island) for all routes combined.",
                idsurv = "<b>STARS model -Interior Delta Route-specific Survival Probability:</b><br> Route-specific survival of daily cohorts of winter run Chinook Salmon through the Delta (Knights Landing to Chipps Island).",
                idRoute = "<b>STARS model - Interior Delta Route-specific Probability:</b> <br>Proportion of daily cohorts of winter run Chinook Salmon through the Delta (Knights Landing to Chipps Island) using the Interior Delta route.",
                compare_all = "<b>STARS model - Compare Probabilities:</b><br>"
    )
    )
  })
  
  output$plot_caption_2 <- renderUI({
    HTML(paste0("<b>STARS model - WY", input$specific_years_2, " Route-specific Survival:</b><br> Median survival of daily cohorts of winter run Chinook Salmon through the Delta (Knights Landing to Chipps Island) by route."))
  })
  
  #plot code for current water year
  render_plot <- function(metric) {

    if (input$select_year == "All years") {
      fct_stars_current_water_year_plot(data = current_WY_data, metric = metric, hydro = as.character(input$select_hydro), hydro_type = current_WY_data$hydro_type)
    } else {
      filtered_data <- current_WY_data %>% 
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
  
  # Render the year-specific plot
  output$plot_year_specific <- plotly::renderPlotly({
    fct_stars_specific_water_year_plot(
      data = year_specific_data, 
      selected_routes = input$select_metric_2, 
      selected_year = input$specific_years_2
    )
  })
  
  
}

shinyApp(ui, server)