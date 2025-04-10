#' @title Table 2. Predicted vs Observed Steelhead Loss 
#' @description This function generates a HTML table summarizing the inputs 
#' and results from Tillotson weekly predicted loss for Steelhead. 
#' Incluedes:  average flow at Sacramento and San Joaquin, with average export and OMR. Also includes,
#'  predicted and observed steelhead loss.   
#' Included as a function so that the html will be generated in the .qmd file versus trying to save a html and import to .qmd. 
require(tidyverse)
require(here)
require(knitr)
require(kableExtra)
require(scales)



generate_table_pred_obs_loss <- function(data, species){
  
  # # Function to calculate the start date of the Water Year
  # startOfWY <- function(date) {
  #   if (month(date) >= 10) {
  #     ymd(paste(year(date), "10-01", sep = "-"))
  #   } else {
  #     ymd(paste(year(date) - 1, "10-01", sep = "-"))
  #   }
  # }
  # 
  # # Function to calculate the start date of a given week in the Water Year
  # weekStartDate <- function(weekNumber, currentDate) {
  #   waterYearStart <- startOfWY(currentDate)
  #   startDateOfWeek <- waterYearStart + days((weekNumber - 1) * 7)
  #   formattedDate <- format(startDateOfWeek, "%m-%d-%y")
  #   return(formattedDate)
  # }
  # 
  
# wrangle for table format
df_tbl<- data %>% 
  mutate( "Predicted loss" =paste0("(",round(median, 2), ", ",round(lowerCI, 2), ", ", round(upperCI, 2), ")")) %>%
  mutate(across(-c(week, calendar_date, ObservedLoss,`Predicted loss`), ~round(., 2))) %>% 
  select(#`Water Year` = WY, 
         `Water year week` = week,
          `Date` = calendar_date,
         `Observed loss` = ObservedLoss, 
         `OMR USGS tidally filtered` = OMR,
         `Export, SWP & CVP (CFS)` = Export,
         `Avg flow at Sacramento (CFS)` = weekly_avg_fpt_flow,
         `Avg flow at San Joaquin (CFS)` = weekly_avg_vns_flow,
         `Avg water temperature at Mallard Island (°C)` = weekly_avg_mal_wtemp,
         `Weekly predicted loss (median, lower CI, upper CI)` = `Predicted loss`
         )

#remove index row names
row.names(df_tbl) <- NULL

# create table
tbl<-knitr::kable(df_tbl, caption = paste("Table of Tillotson et al., (2022) model output of predicted weekly losses for Natural-origin", species, "with model weekly average inputs including 
                  observed loss, Old and Middle Rivers (OMR), USGS tidally filtered flow (CFS), combined exports from CVP and SWP facilities (CFS),  flow at Sacramento and San Joaquin (CFS), and water temperature at Mallard Island (°C)."), align = "l") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>% 
  kableExtra::add_header_above(c(" " = ncol(df_tbl)), align = "l")


return(tbl)

}


