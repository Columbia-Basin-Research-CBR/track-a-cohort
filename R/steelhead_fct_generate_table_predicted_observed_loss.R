#' @title Table 2. Predicted vs Observed Steelhead Loss 
#' @description This function generates a HTML table summarizing average flow at Sacramento and San Joaquin, with average export and OMR. 
#' Table includes predicted (unsure of how calculated) and observed steelhead loss. Seems to be summarized on a weekly basis for current water year.  
#' Included as a function so that the html will be generated in the .qmd file versus trying to save a html and import to .qmd. 
#' @import dplyr
#' @import here
#' @import knitr
#' @importFrom kableExtra kable_styling
#' @import scales
#' @importFrom magrittr %>%
#' 


generate_table_steelhead_pred_obs_loss <- function(data){

  
# wrangle for table format
df_tbl<- data %>% 
  filter(!is.na(date)) %>% 
  arrange(date) %>% 
  mutate( `Total Daily Loss` = sum(daily_loss_non_clipped_steelhead),
          `Predicted Loss` = NA, # unsure of how this is calculated
    across(-c(date, daily_loss_non_clipped_steelhead) , ~zoo::rollapply(.x, 7, mean, fill = NA, align = "left", partial = TRUE))) %>% 
  filter(row_number() %% 7 == 0) %>%  # This selects every 7th row; assumes start date is start of 7-day period
  select("Date" = date, 
         `Total Daily Loss`,
         `Average Export (TAF)` = exports_taf,
         `Average OMRI (cfs)` = omr_index_cfs,
         `Average OMR (cfs); 5d USGS tidally filtered` = usgs_tidally_filtered_omr_5_day_mean_cfs
        ) 

# create table
tbl<-knitr::kable(df_tbl, caption = "Predicted and Observed Total Loss of Hatchery Steelhead with weekly OMR flow and export", align = "l") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F)


return(tbl)

}

