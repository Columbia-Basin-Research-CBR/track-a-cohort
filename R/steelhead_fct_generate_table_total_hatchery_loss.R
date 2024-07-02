#' @title Minimization of Total Loss of Hatchery Steelhead
#' @description This function generates a HTML table summarizing cumulative loss of hatchery steelhead releases and loss. 
#' Included as a function so that the html will be generated in the .qmd file versus trying to save a html and import to .qmd. 
#' @import dplyr
#' @import here
#' @import knitr
#' @importFrom kableExtra kable_styling
#' @import scales
#' @importFrom magrittr %>%
#' 


generate_table_steelhead_total_hatchery_loss <- function(data){
# wrangle for table format
df_tbl1<- data %>% 
  filter(adipose_clip == "Clipped") %>%
  group_by(WY) %>%
  summarise(total_loss = unique(total_loss)) %>%
  ungroup() %>% 
  mutate(nfish_released = NA,
         pct_loss_faciliy = scales::percent((nfish_released/total_loss), accuracy = .01),
         BY = WY-1 ) %>% 
  rename(`Brood Year` = BY, `Total released` = nfish_released, `Water Year` = WY, `Total Loss` = total_loss, `Percent of fish released lost to facilities` = pct_loss_faciliy)

# create table
tb1<-knitr::kable(df_tbl1, caption = "Minimization of Total Loss of Hatchery Steelhead by Water Year", align = "l") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F)


return(tb1)

}

