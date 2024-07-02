#' @title Minimization of Cumulative Loss of Hatchery Steelhead
#' @description This table summarizes cumulative loss of hatchery steelhead releases and loss.
#' @import dplyr
#' @import here
#' @import knitr
#' @import kableExtra
#' @import scales
#' @importFrom magrittr %>%
#' 


# Load the data
load(here("data/steelhead_loss_data.rda"))

# wrangle for table format
df_tbl1<- steelhead_loss_data %>% 
  filter(adipose_clip == "Clipped") %>%
  group_by(WY) %>%
  summarise(total_loss = unique(total_loss)) %>%
  ungroup() %>% 
  mutate(nfish_released = NA,
         pct_loss_faciliy = scales::percent((nfish_released/total_loss), accuracy = .01),
         BY = WY-1 ) %>% 
  rename(`Brood Year` = BY, `Total released` = nfish_released, `Water Year` = WY, `Total Loss` = total_loss, `Percent of fish released lost to facilities` = pct_loss_faciliy)

# create table
tb1<-knitr::kable(df_tbl1, caption = "Minimization of Cumulative Loss of Hatchery Steelhead by Water Year") %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) 


tb1

