#' import genetic and LAD loss data (shared by BOR) 
#' @description genetic data is not QA/QC'd prior to 2020 (data source: BOR). This script should be updated once we can source this genetic and lad data.
#' 
#'  @return writes a .rda to the data folder including wrangled annual jpe estimate data 


#import shared genetic data pre-2020 (not QA/QC'd) provided by BOR
JPE_Genetic_Loss_Comparison<- read_csv(here::here('data-raw/JPE_Genetic_Loss_Comparison.csv'))

jpe_genetic_lad_data <- JPE_Genetic_Loss_Comparison %>% 
  select(WaterYear,BroodYear,JuvenileProductionEstimate,"pct_gen" = PercentJPE_genetic,"pct_lad" = PercentJPE_LAD, "count_lad" = WinterRun_LAD_Loss, "count_gen" = WinterRun_Genetic_Loss) %>%   
  pivot_longer(cols = c(pct_lad, pct_gen, count_lad, count_gen),
               names_to = "method",
               values_to = "value")

#write to data folder
usethis::use_data(jpe_genetic_lad_data)

