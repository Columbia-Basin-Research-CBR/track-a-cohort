#' @title import steelhead loss and export data from SacPas
#' @description This script is more of a holder space for future data query url that will include export per facility and loss per facility to be used in the steelhead Figure 6 barplot and linegraph.
#' Current url download requires year, spp, and year type selection to then download a csv with total daily loss and export for both facilities
#' @import dplyr
#' @import lubridate
#' @import janitor
#' @import usethis
#' @importFrom magrittr %>%

url <- "https://www.cbr.washington.edu/sacramento/tmp/juvloss_1719955580_301.csv"


# Read the data
steelhead_loss_export_data_raw<- read.csv(url, header = TRUE, sep = ",", stringsAsFactors = FALSE)


steelhead_loss_export_data <- steelhead_loss_export_data_raw %>% 
  janitor::clean_names() %>% 
  mutate(date = lubridate::ymd(date)
         )


# Save the data
usethis::use_data(steelhead_loss_export_data, overwrite = TRUE)
