#' @title Import Water Year Hydrologic Classification Indices (HCI) from SacPAS Query
#' @description This script imports Water Year Hydrologic Classification Indices (HCI) using the SacPAS website, https://cbr.washington.edu/sacramento/data/query_hci.html. 
#' Data Courtesy of Water Supply Information, CDEC
#' Currently uses `Reconstructed Year` and not `Official Year` based on BOR supplied TAC code. 
#' @return .rda of HCI assignments saved in `data` folder.
require(tidyverse)
require(janitor)
require(usethis)
require(here)


# load in data from url 
url <- "https://cbr.washington.edu/sacramento/data/php/rpt/hci.php?sc=1&outputFormat=csv&classification=Reconstructed"


hydrological_classification_index <- read.csv(url, header = TRUE, stringsAsFactors = FALSE) %>% 
  filter(Basin == "Sacramento Valley") %>%
  mutate(WY = as.numeric(WY))

hydrological_classification_index


