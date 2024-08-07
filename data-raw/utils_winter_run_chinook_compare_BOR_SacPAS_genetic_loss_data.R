#'  @title Comparing genetic loss data from BOR and SacPAS
#'  @description supporting script for reference until totally switched over to using SacPAS generated genetic loss link. 
#'  Compares datasets and shows which datasets have same loss values per date. 

require(tidyverse)
require(usethis)
require(here)

# import non QA/QC'd genetic loss data shared via BOR flat file
df_genetic_loss_bor <- read.csv(here::here("data-raw/WRGenetic.csv")) %>%
  mutate(date = mdy(SampleDateTime)) %>%
  rename(loss = "Loss_GeneticData") %>%
  arrange(date) %>%
  mutate(
    WY = year(date) + (month(date) >= 10),
    wDay = if_else(month(date) >= 10, yday(date) - 273, yday(date) + 92),
    doy = yday(date),
    CY = year(date),
    wDate = if_else(month(date) >= 10, date + years(1), date)
  )

#import genetic loss data QA/QC'd via SacPAS link

url <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=all&species=1%3Af&dnaOnly=yes&age=no"

# filter to include only winter-run chinook for winter-run for DNA
loss.wch <- read.csv(url) %>%
  filter(DNA.Race == "Winter") 

df_genetic_loss_sacpas <- loss.wch %>%
  janitor::clean_names() %>%
  select(sample_time, facility, dna_race, nfish, loss, length) %>%
  mutate(date = as_date(ymd_hms(sample_time))) %>%
  arrange(date) %>%
  mutate(
    WY = year(date) + (month(date) >= 10),
    wDay = if_else(month(date) >= 10, yday(date) - 273, yday(date) + 92),
    doy = yday(date),
    CY = year(date),
    wDate = if_else(month(date) >= 10, date + years(1), date)
  )


# Merge datasets
merged_data <- full_join(df_genetic_loss_bor, df_genetic_loss_sacpas, by = c("date", "WY", "wDay", "doy", "CY", "wDate"), suffix = c("_bor", "_sacpas"))

# Flag overlapping records
merged_data <- merged_data %>%
  mutate(overlap = if_else(!is.na(loss_bor) & !is.na(loss_sacpas), "Both", 
                           if_else(!is.na(loss_bor), "BOR Only", "SacPAS Only")))

# View the merged data with overlap flag
print(merged_data)