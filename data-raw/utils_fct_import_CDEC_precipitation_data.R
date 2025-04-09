#' @title uils_fct_import_precipitation_data
#' @description Import precipitation data from CDEC for SFS: includes daily precipitation data (incremental inches) for SFS site. Once imported, this function will also calculate the 5-day rolling sum of precipitation and convert it to cfs using a constant value provided by CBR to convert inches to cfs at SFS station. This method is in place of using DAYFLOW a annual retrospective value used in Tillotson et al. 2022.
#' @param station character vector of site codes to import data from: "SFS" is default
#' @param sensor_num numeric vector of sensor numbers to import data from: 45 returns incremental and 2 returns cumulative sum across days
#' @param dur_code character vector of duration codes to import data from: "d" for daily
#' @param start_date character vector of start date in "YYYY-MM-DD" format
#' @param end_date character vector of end date in "YYYY-MM-DD" format
#' @return  combined data frame for precipitation data queried for the metrics, years and sites specified

require(tidyverse)
require(data.table)

# library(CDECRetrieve) #alternative (quicker) process but package does not appear to be maintained regularly
# x<-cdec_query(station = "SFS",dur_code = "D", start_date = "2025-03-09", end_date =  "2025-04-09", sensor = 45)

fct_import_CDEC_precipitation_data <- function(station = "SFS", sensor_num = "45", dur_code ="d", start_date, end_date){
query_url <- sprintf(
  "http://cdec.water.ca.gov/cgi-progs/querySHEF?station_id=%s&sensor_num=%s&dur_code=%s&start_date=%s&end_date=%s&data_wish=Download+SHEF+Data+Now",
  station, sensor_num, dur_code, start_date, end_date
)

sfs_raw<-data.table::fread(query_url)

sfs_wrangled<-sfs_raw %>% 
  as_tibble() %>% 
  mutate(date = lubridate::ymd(yyyymmdd),
         value = ifelse(value == "m", NA, value) ) %>% 
  select(sta,value,date) %>% 
  mutate(wyweek = sapply(date, calculateWYWeek),
         value = as.numeric(value))

sfs_summed<-sfs_wrangled %>% 
  arrange(date) %>% 
  mutate(precip_5day_total = rollapply(value, width = 5, align = "right", fill = NA, FUN = sum),
         precip_cfs = precip_5day_total / 0.00017448) %>%  #constant provided by CBR to convert inches to cfs at SFS station instead of using DAYFLOW a retrospective value used in Tillotson et al. 2022
group_by(wyweek) %>% 
  summarise(
    weekly_5dsum_sfs_precip = if (all(is.na(precip_cfs))) 0 else max(precip_cfs, na.rm = TRUE),
    .groups = "drop",
    weekly_5dsum_sfs_precip_cfs = weekly_5dsum_sfs_precip/1000
  ) %>% 
  select(wyweek, weekly_5dsum_sfs_precip_cfs)

return(sfs_summed)
}

#example
# fct_import_CDEC_precipitation_data(station = "SFS", sensor_num = "45", dur_code ="d", start_date = "2025-03-09", end_date = "2025-04-09")
