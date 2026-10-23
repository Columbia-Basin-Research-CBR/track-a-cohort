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

sfs_raw<-data.table::fread(query_url,verbose=FALSE,showProgress=FALSE)

sfs_wrangled<-sfs_raw %>% 
  as_tibble() %>% 
  mutate(date = lubridate::ymd(yyyymmdd),
         value = ifelse(value == "m", NA, value) ) %>% 
  select(sta,value,date) %>% 
  mutate(wyweek = sapply(date, calculateWYWeek),
         value = as.numeric(value))

# precip is summed and not converted to flow
sfs_summed<-sfs_wrangled %>%
  arrange(date) %>% mutate(precip = value)     
sfs_summed <- sfs_summed %>% group_by(wyweek) %>% 
  summarise(
    weekly_precip = if (all(is.na(precip))) 0 else sum(precip, na.rm = TRUE),
    .groups = "drop"  ) %>% 
  select(wyweek, weekly_precip)

return(sfs_summed)
}

#example
# fct_import_CDEC_precipitation_data(station = "SFS", sensor_num = "45", dur_code ="d", start_date = "2025-03-09", end_date = "2025-04-09")
