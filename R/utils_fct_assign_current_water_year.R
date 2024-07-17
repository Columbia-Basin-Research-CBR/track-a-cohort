#' function to assign current water year based on today's date
#' 
#' @return water year in numeric format
require(tidyverse)



assign_current_water_year <- function() {
  # Get today's date
  today <- Sys.Date()
  
  # Extract the calendar year
  current_year <- year(today)
  
  # Determine the water year
  # If today's date is before October 1st, the water year is the current calendar year
  # If today's date is on or after October 1st, the water year is the next calendar year
  water_year <- ifelse(month(today) > 10 || (month(today) == 10 && day(today) >= 1), current_year + 1, current_year)
  
  return(water_year)
}
