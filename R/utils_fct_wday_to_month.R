#' Function to convert wDay to month name
#reference for how wday was calculated
# wDay = if_else(month(Date) >= 10, yday(Date) - 273, yday(Date) + 92)

# Function to convert wDay to month name
wDay_to_month <- function(wDay) {
  # Adjust the wDay for the water year
  adjusted_wDay <- (wDay + 273) %% 365 + 1
  # Convert the adjusted wDay to a date
  date <- as.Date(paste(2000, adjusted_wDay), format = "%Y %j")
  # Return the month name
  return(format(date, "%b"))
}
