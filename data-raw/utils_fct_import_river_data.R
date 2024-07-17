#' @title Function to import river conditions data from SacPAS
#' @description Import river conditions data from SacPAS River Conditions Data: includes Freeport (FPT) & Vernalis (VNS) river flow (cfs), OMR USGS tidally filtered flow, and Export (pumping discharge flow, cfs) at Tracy (TRP) and Harvey O. Banks (HRO) sites

require(data.table)
require(tidyverse)


fct_import_SacPAS_river_conditions_query <- function(sites, years, metrics) {
  
  # # Create the output folder if it doesn't exist
  # if (!dir.exists(output_folder)) {
  #   dir.create(output_folder)
  # }
  
  # Create an empty list to store wrangled data
  wrangled_data_list <- list()
  
  for (site in sites) {
    # tryCatch({
      # Generate URL with the specified code, years, and metrics: uses Download CSV Only [single data pt/row] 
      url <- paste0("https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csvSingle&hafilter=All&",
                    paste0("year%5B%5D=", paste(years, collapse = "&year%5B%5D=")),
                    "&loc%5B%5D=", paste(site, collapse = "&loc%5B%5D="),
                    "&data%5B%5D=", paste(metrics, collapse = "&data%5B%5D="),
                    "&tempUnit=C&startdate=1%2F1&enddate=12%2F31&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=medium")
      
      # Read data using fread
      raw_data <- data.table::fread(url)
      
      # wrangle data
      wrangled_data <- raw_data %>%
        filter(!is.na(value)) %>% # exclude rows where value is NA
        separate("mm-dd", c("mm", "dd")) %>%
        mutate(
          year = as.numeric(year),
          mm = as.numeric(mm),
          dd = as.numeric(dd)
        ) %>%
        mutate(YMD = lubridate::ymd(paste(year, mm, dd, sep = "-"))) %>%
        mutate(DOY = lubridate::yday(YMD))
      
      # Check if wrangled data is empty
      if (nrow(wrangled_data) == 0) {
        # cat("No data found for site code", site, "\n")
        next  # Skip to the next site code
      }
      
      # Save wrangled data to the list
      wrangled_data_list[[site]] <- wrangled_data
      
      # # Save raw data to a CSV file in the designated folder
      # file_name <- file.path(output_folder, paste0(site, "_raw_data.csv"))
      # write.csv(raw_data, file_name, row.names = FALSE)
      
    #   # output message for successful download
    #   cat("Raw data for", site, "downloaded and saved", "\n")
    # }, error = function(e) {
    #   # output message for Handle errors (e.g., URL not found)--denote which site did not run
    #   cat("Error occurred for site code", site, ":", conditionMessage(e), "\n")
    # })
  }
  
  # Bind all wrangled dataframes together
  combined_data <- bind_rows(wrangled_data_list)
  
  # # save combined file 
  # write.csv(combined_data, file = here::here("data-raw", "SacPAS_river_covariates_combined_sites.csv"), row.names = FALSE)
  
  return(combined_data)
}


  
  

  
  
