#' @title Figure 7. Prediction of Weekly Losses of Winter-run Chinook 
#' @description Plots the predicted weekly losses of winter-run Chinook salmon in the Sacramento River from the Tillotson et al. (2022) model
#' @import ggplot2
#' @import dplyr
#' @import here
#' @importFrom magrittr %>%

# load data files
source(here("data-raw/utils_fct_predict_tillotson_model.R"))
load(here("data", "tillotson_prediction_output.rda"))
winter_run_tillotson_output <- tillotson_prediction_output$winter_run_tillotson_output

#set current year
source(here("R/utils_fct_assign_current_water_year.R"))
current_year <- assign_current_water_year()

# back calculate date from water week with the start of the water year = 10-01
start_date <- as.Date(paste(current_year - 1, "-10-01", sep=""))

# plot
p <- winter_run_tillotson_output %>% 
  mutate(date = start_date + (week-1)*7) %>%  #returns the monday of the start_date week
  ggplot(aes(x = date)) +
  geom_point(aes(y = ObservedLoss, shape = "Observed Weekly Loss"), color = "black") +
  geom_ribbon(aes(ymin = lowerCI, ymax = upperCI, fill = "Predicted loss,\nmedian with 90% CI"), alpha = 0.3) +
  geom_line(aes(y = median, color = "Predicted loss,\nmedian with 90% CI")) +
  geom_point(aes(y = median, color = "Predicted loss,\nmedian with 90% CI")) +
  labs(title = "Predicted Weekly Losses - Tillotson et al. (2022)",
       subtitle = paste("Species: Winter-run Chinook\nWater Year:", current_year),
       x = "Week",
       y = "Predicted Weekly Loss",
       fill = NULL,
       color = NULL, 
       shape = NULL) +
  scale_shape_manual(values = c("Observed Weekly Loss" = 21),
                     breaks = c("Observed Weekly Loss")) +
  scale_color_manual(values = c("Predicted loss,\nmedian with 90% CI" = "steelblue"),
                     breaks = c("Predicted loss,\nmedian with 90% CI", "Observed Weekly Loss")) +
  scale_fill_manual(values = c("Predicted loss,\nmedian with 90% CI" = "steelblue"),
                    breaks = c("Predicted loss,\nmedian with 90% CI")) +
  guides(fill = guide_legend(order = 1),
         color = guide_legend(order = 1)) +
  theme_minimal() +
  theme(text = element_text(size = 15)
  )

print(p)
