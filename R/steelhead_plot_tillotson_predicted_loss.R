#' @title Prediction of Weekly Losses of Steelhead
#' @description Plots the predicted weekly losses of Steelhead salmon in the Sacramento River from the Tillotson et al. (2022) model
require(tidyverse)
require(here)

# load data files
load(here("data", "tillotson_prediction_output.rda"))
steelhead_tillotson_output <- tillotson_prediction_output$steelhead_tillotson_output

#set current year
source(here("R/utils_fct_assign_current_water_year.R"))
current_year <- assign_current_water_year()

# back calculate date from water week with the start of the water year = 10-01
start_date <- as.Date(paste(current_year - 1, "-10-01", sep=""))

# Calculate the date range for plotting
min_date <- start_date + (min(steelhead_tillotson_output$week)-1)*7
max_date <- start_date + (max(steelhead_tillotson_output$week)-1)*7

# Adjust the start limit to the beginning of the first date's month
start_limit <- floor_date(min_date, "month")

# Adjust the end limit to the end of the last date's month
end_limit <- ceiling_date(max_date, "month")

# plot
p <- steelhead_tillotson_output %>% 
  mutate(date = start_date + (week-1)*7) %>%  #returns the monday of the start_date week
  ggplot(aes(x = date)) +
  geom_point(aes(y = ObservedLoss, shape = "Observed Weekly Loss"), color = "black") +
  geom_ribbon(aes(ymin = lowerCI, ymax = upperCI, fill = "Predicted loss,\nmedian with 90% CI"), alpha = 0.3) +
  geom_line(aes(y = median, color = "Predicted loss,\nmedian with 90% CI")) +
  geom_point(aes(y = median, color = "Predicted loss,\nmedian with 90% CI")) +
  labs(title = "Predicted Weekly Loss - Tillotson et al. (2022)",
       subtitle = paste("Species: Unclipped Steelhead\nWater Year:", current_year),
       x = "Week",
       y = "Predicted Weekly Loss", 
       fill = NULL,
       color = NULL, 
       shape = NULL) +
  scale_shape_manual(values = c("Observed Weekly Loss" = 21),
                     breaks = c("Observed Weekly Loss")) +
  scale_color_manual(values = c("Predicted loss,\nmedian with 90% CI" = "#0072B2"),
                     breaks = c("Predicted loss,\nmedian with 90% CI", "Observed Weekly Loss")) +
  scale_fill_manual(values = c("Predicted loss,\nmedian with 90% CI" = "#0072B2"),
                    breaks = c("Predicted loss,\nmedian with 90% CI")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = c(start_limit, end_limit), expand = c(0,0)) +  guides(fill = guide_legend(order = 1),
         color = guide_legend(order = 1)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        panel.grid.major = element_line(linetype = "dotted"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
  )

print(p)
