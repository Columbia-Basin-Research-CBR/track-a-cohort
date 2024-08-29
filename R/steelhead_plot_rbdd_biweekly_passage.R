#' @title RBDD Juvenile Passage Estimates Steelhead
#' @description This figure shows the daily and biweekly passage estimates for steelhead at Red Bluff Diversion Dam.
require(tidyverse)
require(here)


# load data file
load(here("data/rbdd_biweekly_passage_data.rda"))

#set current year
source(here("R/utils_fct_assign_current_water_year.R"))
current_year <- assign_current_water_year()

# Get the current timestamp
timestamp <- format(Sys.time(), "%d %b %Y %H:%M:%S %Z")

# Create a data frame of biweekly periods

# # update with SI method when available--likely in the data-raw section
# biweek_periods <- data.frame(
#   start_date = seq(min(rbdd_biweekly_passage_data$date), max(rbdd_biweekly_passage_data$date), by = "2 weeks")
# )
# biweek_periods$end_date <- c(biweek_periods$start_date[-1] - 1, max(rbdd_biweekly_passage_data$date))


# Plot the data

p <- rbdd_biweekly_passage_data %>%
  ggplot(aes(x = date)) +
  # geom_rect(data = biweek_periods, aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf), fill = "grey80",alpha =.1, inherit.aes = FALSE) +
  geom_line(aes(y = estimated_passage, color = "Daily estimate"), shape = 21) +
  geom_point(aes(y = estimated_passage, color = "Daily estimate"), shape = 21) +
  geom_pointrange(aes(y = biweek_total / 7.5, ymin = biweek_lower_ci / 7.5, ymax = biweek_upper_ci / 7.5, color = "Biweekly with 90% CI"), shape = 17, linewidth = .5) +
  scale_y_continuous("Estimated Daily Passage\n(Fish/Day)",
                     sec.axis = sec_axis(trans = ~ . * 7.5, name = "Estimated Biweekly Total\n(Fish/Biweek)"),
                     expand = c(0,0)) +
  scale_color_manual(values = c("Daily estimate" = "steelblue4", "Biweekly with 90% CI" = "purple")) +
  labs(color = NULL, 
       x = "Date", 
       title = paste0("Red Bluff Juvenile Passage Estimate BY", current_year, " Steelhead"), 
       subtitle = paste0(min(rbdd_biweekly_passage_data$date)," to ",max(rbdd_biweekly_passage_data$date)),
       caption = timestamp) + 
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
