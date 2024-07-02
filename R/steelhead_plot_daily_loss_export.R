#' @title Figure 6. Steelhead Daily Loss and Export (per Facility pending)
#' @description This script is more of a holder space for plot that will include export per facility and loss per facility for steelhead
#' Current data shows total daily loss and export for both facilities
#' @import dplyr
#' @import here
#' @import ggplot2
#' @importFrom magrittr %>%


# Load the data
load(here::here("data/steelhead_loss_export_data.rda"))

p <- steelhead_loss_export_data %>% 
  ggplot( aes(x = date)) +
  geom_bar(aes(y = daily_loss_non_clipped_steelhead ), stat = "identity", color = "grey80", fill = "grey90") +
  geom_line(aes(y = exports_taf)) +
  labs(title = "Steelhead Daily Loss and Export (per Facility pending)",
       x = "Date",
       y = "Fish Count") +
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%d-%Y") +
  theme_minimal() + 
  theme(text = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 1))

print(p)

