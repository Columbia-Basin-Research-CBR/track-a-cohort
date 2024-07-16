#' @title Current and Historical Total Loss of Clipped and Non-clipped Steelhead
#' @description This plot shows the total loss of clipped and non-clipped steelhead by water year.
#' @import ggplot2
#' @import scales
#' @import dplyr
#' @import here
#' @importFrom magrittr %>%
#' 

# load data

load(here::here("data/steelhead_loss_data.rda"))

p <- steelhead_loss_data %>% 
  ggplot( aes(x = as.factor(WY), y = total_loss, fill = adipose_clip)) + 
  geom_bar(stat = "identity", position = "dodge", width = .8) + 
  labs(title = "Total Loss of Clipped and Unclipped Steelhead by Water Year",
       x = "Water Year\n(Oct-Dec of year [t-1], Jan-Sep of year [t])",
       y = "Total Loss", 
       fill = NULL) +
  scale_fill_manual(values = c("grey30", "grey80")) +
  scale_y_continuous(labels = scales::comma, expand = c(0,0), limits = c(0, 25000)) +
  theme_minimal() +
  theme(text = element_text(size = 15),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = .5),
        panel.border= element_rect(color = "grey80", fill = NA), 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(), axis.ticks = element_line(size = .25, color = "grey80"))

print(p)
