#' @title Current and Historical Total Loss of Clipped and Non-clipped Steelhead
#' @description This plot shows the total loss of clipped and non-clipped steelhead by water year.
require(tidyverse)
require(scales)
require(here)


# load data

load(here::here("data/steelhead_loss_data.rda"))

p <- steelhead_loss_data %>% 
  ggplot( aes(x = as.factor(WY), y = total_loss, fill = adipose_clip)) + 
  geom_bar(stat = "identity", position = "dodge", width = .7) + 
  labs(title = "Total Loss of Clipped and Unclipped Steelhead",
       x = "Water Year",
       y = "Total Loss", 
       fill = NULL,
       caption = "Data sources: Preliminary data from CDFW; subject to revision.") +
  scale_fill_manual(values = c("#0072B2", "#F5C767")) +
  scale_y_continuous(labels = scales::comma, expand = c(0,0), limits = c(0, 25000)) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1), 
        text = element_text(size = 15),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank()
  )

print(p)
