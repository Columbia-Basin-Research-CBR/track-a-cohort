#' @title Size Distribution of Clipped and Non-clipped Steelhead Loss By Year
#' @description This plot shows the size distribution by year of the clipped and non-clipped steelhead loss.
require(tidyverse)
require(scales)
require(here)
require(ggridges)


# load data
load(here::here("data/steelhead_loss_data.rda"))


#plot
p <- steelhead_loss_data %>% 
  filter(length < 750) %>%  #set limit for length -- why 750 seems big?
  # filter(WY > 2004) %>% #why filtering WY after 2004?
  ggplot(aes(x = length, y = factor(WY))) +
  ggridges::geom_density_ridges( aes(fill = adipose_clip, color = adipose_clip), draw_baseline = FALSE, scale = 1.5 ) +
  labs(x = 'Fork Length (mm)', 
       y = 'Water Year', 
       subtitle = "Historical Size Distribution of Clipped and Unclipped Steelhead Loss", 
       caption = "Data sources: Preliminary data from CDFW; subject to revision.",
       fill = NULL, color = NULL) +
  scale_fill_manual(values = c("#0072B2",adjustcolor("#F5C767", alpha.f = .7))) +
  scale_color_manual(values = c("#0072B2", "#F5C767")) +
  scale_x_continuous(limits = c(0, 750)) +
  # facet_wrap(~adipose_clip, ncol = 1) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        text = element_text(size = 15),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.border = element_rect(colour = "grey", fill = NA, size = 1),
        axis.ticks = element_line(size = 0.5, color = "grey"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank()
  )

print(p)
