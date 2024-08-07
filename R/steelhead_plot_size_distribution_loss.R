#' @title Size Distribution of Clipped and Non-clipped Steelhead Loss
#' @description This plot shows the total loss of clipped and non-clipped steelhead by water year.
require(tidyverse)
require(scales)
require(here)


# load data
load(here("data/steelhead_loss_data.rda"))


#set current year
source(here("R/utils_fct_assign_current_water_year.R"))
current_year <- assign_current_water_year()

#plot
p <- steelhead_loss_data %>% 
  filter(length < 750) %>%  # set limit for length -- why 750 seems big?
  ggplot() +
  geom_histogram(data = . %>% filter(WY == current_year), 
                 mapping = aes(x = length, y = after_stat(density), fill = adipose_clip, color = adipose_clip), 
                 bins = 50, color = "white") +
  geom_density(data = . %>% filter(WY != current_year), 
               mapping = aes(x = length, fill = adipose_clip, color = adipose_clip), alpha = .25) +
  
  labs(x = 'Fork Length (mm)', 
       y = 'Density', 
       title = "Current and Historical Size Distribution of Clipped and Unclipped Steelhead",
       subtitle = paste0("Current Water Year: ", current_year,
                         "\nHistorical Water Years: ", min(steelhead_loss_data$WY), " to ", max(steelhead_loss_data$WY-1)),
       fill =  paste0("Histogram (WY", current_year, "):"), 
       color = paste0("Density (WY", min(steelhead_loss_data$WY), "-WY", max(steelhead_loss_data$WY-1), "):"),
       caption = "Data sources: Preliminary data from CDFW; subject to revision.") +
  scale_fill_manual(values = c("#0072B2", "#F5C767")) +
  scale_color_manual(values = c("#0072B2", "#F0AD1F")) +
  facet_wrap(~adipose_clip, ncol = 1) +
  guides(fill = guide_legend(override.aes = list(color = "white")),
         color = guide_legend(override.aes = list(alpha = 0.25, fill = c("#0072B2", "#F5C767")))) +
  theme_minimal() +
  theme(
    legend.position = "bottom", 
    text = element_text(size = 15),
    panel.grid.major.y = element_line(linetype = "dotted"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.ticks = element_line(size = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(p)
