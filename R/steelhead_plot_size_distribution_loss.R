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
  filter(length < 750) %>%  #set limit for length -- why 750 seems big?
  # filter(WY > 2004) %>% #why filtering WY after 2004? -- this was in shared BOR script
  ggplot() +
  geom_density( data = . %>% filter(WY != current_year), 
                mapping = aes(x = length, fill = adipose_clip, color = adipose_clip), alpha = .25) +
  geom_histogram(data = . %>% filter(WY == current_year), 
                 mapping = aes(x = length, y=after_stat(density), fill = adipose_clip, color = adipose_clip), 
                 bins = 50,
                 alpha = 0.7) +
  labs(x = 'Fork Length (mm)', 
       y = 'Density', 
       title = "Current and Historical Size Distribution By Rear Type",
       subtitle = paste0("Species: Steelhead\nCurrent Water Year: ", current_year,
                         "\nHistorical Water Years: ", min(steelhead_loss_data$WY) , " to ",max(steelhead_loss_data$WY) )) +
  scale_fill_manual(values = c("grey30", "grey70")) +
  scale_color_manual(values = c("grey30", "grey70")) +
  facet_wrap(~adipose_clip, ncol = 1) +
  theme_minimal() +
  theme(text = element_text(size = 15),
        legend.position = "bottom",
        panel.border= element_rect(color = "grey80", fill = NA), 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(), axis.ticks = element_line(size = .25, color = "grey80"))

print(p)
