#' @title Juvenile Production Estimate (JPE) Winter-run Chinook
#' @description duplicate of annual JPE barplot on SacPAS.Includes annual JPE estimate with highlighted methods used for each annual estimate.
#' @details To access the data file, navigate to the `data` directory located at the
#' project's root. The file is named `jpe_annual_data.rda` and is sourced from `data-raw >import_jpe_annual_data.R`. 
#' @return static ggplot barplot object
require(tidyverse)
require(scales)
require(here)


#load data file
load(here("data/jpe_annual_data.rda"))

# Get the current timestamp
timestamp <- format(Sys.time(), "%d %b %Y %H:%M:%S %Z")

#bar plot of annual JPE                 
p <- jpe_annual_data %>% 
  mutate(brood_year = as.factor(brood_year)) %>% 
  ggplot(aes(x = brood_year, y = value)) +
  geom_bar(stat = "identity",  aes(fill = "Total natural production entering the Delta")) +
  geom_errorbar(aes(ymin = lower_95_CI, ymax = upper_95_CI, color = "95% CI"), width = 0.25) +
  geom_point(data = . %>% filter(!is.na(lower_95_CI) & !is.na(upper_95_CI)), #only add point for years with CI
             aes(color = "95% CI")) +
  labs(title = "Juvenile Production Estimate (JPE) Winter-run Chinook",
       subtitle = "Natural-origin: Total natural production entering the Delta",
       caption = timestamp,
       x = "Brood year", 
       y = "Total natural production entering the Delta", 
       fill = NULL,
       color = NULL) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.05))) +  # Set the lower limit of the y-axis to 0
  scale_color_manual(values = c("95% CI" = "black")) +
  scale_fill_manual(values = c("Total natural production entering the Delta" = "#0072B2")) +
  guides(fill = guide_legend(order = 1),
         color = guide_legend(order = 2)) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        text = element_text(size = 15),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.ticks = element_line(size = 0.5),
        axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank()
        )
  


print(p)
