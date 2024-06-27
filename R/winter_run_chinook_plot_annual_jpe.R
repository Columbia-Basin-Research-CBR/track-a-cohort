#' Figure 1: Juvenile Production Estimate (JPE) Winter-run Chinook
#' @description duplicate of annual JPE barplot on SacPas.Includes annual JPE estimate with highlighted methods used for each annual estimate.
#' @details To access the data file, navigate to the `data` directory located at the
#' project's root. The file is named `jpe_annual_data.rda` and is sourced from `data-raw >import_jpe_annual_data.R`. 
#' @return static ggplot barplot object
#' @import ggplot2
#' @import scales
#' @import dplyr
#' @import here
#' @noRd

#load data file
load(here("data/jpe_annual_data.rda"))

#bar plot of annual JPE                 
p <- jpe_annual_data %>% 
  mutate(brood_year = as.factor(brood_year)) %>% 
  ggplot( aes(x=brood_year, y=value, fill = method))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = lower_95_CI, ymax =upper_95_CI), width = 0.25)+
  labs(title = "Juvenile Production Estimate (JPE) Winter-run Chinook",
       subtitle = "Natural-origin: Total natural production entering the Delta",
       x = "Brood year", 
       y = "Total number of juvenile fish", 
       fill = "JPE Method")+
  scale_y_continuous( labels= scales::comma, expand = expansion(mult = c(0, 0.05))) +  # Set the lower limit of the y-axis to 0
  scale_fill_manual(values = c("NA" = "grey80", "Method 2 (O'Farrell et al., 2018)" = "grey30"),
                    labels = c("NA", "Method 2\n(O'Farrell et al., 2008)"))+
  theme_minimal() +
  theme(text = element_text(size = 15),
        legend.position = "right",
        axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1), 
        axis.line = element_line(colour = "grey", linewidth = .25), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x  = element_blank())


print(p)
