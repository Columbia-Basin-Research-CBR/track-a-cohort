#' Figure 1: Juvenile Production Estimate (JPE) Winter-run Chinook
#' @description duplicate of annua; JPE barplot on SacPas.
#'
#' @return ggplot barplot of annual JPE estimate with highlighted methods used for annual estimate
#'
#' @noRd
#' 

source("../TAC/R/load_dependencies.R")
load(here("data/jpe_annual_data.rda"))
                 
jpe_annual_barplot <- jpe_annual_data %>% 
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
  scale_fill_manual(values = c("NA" = "grey80", "Method 2 (O'Farrell et al., 2018)" = "grey30"))+
  theme_minimal() +
  theme(
        legend.position = "right",
        axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1), 
        axis.line = element_line(colour = "grey", linewidth = .25), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x  = element_blank())


print(jpe_annual_barplot)
