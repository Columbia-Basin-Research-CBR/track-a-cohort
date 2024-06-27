#' Figure 3a: JPE percent loss - compare genetic and LAD loss estimates
#' @description Barplot comparing percent JPE loss: genetic vs LAD loss estimates.
#' Currently using a flat file provided by BOR that will be updated once genetic data
#' is shared with SacPas. The data file is loaded using the `here` package, ensuring
#' that the path is relative to the project's root directory.
#' @details To access the data file, navigate to the `data` directory located at the
#' project's root. The file is named `jpe_genetic_lad_data.rda` and is sourced from `data-raw >import_winter_run_chinook_genetic_lad_loss_data.R`.
#' @return static barplot comparing percent JPE loss: genetic v LAD loss estimates
#' @import ggplot2
#' @import dplyr
#' @import here
#' @noRd

#import data file
load(here("data/jpe_genetic_lad_data.rda"))

#bar plot of genetic and LAD loss
p <- jpe_genetic_lad_data %>% 
  filter(method %in% c("pct_lad", "pct_gen")) %>% 
  ggplot(aes(x=as.factor(BroodYear), y= value, fill=method)) +
  geom_bar(stat="identity", position=position_dodge())+
  labs(title = "Genetic vs Length-At-Date (LAD) Historical Percent Loss of the JPE\n",
       x = "Brood Year", 
       y = "Percent of the JPE Loss",
       fill = NULL) +
  scale_y_continuous(breaks = seq(0, 6, by = 2), limits = c(0, 6), expand = c(0, 0))+
  scale_fill_manual(values = c("black", "grey"),
                    breaks = c("pct_gen", "pct_lad"),
                    labels = c("Genetic", "LAD"))+
  theme_minimal() +
  theme(legend.position = "right", 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        panel.grid.major.x = element_blank(), 
        axis.line.x = element_line(color = "black", .5),
        axis.ticks.x = element_line(color = "black"), 
        text = element_text(size = 15))

print(p)
