#' Figure 3a: JPE percent loss - compare genetic and LAD loss estimates
#' @description static barplot comparing percent JPE loss: genetic v LAD loss estimates
#'
#' @return plot_jpe_pct_genetic_lad_loss
#'
#' @noRd


load("data/jpe_genetic_lad_data.rda")

#bar plot of genetic and LAD loss
plot_jpe_pct_genetic_lad_loss <- jpe_genetic_lad_data %>% 
  filter(method %in% c("pct_lad", "pct_gen")) %>% 
  ggplot(aes(x=as.factor(BroodYear), y= value, fill=method)) +
  geom_bar(stat="identity", position=position_dodge())+
  labs(title = "Genetic vs Length-At-Date (LAD) Historical Percent Loss of the JPE\n",
       x = "Brood Year", 
       y = "Percent of the JPE Lost", #change title
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

print(plot_jpe_pct_genetic_lad_loss)
