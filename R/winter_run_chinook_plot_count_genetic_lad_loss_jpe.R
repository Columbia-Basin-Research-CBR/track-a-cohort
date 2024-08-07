#' JPE loss - compare genetic and LAD loss estimates
#' @description Barplot comparing JPE loss: genetic vs LAD loss estimates.
#' Currently using a flat file provided by BOR that will be updated once genetic data
#' is shared with SacPAS. The LAD loss data is imported directly from SacPAS.
#' @return static barplot comparing JPE loss: genetic v LAD loss estimates

require(tidyverse)
require(here)


# import data file

## Genetic total loss data
load(here("data/jpe_genetic_loss_data.rda"))
genetic_total_loss_data <- jpe_genetic_loss_data$genetic_total_loss_data
## LAD total loss data
load(here("data/jpe_lad_loss_data.rda"))
lad_total_loss_data <- jpe_lad_loss_data$lad_total_loss_data

# Merge genetic and LAD loss data
jpe_genetic_lad_data <- genetic_total_loss_data %>%
  mutate(method = "Genetic") %>%
  bind_rows(lad_total_loss_data %>%
    mutate(method = "LAD") %>%
    filter(between(WY, 1996, 2024))) %>% # filter to match genetic data years provided by BOR
  pivot_longer(
    cols = c(pct_total_loss, total_loss),
    names_to = "value_type",
    values_to = "value"
  )

# bar plot of genetic and LAD loss
p <- jpe_genetic_lad_data %>%
  filter(value_type == "total_loss") %>%
  ggplot(aes(x = as.factor(WY), y = value, fill = method)) +
  geom_bar(stat = "identity", position = position_dodge(.7, preserve = "single")) +
  labs(
    title = "Genetic vs Length-At-Date (LAD) Loss of JPE",
    subtitle = "Species: Natural Winter-run Chinook",
    caption = "Data sources: Genetic loss provided by USBR. LAD loss from CDFW Salvage Database.",
    x = "Water Year",
    y = "Total loss",
    fill = NULL
  ) +
  scale_y_continuous(expand = c(0, 100), labels = scales::comma_format()) +
  scale_fill_manual(
    values = c("#00BFFF", "#0072B2"),
    # breaks = c("count_gen", "count_lad"),
    labels = c("Genetic", "LAD")
  ) +
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



