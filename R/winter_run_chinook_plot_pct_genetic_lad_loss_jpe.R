#' Figure 3a: JPE percent loss - compare genetic and LAD loss estimates
#' @description Barplot comparing percent JPE loss: genetic vs LAD loss estimates. Includes shared genetic data and LAD loss data from SacPAS. Update underlying data files as needed
#' @noRd

require(tidyverse)
require(here)
require(scales)

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
  filter(value_type == "pct_total_loss") %>%
  ggplot(aes(x = as.factor(WY), y = value, fill = method)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Genetic vs Length-At-Date (LAD) Percent Loss of JPE by Water Year",
    subtitle = "Species: Winter-run Chinook",
    x = "Water Year",
    y = "Percent Loss",
    fill = NULL
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, .06), expand = c(0, 0)) +
  scale_fill_manual(
    values = c("black", "grey"),
    # breaks = c("pct_gen", "pct_lad"),
    labels = c("Genetic", "LAD")
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(color = "black", .5),
    axis.ticks.x = element_line(color = "black"),
    text = element_text(size = 15)
  )

print(p)

# Notes:
# currently missing 2007 JPE data from SacPAS
# filter WY data to match genetic data years provided by BOR
# plot by BY or WY?
