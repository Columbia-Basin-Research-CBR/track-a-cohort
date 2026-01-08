#' JPE percent loss - compare genetic and LAD loss estimates
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

csv_data <- jpe_genetic_lad_data %>%
  mutate(status = gsub("\n", " ", status)) %>%
  filter(value_type == "pct_total_loss") %>%
  select(-value_type)

csv_path <- here::here("docs/www/TAC_chinook_csvs/")
if (!dir.exists(csv_path)) {
  dir.create(csv_path, recursive = TRUE)
}
write.table(csv_data, paste0(csv_path, "genetic_lad_percent_loss.csv"), row.names = FALSE, sep = "|")

# Get the current timestamp
timestamp <- format(Sys.time(), "%d %b %Y %H:%M:%S %Z")

# bar plot of genetic and LAD loss
p <- jpe_genetic_lad_data %>%
  filter(value_type == "pct_total_loss") %>%
  ggplot(aes(x = as.factor(WY), y = value, fill = method)) +
  geom_bar(stat = "identity", position = position_dodge(.7, preserve = "single")) +
  labs(
    title = "Genetic vs Length-At-Date (LAD) Percent Loss of JPE",
    subtitle = "Species: Natural Winter-run Chinook",
    caption = paste0("Genetic loss data provided by USBR before Water Year 2010;\nLAD and genetic loss data sourced from the CDFW Salvage Database.\n", timestamp),
    x = "Water Year",
    y = "Percent Loss",
    fill = NULL
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, .06), expand = c(0, 0)) +
  scale_fill_manual(
    values = c("#00BFFF", "#0072B2"),
    # breaks = c("pct_gen", "pct_lad"),
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

