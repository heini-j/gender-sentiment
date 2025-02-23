library(readr)
library(dplyr)
library(ggplot2)
library(showtext)
library(paletteer)

# Enable showtext
showtext_auto()

# Add a Google Font (Lato as an example)
font_add_google("Lato", "lato")

descriptives <- read_csv("df_combined.csv")

# making a summary table of the data, with n of each code and mean_sentiment mean and sd for each code, sorted in descending order

descriptives <- descriptives |>
  mutate(across(starts_with("sentiment_"), as.numeric))

descriptives |>
  group_by(paper, gender) |>
  summarise(n = n(),
            mean = mean(mean_sentiment, na.rm = TRUE),
            sd = sd(mean_sentiment, na.rm = TRUE)
  ) |>
  arrange(desc(n))

# Making a stacked bar plot showing the n for each code per paper

selected_colors <- paletteer::paletteer_d("LaCroixColoR::Berry")[c(4, 6)]


 descriptives_plot <- descriptives |>
  group_by(code, paper, gender) |>
  summarise(n = n(), .groups = "drop") |>  
  arrange(desc(n)) |>
  ggplot(aes(x = reorder(code, -n), y = n, fill = paper)) +
  geom_col(position = "stack") +
  facet_wrap(~ gender, scales = "free_x", ncol = 2) +
  geom_text(aes(label = n), 
            position = position_stack(vjust = 0.5),  # Center inside the bars
            size = 3, color = "white") +  # Adjust text size & color
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Number of articles on each politician by source",
       x = NULL,
       y = NULL,
       fill = NULL) +
  scale_fill_manual(values = selected_colors) +
  theme_minimal(base_family = "lato", base_size = 12) +  
  theme(
    plot.title = element_text(size = 14),
    legend.position = "top",
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.ticks.y = element_blank(), # Remove y-axis ticks
    panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
    panel.grid.minor.y = element_blank())
 
 ggsave("articles.emf", plot = descriptives_plot, width = 8, height = 6, device = "emf")
  

