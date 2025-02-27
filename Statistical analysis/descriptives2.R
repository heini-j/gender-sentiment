library(readr)
library(dplyr)
library(ggplot2)
library(showtext)
library(paletteer)

# Enable showtext
showtext_auto()

# Add a Google Font
font_add_google("Lato", "lato")

# Reading the data -------------------------------------------

descriptives <- read_csv("df_combined.csv")

# Descriptive tables -------------------------------------------

# making a summary table of the data, with n of each code and mean_sentiment mean and sd for each code, sorted in descending order

descriptives <- descriptives |>
  mutate(across(starts_with("sentiment_"), as.numeric))

descriptives_table <- descriptives |>
  group_by(paper, gender) |>
  summarise(n = n(),
            mean = mean(mean_sentiment, na.rm = TRUE),
            sd = sd(mean_sentiment, na.rm = TRUE)
  ) |>
  arrange(desc(gender))

write_excel_csv(descriptives_table, "descriptives_table.csv")

# only number of articles

descriptives_table2 <- descriptives |>
  group_by(paper, gender, round) |>
  summarise(n = n()
  ) |>
  arrange(desc(gender))

write_excel_csv(descriptives_table2, "descriptives_table2.csv")


# Visualising -------------------------------------------

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
            size = 10, color = "white") +  # Adjust text size & color
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 30),
              axis.text.y = element_text(size = 30),
              plot.title = element_text(size = 30),
              legend.text = element_text(size = 30),
              strip.text = element_text(size = 30))+
  labs(title = NULL,
       x = NULL,
       y = NULL,
       fill = NULL) +
  scale_fill_manual(values = selected_colors) +
  theme_minimal(base_family = "lato", base_size = 28)
 
 ggsave("articles.png", plot = descriptives_plot, width = 8, height = 6, dpi = 300)
  

