library(readr)
library(dplyr)
library(lubridate)
library(ridgeline)
library(ggridges)
library(ggplot2)
library(showtext)
library(paletteer)
library(forcats)

# Enable showtext
showtext_auto()

# Add a Google Font (Lato as an example)
font_add_google("Lato", "lato")

# Reading the data -------------------------------------------

sentiment_df <- read_csv("df_combined.csv")


# Plotting the sentiment scores -------------------------------------------


# density plots -----------------------------------------------------------


selected_colors <- paletteer::paletteer_d("LaCroixColoR::Berry")[c(1, 4)]

# paper x gender
density_plot <- ggplot(sentiment_df, aes(x = mean_sentiment, fill = gender)) +
  facet_wrap(~ paper, scales = "free_x", ncol = 2) +
  geom_density(alpha = 0.4, linewidth = 0.1, bw = 0.6) +
  labs(
    title = NULL,
    x = "Mean sentiment score",
    y = "Density",
    fill = NULL
  ) +
  scale_fill_manual(values = selected_colors) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 30),
        axis.text.y = element_text(size = 30),
        plot.title = element_text(size = 30),
        legend.text = element_text(size = 30),
        strip.text = element_text(size = 30)) +
  theme_minimal(base_family = "lato", base_size = 30) 

density_plot

ggsave("sentiment_scores.png", plot = density_plot, width = 6, height = 4, dpi = 300) 


# ridgeline plots -----------------------------------------------------------

ridgeline(x = sentiment_df$mean_sentiment, y = sentiment_df$code, bw = 0.5, mode = TRUE)

sentiment_df <- sentiment_df |> 
  mutate(code = factor(code, levels = sort(unique(code))))

sentiment_ridges <- sentiment_df |>
  ggplot(aes(x = mean_sentiment, y = code, fill = gender)) +
  geom_density_ridges(scale = 3, alpha = 0.5, linewidth = 0.1) +
  facet_wrap(~ paper, scales = "free_x", ncol = 2) +
  scale_fill_manual(values = selected_colors) +
  scale_x_continuous(limits = c(0, 10)) + 
  labs(
    title = NULL,
    x = "Mean sentiment score",
    y = NULL,
    fill = NULL
  ) +
  theme_ridges() +
  theme_minimal(base_family = "lato", base_size = 30) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 30),
    axis.text.y = element_text(size = 30),
    plot.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    strip.text = element_text(size = 30),
    legend.position = "bottom",  # Move legend
    legend.direction = "horizontal",
    legend.justification = "center"
  )

sentiment_ridges

ggsave("sentiment_ridges.png", plot = sentiment_ridges, width = 5, height = 6, dpi = 300)



# boxplot, paper x round  -----------------------------------------------------------------

# sentiment scores per gender and round


sentiment_df |>
  filter(!is.na(round)) |># Pipe the data into ggplot()
  ggplot(aes(x = mean_sentiment, y = factor(round), fill = gender)) +
  geom_boxplot(na.rm = TRUE) +
  labs(
    title = "Sentiment scores",
    x = "Mean sentiment score",
    y = "Round"
  ) +
  theme_minimal()






