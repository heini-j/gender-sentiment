library(readr)
library(dplyr)
library(lubridate)
library(ridgeline)
library(ggridges)
library(ggplot2)
library(showtext)
library(paletteer)

# Enable showtext
showtext_auto()

# Add a Google Font (Lato as an example)
font_add_google("Lato", "lato")

# Reading and cleaning the data -------------------------------------------

sentiment_df <- read_csv("df_combined.csv")


# Plotting the sentiment scores -------------------------------------------


# density plots -----------------------------------------------------------
?geom_density

selected_colors <- paletteer::paletteer_d("LaCroixColoR::Berry")[c(1, 4)]

# paper x gender
density_plot <- ggplot(sentiment_df, aes(x = mean_sentiment, fill = gender)) +
  facet_wrap(~ paper, scales = "free_x", ncol = 2) +
  geom_density(alpha = 0.4, linewidth = 0.1, bw = 0.6) +
  labs(
    title = "Mean sentiment scores for female and male politicians",
    x = "Mean sentiment score",
    y = "Density",
    fill = NULL
  ) +
  scale_fill_manual(values = selected_colors) +
  theme_minimal(base_family = "lato", base_size = 12) +
  theme(
    plot.title = element_text(size = 14),
    legend.position = "top"
  ) 

density_plot

ggsave("sentiment_scores.emf", plot = density_plot, width = 8, height = 6, device = "emf")

ggplot(sentiment_df, aes(x = mean_sentiment, fill = paper)) +
  facet_wrap(~ paper, scales = "free_x", ncol = 2) +  
  geom_density(alpha = 0.3) +
  theme_minimal()


# making a function to create the histogram for each code

sentiment_df |>
  filter(gender == "female") |>
  ggplot(aes(mean_sentiment)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(paper~code, scales = "free_y") +
  labs(
    title = "Distribution of mean sentiment scores per female politician",
    x = "Mean sentiment score",
    y = "Frequency"
  ) +
  theme_minimal()

sentiment_df |>
  filter(gender == "male") |>
  ggplot(aes(mean_sentiment)) +
  geom_histogram(binwidth = 0.5) +
  facet_grid(code ~ paper, scales = "free_y") +
  labs(
    title = "Distribution of mean sentiment scores per female politician",
    x = "Mean sentiment score",
    y = "Frequency"
  ) +
  theme_minimal()

  

create_histogram <- function(code) {
    sentiment_df |>
    filter(code == code) |>
    ggplot(aes(mean_sentiment)) +
    geom_histogram(binwidth = 0.5) +
    facet_wrap(~paper) +
    labs(
      title = paste("Distribution of mean sentiment scores for", code),
      x = "Mean sentiment score",
      y = "Frequency"
    )
}

codes <- c("EH", "BHK", "LM", "LS", "VA", "KKS", "EBS", "MR", "MM", "IK",
           "AC", "TB", "GP", "IC", "AR", "EN", "TA", "GPF", "MC", "JG")

for (code in codes) {
  create_histogram(code)
  print(paste("Histogram for code", code, "created"))
  
}

# ridgeline plots -----------------------------------------------------------

ridgeline(x = sentiment_df$mean_sentiment, y = sentiment_df$code, bw = 0.5, mode = TRUE)

sentiment_df |>
  ggplot(aes(x = mean_sentiment, y = code, fill = as.factor(gender))) +
  geom_density_ridges(scale = 3, alpha = 0.5) +
  facet_wrap(~ paper, scales = "free_x", ncol = 2) +
  scale_fill_manual(values = selected_colors) +
  theme_ridges() +
  theme_minimal(base_family = "lato", base_size = 12) +
  theme(
    plot.title = element_text(size = 14),
    legend.position = "top"
   )



# boxplots -----------------------------------------------------------------

# making a stacked bar plot to show how many observations there are of each code in the dataframe

sentiment_df |>
  ggplot(aes(x = fct_infreq(code), fill = paper)) +
  geom_bar(position = "stack") +
  labs(
    title = "Number of observations per code",
    x = "Code",
    y = "Count"
  ) +
  geom_text( 
    size = 3, position = position_stack(vjust = 0.5)) + 
  theme_minimal()


sentiment_df |>
  group_by(code) |>
  summarise(n = n(), mean = mean(mean_sentiment), sd = sd(mean_sentiment))

unique(sentiment_df$code)
