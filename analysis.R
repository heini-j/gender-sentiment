library(car)
library(readr)
library(broom)
library(ggeffects)
library(stargazer)
library(ggplot2)
library(showtext)
library(paletteer)


# Enable showtext
showtext_auto()

# Add a Google Font
font_add_google("Lato", "lato")

sentiment_df <- read_csv("df_combined.csv")

# OLS regression -----------------------------------------------------------

# base model

sentiment_df$gender <- as.factor(sentiment_df$gender)
sentiment_df$paper <- as.factor(sentiment_df$paper)

ols_model <- lm(mean_sentiment ~ gender * paper, data = sentiment_df)
summary(ols_model)

# printing the results in a table to a doc file

stargazer(ols_model, type = "html", out = "ols_model.doc")

# Plotting the interaction effect 

interaction_results <- ggpredict(ols_model, terms = c("gender", "paper"), ci_level = 0.95)

print(interaction_results)

selected_colors <- paletteer::paletteer_d("LaCroixColoR::Berry")[c(4, 6)]

interaction <- ggplot(interaction_results, aes(x = x, y = predicted, group = group, color = group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = selected_colors) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, size = 0.5) +
  labs(
    title = NULL,
    x = NULL,
    y = "Predicted Mean Sentiment Score",
    color = "Source",
    caption = "The interaction effect in shown with 95% confidence intervals."
  ) +
  theme_minimal(base_family = "lato", base_size = 30) 

ggsave("interaction_plot.png", interaction, width = 6, height = 3, dpi = 300)
  

# Adding the control variables  ----------------------------------------

# control model 1 -------------------------------------------------------

ols_model_controls1 <- lm(mean_sentiment ~ gender * paper + left_right, 
                         data = sentiment_df)
summary(ols_model_controls1)

# printing the results in a table to a doc file

stargazer(ols_model_controls1, type = "html", out = "ols_model_controls1.doc")

# updated interaction plot

interaction_controls <- ggpredict(ols_model_controls1, terms = c("gender", "paper"), ci_level = 0.95)

print(interaction_results)

selected_colors <- paletteer::paletteer_d("LaCroixColoR::Berry")[c(4, 6)]

interaction_control1 <- ggplot(interaction_controls, aes(x = x, y = predicted, group = group, color = group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = selected_colors) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, linewidth = 0.5) +
  labs(
    title = NULL,
    x = NULL,
    y = "Predicted Mean Sentiment Score",
    color = "Source",
    caption = "The interaction effect in shown with 95% confidence intervals."
  ) +
  theme_minimal(base_family = "lato", base_size = 30) 

ggsave("interaction_control1.png", interaction_control1, width = 6, height = 3, dpi = 300)

# control model 2 -------------------------------------------------------

ols_model_controls2 <- lm(mean_sentiment ~ gender * paper + seniority + placement_2024 + left_right, 
                        data = sentiment_df)
summary(ols_model_controls2)

# printing the results in a table to a doc file

stargazer(ols_model_controls2, type = "html", out = "ols_model_controls2.doc")

interaction_controls2 <- ggpredict(ols_model_controls2, terms = c("gender", "paper"), ci_level = 0.95)

interaction_control2 <- ggplot(interaction_controls2, aes(x = x, y = predicted, group = group, color = group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = selected_colors) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, linewidth = 0.5) +
  labs(
    title = NULL,
    x = NULL,
    y = "Predicted Mean Sentiment Score",
    color = "Source",
    caption = "The interaction effect in shown with 95% confidence intervals."
  ) +
  theme_minimal(base_family = "lato", base_size = 30) 

ggsave("interaction_control2.png", interaction_control2, width = 6, height = 3, dpi = 300)




