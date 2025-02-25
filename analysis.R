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


# t-test -----------------------------------------------------------

t.test(mean_sentiment ~ gender, data = sentiment_df)

# OLS regression -----------------------------------------------------------

ols_model <- lm(mean_sentiment ~ gender * paper, data = sentiment_df)
summary(ols_model)

model_summary <- tidy(ols_model, conf.int = TRUE, conf.level = 0.95)

?tidy

stargazer(ols_model, type = "html", out = "regression_results.doc")

write_excel_csv(ols_model, "OLS_summary.csv")

# Plotting the interaction effect ----------------------------------------

effect_plot <- allEffects(ols_model)
ggplot(effect_plot) +
  geom_line() 

# Changing gender and paper to factors -----------------------------------

sentiment_df$gender <- as.factor(sentiment_df$gender)
sentiment_df$paper <- as.factor(sentiment_df$paper)

interaction_results <- ggpredict(ols_model, terms = c("gender", "paper"), ci_level = 0.95)

?ggpredict

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
  

# plotting the OLS results -----------  ------------------------------

ggplot(model_summary, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  coord_flip() +  # Flip the coordinates for better readability
  labs(title = "Coefficient Plot", x = "Predictors", y = "Estimated Coefficients") +
  theme_minimal()

