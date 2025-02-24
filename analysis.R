library(car)
library(readr)
library(broom)
library(ggeffects)
library(stargazer)
library(ggplot2)

sentiment_df <- read_csv("df_combined.csv")

# t-test -----------------------------------------------------------

t.test(mean_sentiment ~ gender, data = sentiment_df)

# OLS regression -----------------------------------------------------------

ols_model <- lm(mean_sentiment ~ gender * paper, data = sentiment_df)
summary(ols_model)

model_summary <- tidy(ols_model, conf.int = TRUE)

stargazer(ols_model, type = "html", out = "regression_results.doc")

write_excel_csv(ols_model, "OLS_summary.csv")

# Plotting the interaction effect ----------------------------------------

effect_plot <- allEffects(ols_model)
ggplot(effect_plot) +
  geom_line() 

# Changing gender and paper to factors -----------------------------------

sentiment_df$gender <- as.factor(sentiment_df$gender)
sentiment_df$paper <- as.factor(sentiment_df$paper)

interaction_results <- ggpredict(ols_model, terms = c("gender", "paper"))

print(interaction_results)

ggplot(interaction_results, aes(x = x, y = predicted, group = group, color = group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, size = 0.4) +
  labs(
    title = "Interaction Effect of Gender and Paper on Sentiment",
    x = "Gender",
    y = "Predicted Mean Sentiment Score",
    color = "Paper"
  ) +
  theme_minimal(base_family = "lato", base_size = 14)
  

# plotting the OLS results -----------  ------------------------------

ggplot(model_summary, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  coord_flip() +  # Flip the coordinates for better readability
  labs(title = "Coefficient Plot", x = "Predictors", y = "Estimated Coefficients") +
  theme_minimal()

