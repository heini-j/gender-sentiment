library(car)
library(readr)
library(broom)
library(effects)

sentiment_df <- read_csv("df_combined.csv")

# t-test -----------------------------------------------------------

t.test(mean_sentiment ~ gender, data = sentiment_df)

# OLS regression -----------------------------------------------------------

ols_model <- lm(mean_sentiment ~ gender * paper, data = sentiment_df)
summary(ols_model)

model_summary <- tidy(ols_model)

effect_plot <- allEffects(ols_model)
plot(effect_plot)

# plotting the OLS results -----------  ------------------------------

ggplot(model_summary, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  coord_flip() +  # Flip the coordinates for better readability
  labs(title = "Coefficient Plot", x = "Predictors", y = "Estimated Coefficients") +
  theme_minimal()

