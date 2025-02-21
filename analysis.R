library(car)

ols_model <- lm(mean_sentiment ~ gender * paper + round, data = sentiment_df)
summary(ols_model)


anova_model <- aov(mean_sentiment ~ gender * paper, data = sentiment_df)
summary(anova_model)

TukeyHSD(anova_model)

plot(anova_model)  # Residual plots
shapiro.test(residuals(anova_model))  # Normality test
leveneTest(anova_model) 

boxplot(residuals(anova_model))

kruskal.test(mean_sentiment ~ gender, data = sentiment_df)
