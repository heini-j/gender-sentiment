library(readr)
library(dplyr)
library(stargazer)
library(sjPlot)

# Enable showtext
showtext_auto()

# Add a Google Font
font_add_google("Lato", "lato")

# Load the data

sentiment_df <- read_csv("df_combined.csv")

# taking a subset of the data

subset_women <- sentiment_df |>
  filter(gender == "female")

# creating a dummy variable to mark congruent politicians

subset_women$incongruent <- ifelse(subset_women$code %in% c("EBS", "KKS", "VA"), 1, 0)

ols_congruence <- lm(mean_sentiment ~ incongruent * paper + left_right, data = subset_women)
summary(ols_congruence)

# saving as a .doc file to access in word

stargazer(ols_congruence, type = "html", out = "ols_congruence.doc")

# coefficient plot

coef_plot <- plot_model(ols_congruence, type = "est", show.values = TRUE, show.p = TRUE, ci.lvl = 0.95)

selected_colors <- paletteer::paletteer_d("LaCroixColoR::Berry")[c(1, 5)]

coef_plot + 
  theme_minimal(base_family = "lato", base_size = 30) +  
  scale_color_manual(values = selected_colors) +  
  labs(title = NULL, x = NULL, y = "Estimate")
