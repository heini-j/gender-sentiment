library(readr)
library(dplyr)
library(lubridate)
library(ridgeline)
library(ggridges)
library(ggplot2)

# Reading and cleaning the data -------------------------------------------

sentiment_df <- read_csv("df_combined.csv")

View(sentiment_df)

# Moving the code to the beginning of the dataframe

sentiment_df <- sentiment_df |>
  relocate(code)

# Renaming some columns for simplicity

sentiment_df <- sentiment_df |>
  rename(
    date = Date,
    paper = Paper,
    title = "Title of article"
  )

# creating round 1, round 2 and round 3 variables based on the date

sentiment_df$date <- dmy(sentiment_df$date)

# creating a round1 variable if the date is before 1-12-2022

sentiment_df$round1 <- ifelse(sentiment_df$date < dmy("01-12-2022"), 1, 0)
sentiment_df$round2 <- ifelse(sentiment_df$date >= dmy("01-12-2022") & sentiment_df$date < dmy("01-12-2023"), 1, 0)
sentiment_df$round3 <- ifelse(sentiment_df$date >= dmy("01-12-2023"), 1, 0)

# Creating a variable "round" that is 1, 2 or 3 based on the date

sentiment_df$round <- case_when(
  sentiment_df$date < dmy("01-12-2022") ~ 1,
  sentiment_df$date >= dmy("01-12-2022") & sentiment_df$date < dmy("01-12-2023") ~ 2,
  sentiment_df$date >= dmy("01-12-2023") ~ 3
)

# Creating a new column for the gender of the politician

sentiment_df <- sentiment_df |>
  mutate(
    gender = ifelse(code %in% 
                      c("EH", "BHK", "LM", "LS", "VA", "KKS", "EBS", "MR", "MM", "IK"), 
                    "female", "male")
  )


# Plotting the sentiment scores -------------------------------------------

p1 <- ggplot(sentiment_df, aes(mean_sentiment)) +
  geom_histogram(binwidth = 0.5) +
  facet_grid(gender ~ paper, scales = "free_x", space = "free_x") +
  labs(
    title = "sentiment scores",
    tag = "Figure 1",
    x = "sentiment score average per article",
    y = "count",
  )

p1 + theme_minimal()


p2 <- ggplot(sentiment_df, aes(mean_sentiment)) +
  geom_histogram(binwidth = 0.5) +
  facet_grid( ~ paper, scales = "free_x", space = "free_x") +
  labs(
    title = "sentiment scores",
    tag = "Figure 1",
    x = "sentiment score average per article",
    y = "count",
  )

ggplot(sentiment_df, aes(x = mean_sentiment, fill = gender)) +
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

# ridgeline plots

ridgeline(x = sentiment_df$mean_sentiment, y = sentiment_df$code, bw = 0.5, mode = TRUE)

sentiment_df |>
  ggplot(aes(x = mean_sentiment, y = code, fill = gender)) +
  geom_density_ridges(scale = 1.5, ) +
  theme_ridges()



