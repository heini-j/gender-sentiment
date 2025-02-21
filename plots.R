library(readr)
library(dplyr)
library(lubridate)

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

