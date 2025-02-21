library(readr)
library(dplyr)


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

# Creating a new column for the gender of the politician

sentiment_df <- sentiment_df |>
  mutate(
    gender = ifelse(code %in% 
                      c("EH", "BHK", "LM", "LS", "VA", "KKS", "EBS", "MR", "MM", "IK"), 
                    "female", "male")
  )


