library(readr)
library(tidyverse)

# Read the CSV file
df <- read_csv("GPF.csv")

# Transform data to wide format

df_wide <- df %>%
  select(Paper, Date, "Title of article", variable, Sentiment) %>%  # Keep relevant columns
  pivot_wider(names_from = variable, values_from = Sentiment, names_prefix = "sentiment_")

View(df_wide)

# adding the code of the person as a column to the new df

df_wide$code <- "IK"


# creating a function to use it with all the sentiment dfs ------------------------ 

codes <- c("EH", "BHK", "LM", "LS", "VA", "KKS", "EBS", "MR", "MM", "IK",
           "AC", "TB", "GP", "IC", "AR", "EN", "TA", "GPF", "MC", "JG")

file_list <- list.files(path = getwd(), pattern = paste0("^(", paste(codes, collapse = "|"), ").*\\.csv$"), full.names = TRUE)

df_list <- list()

transform_df <- function(data) {
  data |>
    select(Paper, Date, "Title of article", variable, Sentiment) |>  # Keep relevant columns
    pivot_wider(names_from = variable, values_from = Sentiment, names_prefix = "sentiment_")
  
  
}

for (file in file_list) {
  # Extract code from filename
  code <- str_extract(basename(file), paste(codes, collapse = "|"))
  
  df <- read_csv(file)
  
  df_list[[code]] <- transform_df(df)
  
  df_list[[code]]$code <- code
  
  # Print confirmation
  print(paste("Processed:", code))
}

# Combine all data frames into one

df_combined <- bind_rows(df_list)
