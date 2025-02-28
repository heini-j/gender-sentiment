library(readr)
library(tidyverse)


# Creating an example case for generalising the code ----------------------------

# Read the CSV file
df <- read_csv("GPF.csv")

# Transform data to wide format

df_wide <- df %>%
  select(Paper, Date, "Title of article", variable, Sentiment) %>%  # Keep relevant columns
  pivot_wider(names_from = variable, values_from = Sentiment, names_prefix = "sentiment_")

View(df_wide)

# adding the code of the person as a column to the new df

df_wide$code <- "GPF"


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
  
  df <- read_csv(file, col_names = TRUE)
  
  df_list[[code]] <- transform_df(df)
  
  df_list[[code]]$code <- code
  
  # Print confirmation
  print(paste("Processed:", code))
}

# Combine all data frames into one

df_combined <- bind_rows(df_list)

df_combined <- bind_rows(list(df_combined, df_wide))

# Checking that all codes are included

unique(df_combined$code)

# Cleaning and arrranging the data --------------------------------------------

# Adding a mean sentiment column
df_combined$mean_sentiment <- rowMeans(df_combined |> select(starts_with("sentiment_")), na.rm = TRUE)

# Moving the code to the beginning of the dataframe

df_combined <- df_combined |>
  relocate(code)

# Renaming some columns for simplicity

df_combined <- df_combined |>
  rename(
    date = Date,
    paper = Paper,
    title = "Title of article"
  )

# creating round 1, round 2 and round 3 variables based on the date

df_combined$date <- dmy(df_combined$date)

# creating a round1 variable if the date is before 1-12-2022

df_combined$round1 <- ifelse(df_combined$date < dmy("01-12-2022"), 1, 0)
df_combined$round2 <- ifelse(df_combined$date >= dmy("01-12-2022") & df_combined$date < dmy("01-12-2023"), 1, 0)
df_combined$round3 <- ifelse(df_combined$date >= dmy("01-12-2023"), 1, 0)

# Creating a variable "round" that is 1, 2 or 3 based on the date

df_combined$round <- case_when(
  df_combined$date < dmy("01-12-2022") ~ 1,
  df_combined$date >= dmy("01-12-2022") & df_combined$date < dmy("01-12-2023") ~ 2,
  df_combined$date >= dmy("01-12-2023") ~ 3
)

# Creating a new column for the gender of the politician

df_combined <- df_combined |>
  mutate(
    gender = ifelse(code %in% 
                      c("EH", "BHK", "LM", "LS", "VA", "KKS", "EBS", "MR", "MM", "IK"), 
                    "female", "male")
  )

# changing the type of all columns starting "sentiment_" to numeric

df_combined <- df_combined |>
  mutate(across(starts_with("sentiment_"), as.numeric))

# Control variables -----------------------------------------------------------

df_combined <- read_csv("df_combined.csv")

# seniority in years

df_combined$seniority <- case_when(
  df_combined$code %in% c("LM", "LS") ~ 4,
  df_combined$code %in% c("MR", "EBS", "EH", "TB") ~ 5,
  df_combined$code %in% c("IK") ~ 7,
  df_combined$code %in% c("MM", "AC") ~ 9,
  df_combined$code %in% c("BHK", "KKS", "AR", "JG", "MC", "TA") ~ 13,
  df_combined$code %in% c("IC", "EN") ~ 17,
  df_combined$code %in% c("VA") ~ 19,
  df_combined$code %in% c("GP", "GPF") ~ 21)

# party's placement on the left_right on 1-3 scale with labels

df_combined$left_right <- case_when(
  df_combined$code %in% c("EH", "LM", "EBS", "MM", "IK", "EN") ~ 1,
  df_combined$code %in% c("BHK", "VA", "LS", "GPF", "MC", "JG") ~ 2,
  df_combined$code %in% c("KKS", "MR", "GP", "AR", "IC", "AC", "TB", "TA") ~ 3
)

df_combined |>
  mutate(left_right = factor(left_right, levels = 1:3, labels = c("left", "center", "right")))

# placement in 2024

df_combined$placement_2024 <- case_when(
  df_combined$code %in% c("EBS", "KKS", "VA", "AR", "GP", "IC") ~ 1,
  df_combined$code %in% c("BHK", "EH", "AC", "TB") ~ 2,
  df_combined$code %in% c("IK", "MM", "MR", "EN", "GPF", "JG", "MC", "TA") ~ 3,
  df_combined$code %in% c("LS", "LM") ~ 4
)

df_combined |>
  mutate(placement_2024 = factor(placement_2024, levels = 1:4, labels = c("federal council", "council of states", "national council", "party president")))


# saving the dataset ----------------------------------------------------------

write_csv(df_combined, "df_combined.csv")
