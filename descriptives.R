library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

df <- read_excel("sentiment_analysis.xlsx", sheet = "total")

# creating a  the gender of variable for the gender of a politician by their codes


df$gender <- ifelse(df$Code %in% c("EH", "BHK", "LM", "LS", "VA", "KKS", "EBS", "MR", "MM", "IK"), 1, 0)

colnames(df)                                   
# renaming the columns

df <- df |> 
  rename(code = "Code", 
         WW_total = "Articles WW total",
         WW_round1 = "21-22...4",
         WW_round2 = "22-23...5",
         WW_round3 = "23-24...6",
         WOZ_total = "Articles WOZ total",
         WOZ_round1 = "21-22...8",
         WOZ_round2 = "22-23...9",
         WOZ_round3 = "23-24...10",
         n_total = "TOTAL")

# selecting only rows that dont have missing values in the "code" column

descriptives <- df[!is.na(df$code),]

View(descriptives)

# filtering out row 11

descriptives <- descriptives[-11,]


# making a stacked bar plot for each code, combining the WW_total and WOZ_total columns and grouping to two by gender

articles <- descriptives |>
  pivot_longer(cols = c(WW_total, WOZ_total), names_to = "source", values_to = "total") |>
  mutate(source = recode(source, "WW_total" = "Weltwoche", "WOZ_total" = "Wochenzeitung")) |>
  ggplot(aes(x = code, y = total, fill = source)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~gender, 
             scales = "free_x", 
             labeller = as_labeller(c("0" = "Male politicians", "1" = "Female politicians"))) +
  scale_fill_manual(values = c("Weltwoche" = "#6a3d9a",  # Deep purple
                               "Wochenzeitung" = "#1f78b4"), 
                                guide = guide_legend(title = NULL))+ # Blue-grey
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Number of articles by politician and source",
       x = NULL,
       y = "Total articles")

ggsave("articles.emf", plot = articles, width = 8, height = 6, device = "emf")  # Best for Word


# creating a bar plot that shows the total number of articles by research round

rounds <- descriptives |>
  pivot_longer(cols = c(WW_round1, WW_round2, WW_round3, WOZ_round1, WOZ_round2, WOZ_round3), names_to = "round", values_to = "total") |>
  mutate(round = recode(round, "WW_round1" = "Round 1", "WW_round2" = "Round 2", "WW_round3" = "Round 3", "WOZ_round1" = "Round 1", "WOZ_round2" = "Round 2", "WOZ_round3" = "Round 3")) |>
  ggplot(aes(x = round, y = total, fill = gender)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Round 1" = "#e41a1c",  # Red
                               "Round 2" = "#377eb8",  # Blue
                               "Round 3" = "#4daf4a"),  # Green
                    guide = guide_legend(title = NULL)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Number of articles by research round",
       x = NULL,
       y = "Total articles")







