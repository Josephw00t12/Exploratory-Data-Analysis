# Load packages
library(tidyverse)

# Load dataset (Kaggle path)
df <- read_csv("adult.csv")

# Fix column names (if needed)
colnames(df) <- c(
  "age", "workclass", "fnlwgt", "education", "education_num",
  "marital_status", "occupation", "relationship", "race", "sex",
  "capital_gain", "capital_loss", "hours_per_week", "native_country", "income"
)

glimpse(df)

df <- df %>%
  mutate(across(where(is.character), ~na_if(., "?"))) %>%
  drop_na()

df <- df %>%
  mutate(
    education_grouped = case_when(
      education %in% c("Preschool", "1st-4th", "5th-6th") ~ "Elementary",
      education %in% c("7th-8th", "9th") ~ "Middle School",
      education %in% c("10th", "11th", "12th", "HS-grad") ~ "High School",
      education %in% c("Some-college", "Assoc-acdm", "Assoc-voc") ~ "College/Associate",
      education %in% c("Bachelors", "Masters", "Doctorate", "Prof-school") ~ "Advanced",
      TRUE ~ NA_character_
    )
  )

# Quick checks
head(df)
table(df$education_grouped)
sum(is.na(df$education_grouped))

# Contingency table (education_grouped vs income)
table_counts <- table(df$education_grouped, df$income)
table_counts

# Add row and column totals
table_with_totals <- addmargins(table_counts)
table_with_totals

# Row percentages
row_percent <- prop.table(table_counts, margin = 1) * 100
round(row_percent, 2)

library(gt)

apa_percent <- round(prop.table(table_counts, 1) * 100, 1)

combined <- matrix(
  paste0(table_counts, " (", apa_percent, "%)"),
  nrow = nrow(table_counts),
  dimnames = dimnames(table_counts)
)

combined_df <- as.data.frame.matrix(combined)

gt_table <- combined_df %>%
  gt() %>%
  tab_header(
    title = "Contingency Table",
    subtitle = "Education Group by Income"
  ) %>%
  cols_label(
    `<=50K` = "≤50K",
    `>50K` = ">50K"
  )

gt_table
