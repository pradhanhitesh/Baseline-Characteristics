# Authors: Suhrud Panchwagh, Hitesh Pradhan
#
# Script name: flextable.R
#

# Import libraries
library(tidyverse)
library(gtsummary)
library(flextable)
library(readxl)
library(dplyr)

# Import data
df <- read.csv("path/to/your/csv/file")
attach(df)

# Define your categorical grouping variables
df$Group <- as.factor(df$Group)

# If you have multiple categorical variables,
# use the same command. (Line 13)
df$cat_var1 <- as.factor(df$cat_var1)
df$cat_var2 <- as.factor(df$cat_var2)
df$cat_var3 <- as.factor(df$cat_var3)

# If you have only two groups, use t.test
table1 <- df[, -c(1, 2)] %>%
    tbl_summary(
        by = Group,
        type = list(where(is.numeric) ~ "continuous"),
        missing = "no",
        statistic = list(all_continuous() ~ "{mean} ({sd})")
    ) %>%
    add_p(test = list(where(is.numeric) ~ "t.test")) %>%
    add_overall() %>%
    as_flex_table() %>%
    save_as_docx(path = "flextable.docx")

# If you have more than two groups, use aov
table1 <- df[, -c(1, 2)] %>%
    tbl_summary(
        by = Group,
        type = list(where(is.numeric) ~ "continuous"),
        missing = "no",
        statistic = list(all_continuous() ~ "{mean} ({sd})")
    ) %>%
    add_p(test = list(where(is.numeric) ~ "aov")) %>%
    add_overall() %>%
    as_flex_table() %>%
    save_as_docx(path = "flextable.docx")
