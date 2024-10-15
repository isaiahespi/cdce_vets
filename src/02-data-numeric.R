# using data exported from Qualtrics on 2024-09-11 at 11:39AM 
# this script is used to create the numeric data set

# load packages
library(tidyverse)
library(haven)

# load data_spss version, prior to `haven::as_factor()`
load("data/vets-survey-exp-2024-09-11_spss.Rdata")


data2 <- data_spss |>
  # subset spss data set to omit the display order variables
  select(!contains("_do_")
         & !contains("_ado_")
         & !contains("_click")
         & !contains("_count")) |>
  mutate(
    group = case_when(
      group == "Treatment" ~ 1,
      group == "Control" ~ 0),
    qset = case_when(
      qset == "A" ~ 1,
      qset == "B" ~ 0)) |>
  
  # remove all 'lbl' from all <dbl+lbl> variables, making them all numeric
  haven::zap_labels() |> 
  # re-code values for consistent direction with like variables
  mutate(
    q8  = case_match(q8, 1 ~ 5, 2 ~ 4, 3 ~3, 4~2, 5 ~ 1),
    q20 = case_match(q20, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1),
    q23 = case_match(q23, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1),
    q24 = case_match(q24, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1),
    q31 = case_match(q31, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1),
    q34 = case_match(q34, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1),
    q35 = case_match(q35, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1),
    q49 = case_match(q49, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1),
    q50 = case_match(q50, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1))


