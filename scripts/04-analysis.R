# analysis

# load packages
library(tidyverse)
library(janitor)
library(easystats)
library(gtsummary)
# library(gt)
# library(labelled)
# library(sjlabelled)
# library(sjmisc)
# library(sjPlot)
# library(sjstats)
# library(pewmethods)
# library(surveytoolbox)
# library(expss)

# load data
load("data/data_vets_survey-exp-2024-09-11.Rdata") # data set as factor
load("data/data_numeric_vets_survey-exp-2024-09-11.Rdata") # numeric data set
load("data/vets-survey-exp-2024-09-11_spss.Rdata") #spss data set

# construct the partyid var with 3 categories, true independents
data <- data |>
  mutate(
    partyid_3cat = pewmethods::fct_case_when(
      partyid == "Republican" ~ "Republican",
      partystr_rep == "Strong" ~ "Republican",
      partystr_rep == "Not very strong" ~ "Republican",
      partylean == "Republican" ~ "Republican",
      partyid == "Democrat" ~ "Democrat",
      partystr_dem == "Strong" ~ "Democrat",
      partystr_dem == "Not very strong" ~ "Democrat",
      partylean == "Democratic" ~ "Democrat",
      partyid == "Independent" ~ "Independent",
      partyid == "Other" ~ "Independent",
      partylean == "Neither" ~ "Independent"
    ),
    .before = partyid) |> 
  labelled::set_variable_labels(
    partyid_3cat = "Party ID 3 categories, with true Independents")


# create an age group variable
data <- data |>    
  mutate(age_cat = 
           forcats::fct_collapse(
             as_factor(age),
             "18-24" = c(18:24),
             "25-34" = c(25:34),
             "35-44" = c(35:44),
             "45-54" = c(45:54),
             "55-64" = c(55:64),
             "65-74" = c(65:74),
             "75-84" = c(75:82, 84),
             "85-92" = c(85:88, 92)
             ),
         .after = age) |> 
  labelled::set_variable_labels(
    age_cat = "Age categorized into eight groups"
  )

# generate data dictionary
data_dict <- labelled::generate_dictionary(data)


# check balance of treatment and control groups ::::::::::::::::::::::::::::####

# table of treatment and control groups. Simplest
data |> 
  janitor::tabyl(group, show_na = T) |> 
  janitor::adorn_totals("row") |> 
  janitor::adorn_pct_formatting()

# alternative. Informative
data |> 
  datawizard::data_tabulate(group)

# quick, informative, easy
data |> 
  select(group, age, gender, race, educ, milserv1, milserv2, milservfam) |> 
  datawizard::data_tabulate(drop_levels = T,
                            remove_na = F,
                            collapse = T)



# best method for html document
data |>  
  gtsummary::tbl_summary(
    by = group,
    include = c(age, gender, race, educ, milserv1, milserv2, milservfam),
    statistic = list(age ~ "{mean} ({sd})",
                     gtsummary::all_categorical()~ "{n} ({p}%)"),
    type = list(milserv1 ~ "categorical",
                milserv2 ~ "categorical",
                milservfam ~"categorical"), 
    missing = "no") |> 
  gtsummary::add_n() |> 
  gtsummary::add_overall() |> 
  gtsummary::modify_header(label = "**Variable**") |> 
  gtsummary::bold_labels() |> 
  gtsummary::as_gt() |> 
  gt::tab_header("Table 1. Balance") |> 
  gt::tab_footnote("NA omitted. Failure to complete survey, n = 120") |> 
  gt::tab_options(
    table.font.size = "small",
    data_row.padding = gt::px(1)
  )

# describe distribution (as best as possible with given data)
# better for particular variables
data2 |> 
  select(group, age, gender, race, educ, milserv1, milserv2, milservfam) |> 
  datawizard::describe_distribution(include_factors = T)



# Crabs (crosstabs) ::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# count results of crosstab using base R `table()`
# recall: table(independent_var, dependent_var)
table(data$group, data$q19)

# recall: table(row, col, z)
table(data$q7, data$q19, data$group, useNA = "ifany")

# prop.table with row percentages
# recall: prop.table(table(iv, dv, z), margin = 1) for row percentages
round(prop.table(table(data$q7, data$q19, data$group), margin = 1), 4)


# crosstab using the janitor package for comparison
# tabyl works like this: tabyl(row_var, col_var)
data |> 
  janitor::tabyl(group, q19, show_na = F) |> 
  janitor::adorn_totals("col") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2) |> 
  janitor::adorn_ns()


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

dvs_az <- data |> select(q19:q29) |> colnames()

dv_lcl <- data |> select(q30:q40_5) |> colnames()

confimpct_vars <- data |> select(q41_1:q43_6) |> colnames()

confimpct_maj_vars <- data |> select(q44_1:q46_6) |> colnames()

ex_vars <- data |> select(q5:q8) |> colnames()

demo_vars <- data |>
  select(age_cat, gender, hisp, race, educ, milserv1, milserv2, milservfam) |>
  colnames()

poli_vars <- data |> select(voted2020, choice2020, voteintent, partyid_3cat,
                            ideo, ideolean)


# get levels of selected variables

# "not confident" levels
data |>
  select(q19,q20,q22,q23,q24,q26, q30, q31, q33, q34, q35, q37) |>
  sapply(levels)


# "not likely" levels
data |>  select(contains("28_"), contains("40_")) |>  sapply(levels)

# increase/decrease confidence levels
data |>  select(contains("41_"),
                contains("43_"),
                contains("44_"),
                contains("46_")) |>  sapply(levels)


# committed/not committed, concerned/not concerned, approve/disapprove,
# adopt/shouldn't adopt levels
data |>  select(q21, q32, q25, q36, q27, q39, q29) |>  sapply(levels)


data |>  select(q19:q29, -q21, -q25, -q27, -q29, -contains("28_")) |>  sapply(levels)

data |>  select(q21, q25, q27, q29, -contains("28_")) |>  sapply(levels)


# collapse factor vars and add new collapsed factors to data frame
dumdf <- data |>
  select(rowID, q19, q20, q22, q23, q24, q26, q30, q31, q33, q34, q35, q37) |>
  mutate(across(
    c(q19, q20, q22, q23, q24, q26, q30, q31, q33, q34, q35, q37),
    ~ forcats::fct_collapse(
      .,
      Not_confident = c("Not at all confident", "Not too confident"),
      Confident = c("Somewhat confident", "Very confident")
    ),
    .names = "{.col}_2lvl"
  ))

dumdf <- dumdf |> 
  select(rowID, contains("_2lvl")) |> 
  pewmethods::dummify_factors(sep = "_")

dumdf <- dumdf |> 
  select(rowID, contains("_2lvl")) |> 
  mutate(
    q19_dum = dplyr::case_when(
    q19_2lvl_Not_confident == 1 ~ 0,
    q19_2lvl_Confident == 1 ~ 1,
    TRUE ~ NA),
    q20_dum = dplyr::case_when(
    q20_2lvl_Not_confident == 1 ~ 0,
    q20_2lvl_Confident == 1 ~ 1,
    TRUE ~ NA),
    q22_dum = dplyr::case_when(
    q22_2lvl_Not_confident == 1 ~ 0,
    q22_2lvl_Confident == 1 ~ 1,
    TRUE ~ NA),
    q23_dum = dplyr::case_when(
    q23_2lvl_Not_confident == 1 ~ 0,
    q23_2lvl_Confident == 1 ~ 1,
    TRUE ~ NA),
    q24_dum = dplyr::case_when(
    q24_2lvl_Not_confident == 1 ~ 0,
    q24_2lvl_Confident == 1 ~ 1,
    TRUE ~ NA),
    q26_dum = dplyr::case_when(
    q26_2lvl_Not_confident == 1 ~ 0,
    q26_2lvl_Confident == 1 ~ 1,
    TRUE ~ NA),
    q30_dum = dplyr::case_when(
    q30_2lvl_Not_confident == 1 ~ 0,
    q30_2lvl_Confident == 1 ~ 1,
    TRUE ~ NA),
    q31_dum = dplyr::case_when(
    q31_2lvl_Not_confident == 1 ~ 0,
    q31_2lvl_Confident == 1 ~ 1,
    TRUE ~ NA),
    q33_dum = dplyr::case_when(
    q33_2lvl_Not_confident == 1 ~ 0,
    q33_2lvl_Confident == 1 ~ 1,
    TRUE ~ NA),
    q34_dum = dplyr::case_when(
    q34_2lvl_Not_confident == 1 ~ 0,
    q34_2lvl_Confident == 1 ~ 1,
    TRUE ~ NA),
    q35_dum = dplyr::case_when(
    q35_2lvl_Not_confident == 1 ~ 0,
    q35_2lvl_Confident == 1 ~ 1,
    TRUE ~ NA),
    q37_dum = dplyr::case_when(
    q37_2lvl_Not_confident == 1 ~ 0,
    q37_2lvl_Confident == 1 ~ 1,
    TRUE ~ NA),
    .keep = "unused"
    )

df <- datawizard::data_merge(data, dumdf, join = "left", by = "rowID")



# get levels for matrix style question Q28
data |>  select(contains("28_")) |>  sapply(levels)

# collapse factor vars and add new collapsed factors to data frame
dum_28 <- data |>
  select(rowID, contains("28_")) |> 
  mutate(across(contains("28_"), ~forcats::fct_collapse(
    .,
    Not_likely = c("Not likely at all", "Not too likely"),
    Likely = c("Somewhat likely", "Very likely")),
    .names = "{.col}_2lvl"))

dum_28 <- dum_28 |> 
  select(rowID, contains("_2lvl")) |> 
  pewmethods::dummify_factors(sep = "_") |>  
  mutate(
    q28_1_dum = dplyr::case_when(
    q28_1_2lvl_Not_likely == 1 ~ 0,
    q28_1_2lvl_Likely == 1 ~ 1,
    TRUE ~ NA),
    q28_2_dum = dplyr::case_when(
    q28_2_2lvl_Not_likely == 1 ~ 0,
    q28_2_2lvl_Likely == 1 ~ 1,
    TRUE ~ NA),
    q28_3_dum = dplyr::case_when(
    q28_3_2lvl_Not_likely == 1 ~ 0,
    q28_3_2lvl_Likely == 1 ~ 1,
    TRUE ~ NA),
    q28_4_dum = dplyr::case_when(
    q28_4_2lvl_Not_likely == 1 ~ 0,
    q28_4_2lvl_Likely == 1 ~ 1,
    TRUE ~ NA),
    q28_5_dum = dplyr::case_when(
    q28_5_2lvl_Not_likely == 1 ~ 0,
    q28_5_2lvl_Likely == 1 ~ 1,
    TRUE ~ NA),
    .keep = "unused"
    )

df <- datawizard::data_merge(df, dum_28, join = "left", by = "rowID")





#3 way crosstable using janitor::tabyl works well
# tabyl works like this: tabyl(row_var, col_var, group_var)

# dv=q19, iv=gender, z=group
data |> 
  janitor::tabyl(gender, q19, group, show_na = F) |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |> 
  janitor::adorn_ns() |>  
  janitor::adorn_title() |>
  kableExtra::kable(format = "simple")


# by gender
data |> 
  janitor::tabyl(q7, q19, group, show_na = F) |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |> 
  janitor::adorn_ns() |>  
  janitor::adorn_title() |>
  kableExtra::kable(format = "simple")


# put column names of vars into a character vector
# exclude timing var, qset, consent, citizenship, age, and state (q4)
qs <- data |> 
  select(starts_with("q"),
         -contains("_time"),
         -contains("lucid_"), 
         -qset,
         -q1,
         -age,
         -q3,
         -q4,
         -q14,
         -q17,
         -contains("popeff_qdo")) |>
  colnames()


  









# write function to make tabyl crosstabs 
make_crab <- function(x, y, z){
  crab_df <- data |> select(z, all_of(x), all_of(y)) |>
    janitor::tabyl(group, !!sym(x), show_na = F) |>
    janitor::adorn_percentages() |>
    janitor::adorn_pct_formatting(digits = 2) |>
    janitor::adorn_ns() |> 
    janitor::adorn_title("combined")
}


# map function make_crab to all questions using the colnames character vector
combined <- map(qs, make_crab)





class(data)
class(data_spss)






