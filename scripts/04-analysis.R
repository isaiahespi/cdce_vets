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
load("data/df-2024-09-11.Rdata") # df set as factor with dummy vars
load("data/data_numeric_vets_survey-exp-2024-09-11.Rdata") # data2 numeric only
load("data/vets-survey-exp-2024-09-11_spss.Rdata") #spss data set

# load data dictionary/codebook
df_dict <- read.csv("codebooks/df_dict.csv")

dplyr::glimpse(df)

# check balance of treatment and control groups ::::::::::::::::::::::::::::####

# table of treatment and control groups. Simplest
df |> 
  janitor::tabyl(group, show_na = T) |> 
  janitor::adorn_totals("row") |> 
  janitor::adorn_pct_formatting()

# alternative. Informative
df |> 
  datawizard::data_tabulate(group)

# quick, informative, easy
df |> 
  select(group, age, gender, race, educ, milserv1, milserv2, milservfam) |> 
  datawizard::data_tabulate(drop_levels = T,
                            remove_na = F,
                            collapse = T)



# best method for html document
df |>  
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
table(df$group, df$q19)

# recall: table(row, col, z)
table(df$q7, df$q19, df$group, useNA = "ifany")

# prop.table with row percentages
# recall: prop.table(table(iv, dv, z), margin = 1) for row percentages
round(prop.table(table(df$q7, df$q19, df$group), margin = 1), 4)


# crosstab using the janitor package for comparison
# tabyl works like this: tabyl(row_var, col_var)
df |> 
  janitor::tabyl(group, q19, show_na = F) |> 
  janitor::adorn_totals("col") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2) |> 
  janitor::adorn_ns()


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

#3 way crosstable using janitor::tabyl works well
# tabyl works like this: tabyl(row_var, col_var, group_var)

# dv=q19, iv=gender, z=group
df |> 
  janitor::tabyl(gender, q19, group, show_na = F) |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |> 
  janitor::adorn_ns() |>
  janitor::adorn_title("combined") |>
  kableExtra::kable(format = "pipe")

# 
df |> 
  janitor::tabyl(q7, q19, group, show_na = F) |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |> 
  janitor::adorn_ns() |>  
  janitor::adorn_title("combined") |>
  kableExtra::kable(format = "simple")




df |> select(q5:q8) |> sjlabelled::get_label()

# q20 by awareness
df |> 
  janitor::tabyl(q5, q20.clps, group, show_na = F) |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |> 
  janitor::adorn_ns() |> 
  janitor::adorn_title("combined", row_name = "Q5", col_name = "Q20")

# q20 by election official favorability
df |> 
  janitor::tabyl(q6, q20.clps, group, show_na = F) |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |> 
  janitor::adorn_ns() |> 
  janitor::adorn_title("combined", row_name = "Q6", col_name = "Q20")

# q20 by legitimacy of 2020 election
df |> 
  janitor::tabyl(q7, q20.clps, group, show_na = F) |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |> 
  janitor::adorn_ns() |> 
  janitor::adorn_title("combined", row_name = "Q7", col_name = "Q20")

# q20 by general trust in other people
df |> 
  janitor::tabyl(q8, q20.clps, group, show_na = F) |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |> 
  janitor::adorn_ns() |> 
  janitor::adorn_title("combined", row_name = "Q7", col_name = "Q20")


df |> 
  mutate(q5.clps = datawizard::categorize(q5, "median", labels = c("Inattentive", "Attentive"))) |> 
  janitor::tabyl(q5.clps, q19.clps, group, show_na = F) |>
  janitor::adorn_percentages() |>
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns() |>
  janitor::adorn_title("combined")
  
df |> 
  mutate(q8.clps = datawizard::categorize(q8, "equal_length", n_groups = 2, labels = c("Not Trusting", "Trusting"))) |> 
  janitor::tabyl(q8.clps, q19.clps, group, show_na = F) |>
  janitor::adorn_percentages() |>
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns() |>
  janitor::adorn_title("combined")


# write function to make tabyl crosstabs 
make_crabs <- function(x, y){
  crab_df <- df |> select(all_of(x), all_of(y), group) |>
    janitor::tabyl(!!sym(x), !!sym(y), group, show_na = F) |>
    janitor::adorn_percentages() |>
    janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
    janitor::adorn_ns() |> 
    janitor::adorn_title("combined")
}


dvs_az <- df |> select(q19:q29) |> colnames()

dv_lcl <- df |> select(q30:q40_5) |> colnames()

confimpct_vars <- df |> select(q41_1:q43_6) |> colnames()

confimpct_maj_vars <- df |> select(q44_1:q46_6) |> colnames()

ex_vars <- df |> select(q5:q8) |> colnames()

demo_vars <- df |>
  select(age_cat, gender, hisp, race, educ, milserv1, milserv2, milservfam) |>
  colnames()

poli_vars <- df |> select(voted2020, choice2020, voteintent, partyid_3cat,
                            ideo, ideolean) |> colnames()

# put column names of vars into a character vector
# exclude timing var, qset, consent, citizenship, age, and state (q4)
qs <- df |> 
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

qs.clps <- df |> 
  select(contains(".clps")) |>
  colnames()

ex.clps <- df |> 
  select(q5.clps, q6.clps, q8.clps) |> 
  colnames()

sjlabelled::get_labels(df$race)
"White or Caucasian"
"Black or African American"
"American Indian"           
"Asian"                    
"Other"

ex_vars[1]
qs
ex.clps
qs.clps

# map function make_crab to all questions using the colnames character vector
q5_crabs <- map2(ex.clps[1], qs.clps, make_crabs)
q6_crabs <- map2(ex.clps[2], qs.clps, make_crabs)
q7_crabs <- map2("q7", qs.clps, make_crabs)
q8_crabs <- map2(ex.clps[3], qs.clps, make_crabs)
party_crabs <- map2("partyid_3cat", qs.clps, make_crabs)
age_crabs <- map2("age_cat", qs.clps, make_crabs)
gender_crabs <- map2("gender", qs.clps, make_crabs)
race_crabs <- map2("race", qs.clps, make_crabs)


saveRDS(q5_crabs, file = "data/q5_crabs.RData")
saveRDS(q6_crabs, file = "data/q6_crabs.RData")
saveRDS(q7_crabs, file = "data/q7_crabs.RData")
saveRDS(q8_crabs, file = "data/q8_crabs.RData")
saveRDS(party_crabs, file = "data/party_crabs.RData")


readRDS("data/q5_crabs.RData")
readRDS("data/q6_crabs.RData")
readRDS("data/q7_crabs.RData")
readRDS("data/q8_crabs.RData")
readRDS("data/party_crabs.RData")

# iv = q5, dv = q19, z = group
kableExtra::kable(q5_crabs[[1]], format = "simple")
























