# analysis

# load packages
library(tidyverse)
library(janitor)
library(kableExtra)
# library(gtsummary)
# library(gt)
# library(labelled)
# library(sjlabelled)
# library(rlang)
# library(sjmisc)
# library(sjPlot)
# library(sjstats)
# library(pewmethods)
# library(surveytoolbox)
# library(expss)
# library(easystats)

# load data
load("data/df-2024-09-11.Rdata") # df set as factor with dummy vars
load("data/data_numeric_vets_survey-exp-2024-09-11.Rdata") # data2 numeric only
load("data/vets-survey-exp-2024-09-11_spss.Rdata") #spss data set

# load raw SPSS export that includes display order vars
# downloaded 2024-09-11 at 11:38 AM
raw_spss <- haven::read_sav(file = "data-raw/vets_spss_do_2024-09-11_11.38.sav") 


# load data dictionary/codebook
df_dict <- read.csv("codebooks/df_dict.csv")

# also load the raw data dict for comparison
raw_spss_dict <- read.csv("codebooks/raw_spss_dict.csv")



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
  select(group,
         age_cat,
         gender_3cat,
         race,
         educ,
         milserv1,
         milserv2,
         milservfam) |>
  datawizard::data_tabulate(drop_levels = T,
                            remove_na = F,
                            collapse = T)


# best method ended up being the descriptive table from the `table1` R package



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


# table of treatment and control groups
df |>
  gtsummary::tbl_summary(
    by = group,
    include = c(age, gender, race, educ, milserv1, milserv2, milservfam),
    statistic = list(
      age ~ "{mean} ({sd})",
      gtsummary::all_categorical() ~ "{n} ({p}%)"
    ),
    type = list(
      milserv1 ~ "categorical",
      milserv2 ~ "categorical",
      milservfam ~ "categorical"
    ),
    missing = "ifany"
  ) |>
  gtsummary::add_n() |>
  gtsummary::add_overall() |>
  gtsummary::modify_header(label = "**Variable**") |>
  gtsummary::bold_labels() |>
    gtsummary::as_kable_extra(
      booktabs = T,
      longtable = T
    ) |> 
    kableExtra::kable_styling(
      position = "left",
      fixed_thead = T,
      font_size = 14,
      bootstrap_options = c("striped", "hover", "responsive")
    ) |> 
  kableExtra::footnote(general = "Failure to complete survey, n = 120") |> 
  kableExtra::scroll_box(height = "400px")





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
round(prop.table(table(df$q6.clps, df$q21.clps, df$group), margin = 1), 4)

# crosstab using the janitor package for comparison
# tabyl works like this: tabyl(row_var, col_var)
df |> 
  janitor::tabyl(group, q19, show_na = F) |> 
  janitor::adorn_totals("col") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2) |> 
  janitor::adorn_ns()


## This tabyl to gt style table works well. It functions as a good table to use
## for html display. My goal is to have one dv question span above the columns,
## and multiple iv questions of interest are in the rows where the rows are
## grouped by experiment condition. The table below doesn't do that but seems to
## work well as a 3-way cross table.

df |> 
  janitor::tabyl(q5.clps, q19.clps, group, show_na = F) |> 
  janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("top", 
                       row_name = "Q5",
                       col_name = "Q19") |>
  bind_rows(.id = "group_var") |>   
  gt::gt(
    rowname_col = "row_var",
    groupname_col = "group_var") |>  
  gt::row_group_order(groups = c("Control", "Treatment")) |> 
  gt::cols_label(c('...3', '...4') ~ " ",
                 "Q19" ~ " ") |>
  gt::cols_label_with(columns = '...1', fn = ~sjlabelled::get_label(df$q5.clps)) |> 
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups()
  ) |> 
  gt::tab_spanner(
    columns = 3:4, label = "Q19. Vote count confidence, AZ"
  ) |> 
  gt::tab_footnote(
    footnote = "Table reflects row percentages. NAs omitted") |> 
  gt::tab_footnote(
    footnote = "Q19. How confident are you that votes in Maricopa County, AZ will be counted as voters intend in the elections this November?",
                   locations = gt::cells_column_spanners()) |> 
  gt::tab_footnote(
    footnote = "Q5. How often do you pay attention to what is going on in government and politics?",
    locations = gt::cells_column_labels(columns = "...1")) |> 
  gt::tab_options(
    table.font.size = "small",
    data_row.padding = gt::px(1)
  )


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

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# save variables of interest as character vectors (or objects or whatever)
# questions related to Maricopa County, AZ 
qs_az <- df |> select(
  q19.clps,
  q20.clps,
  q21.clps,
  q22.clps,
  q23.clps,
  q24.clps,
  q25.clps,
  q26.clps,
  q27.clps,
  q28_1.clps,
  q28_2.clps,
  q28_3.clps,
  q28_4.clps,
  q28_5.clps,
  q29.clps
) |> colnames()

# questions related to local area 
qs_lcl <- df |>
  select(
    q30.clps,
    q31.clps,
    q32.clps,
    q33.clps,
    q34.clps,
    q35.clps,
    q36.clps,
    q37.clps,
    q38.clps,
    q39.clps
  ) |>
  colnames()

# questions q41 to q43 confidence impact questions
confimpct_qs <- df |> select(q41_1.clps:q43_6.clps) |> colnames()

# questions q44 to q46 confidence impact questions, majority language
confimpct_maj_vars <- df |> select(q44_1.clps:q46_6.clps) |> colnames()


political_qs <- df |> select(voted2020.clps, choice2020, voteintent, partyid_3cat,
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

# do the same for the factor variables where levels have been collapsed
# excludes q5 through q8
qs.clps <- df |> 
  select(contains(".clps"), -q5.clps, -q6.clps, -q7, -q8.clps) |>
  colnames()


# now only the collapsed versions of q5 to q8 (q7 doesn't need collapsing)
ex.clps <- df |>
  select(q5.clps, q6.clps, q7, q8.clps) |>
  colnames()


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# I got some odd different values when using base R tables, at least for
# proportions. I think because my tabyls were omitting NA. Gonna have to look
# into this later.
table(df$q5.clps, df$q19.clps, useNA = "ifany")
round(prop.table(table(df$q5.clps, df$q19.clps, useNA = "ifany"), margin = 1), 2)

table(df$q5.clps, df$q19.clps, df$group, useNA = "ifany")
round(prop.table(table(df$q5.clps, df$q19.clps, df$group, useNA = "ifany"), margin = 2), 2)
proportions(table(df$q5.clps, df$q19.clps, df$group, useNA = "ifany"), margin = 2)

df |>  
  sjmisc::flat_table(group, q5.clps, q19.clps, margin = "row",
                     digits = 2, show.values = T)


df |>
  janitor::tabyl(q5.clps, q19.clps, group, show_na =F ) |>
  janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |>
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |> 
  janitor::adorn_ns() |> 
  janitor::adorn_title("top")

df |> 
  janitor::tabyl(q6.clps, q21.clps, group, show_na = F) |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_title("top") 


df |> 
  janitor::tabyl(q5.clps, q19.clps, group, show_na = F) |> 
  janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |>
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |> 
  janitor::adorn_ns() |> 
  janitor::adorn_title("top") |> 
  dplyr::bind_rows(.id = "group_var") |> 
  gt::gt(
    rowname_col = "row_var",
    groupname_col = "group_var"
  )
  


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####
# merge question sets that show no significant difference

df |> 
  select(rowID, qset,
         q41_1:q41_4, 
         q43_1:q43_4, 
         q44_1, q44_4, 
         q44_5, q44_2, 
         q46_1, q46_4, 
         q46_6, q46_2) |>
  # this filters out all the NAs from the qset var, n = 105
  filter(!is.na(qset)) |>
  # idk why this works, but its the sum of NA values per variable minus the NA
  # from the corresponding set. So NAs independent of the other sample.
  summarise_all(~sum(is.na(.) - !is.na(.)))


# coalesce the qset vars that can be merged. This combines/coalesces some vars
# from the q44 and q46 set (minus statements regarding lawyers and students)
# into q41 and q43 since there was no significant difference found between
# question set A and question set B in proportions of responses categories.
df <- df |> 
  group_by(rowID) |> 
  mutate(q41.1 = dplyr::coalesce(q41_1, q44_1),
         q41.2 = dplyr::coalesce(q41_2, q44_4),
         q41.3 = dplyr::coalesce(q41_3, q44_5),
         q41.4 = dplyr::coalesce(q41_4, q44_2),
         q43.1 = dplyr::coalesce(q43_1, q46_1),
         q43.2 = dplyr::coalesce(q43_2, q46_4),
         q43.3 = dplyr::coalesce(q43_3, q46_6),
         q43.4 = dplyr::coalesce(q43_4, q46_2)) |>
  ungroup() 


df |> 
  select(rowID, qset,
         q41.1:q41.4, 
         q43.1:q43.4) |>
  # this filters out all the NAs from the qset var, n = 105
  filter(!is.na(qset)) |>
  # idk why this works, but its the sum of NA values per variable minus the NA
  # from the corresponding set. So NAs independent of the other sample.
  summarise_all(~sum(is.na(.)))

# another way to check. number of observations for merged var should be sum of
# both q41_1 and q44_1. Total NA in new merged var should be enough to equal
# sample n of 1283.
df |> select(rowID, qset, q41_1, q44_1, q41.1) |>
  filter(!is.na(qset)) |>
  summarise(
    n = sum(length(rowID)),
    'q41.1_n' = sum(!is.na(q41.1)),
    'q41.1 na' = sum(is.na(q41.1)),
    'q41_1 n' = sum(!is.na(q41_1)),
    'q41_1 na' = sum(is.na(q41_1)-!is.na(q44_1)),
    'q44_1 n' = sum(!is.na(q44_1)),
    'q44_1 na' = sum(is.na(q41_1)-!is.na(q44_1))
  )


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####
# conduct a 2-sample test of independence on two different proportions

# get raw counts of increase v no increase in confidence
# mutate vars to dichotomous,
# select two columns from tabyl object,
# then pipe into rstatix::prop_test() 
df |>
  mutate(
    confimpct_vets = dplyr::case_when(
      q41.4.clps == "increase" ~ 'increase',
      q41.4.clps == "no_impact" ~ 'no_increase',
      q41.4.clps == 'decrease' ~ 'no_increase'
      ),
    group = dplyr::case_when(
      group == "Treatment" ~ 1,
      group == "Control" ~ 0
    )
    ) |>
  janitor::tabyl(group, confimpct_vets,  show_na = F) |> 
  select(increase, no_increase) |> 
  rstatix::prop_test(detailed = T)


df |>
  mutate(
    confimpct_vets = forcats::fct_recode(
      q41.1.clps,
      'increase' = 'increase',
      'no increase' = 'no_impact', 
      'no increase' = 'decrease')
    ) |>
  select(
    group,
    confimpct_vets,
    q19.clps,
    q20.clps,
    q21.clps,
    q22.clps,
    q23.clps,
    q24.clps,
    q25.clps,
    q26.clps,
    q27.clps,
  ) |>
  group_by(group) 


#### 
# create a table containing the differing statements for question sets A and B

qsetA <- df |> 
  select(q41_1:q46_6) |> 
  labelled::set_variable_labels(
    q41_1 = "Election officials test machines",
    q41_2 = "Election officials conduct audits",
    q41_3 = "Partisan Poll watchers observe the election.",
    q41_4 = "Election staff include veterans and family",
    q41_5 = "Election staff include lawyers",
    q41_6 = "Election staff include college students",
    q43_1 = "Law enforcement presence.",
    q43_2 = "Partisan Poll watchers observe the election",
    q43_3 = "People holding signs or giving out literature",
    q43_4 = "Election staff includes veterans",
    q43_5 = "Election staff includes lawyers",
    q43_6 = "Election staff includes students"
    ) |> 
  surveytoolbox::varl_tb() |> 
  select(var, var_label) |> 
  dplyr::slice(1:12) |> 
  dplyr::rename('Qset A' = var,
                'A statements' = var_label)

qsetB <- df |> 
  select(q41_1:q46_6) |> 
  labelled::set_variable_labels(
    q44_1 = "Election officials test machines",
    q44_2 = "Majority of Election staff are veterans",
    q44_3 = "Majority of Election staff are lawyers",
    q44_4 = "Election officials conduct audits",
    q44_5 = "Partisan Poll watchers observe the election",
    q44_6 = "Majority of Election staff are students",
    q46_1 = "Law enforcement presence.",
    q46_2 = "Majority of Election staff are veterans",
    q46_3 = "Majority of Election staff are lawyers",
    q46_4 = "Partisan Poll watchers observe the election",
    q46_5 = "Majority of Election staff are students",
    q46_6 = "People holding signs or giving out literature"
    ) |> 
  surveytoolbox::varl_tb() |> 
  select(var, var_label) |> 
  dplyr::slice(13:24) |> 
  dplyr::rename('Qset B' = var,
                'B statements' = var_label)

# bind the dataframes together as one with two columns
qsetAB <- dplyr::bind_cols(qsetA, qsetB)
rm(qsetA, qsetB)


kableExtra::kbl(qsetAB) |> 
  kableExtra::pack_rows("Fairness and Accuracy", 1, 6) |> 
  kableExtra::pack_rows("Voter Safety", 7, 12) |> 
  kableExtra::kable_styling(
    bootstrap_options = c('striped',
                          'bordered',
                          'hover',
                          'condensed',
                          'responsive'))


kableExtra::kbl(qsetAB) |> 
  kableExtra::pack_rows("Regardless of whether any of these are actually the case, how would the following impact your confidence in the fairness and accuracy of elections conducted this November?", 1, 6) |> 
  kableExtra::pack_rows("How would the following impact your confidence that voters are safe from violence, threats of violence, or intimidation while voting in-person during elections this November?", 7, 12) |> 
  kableExtra::kable_styling(
    bootstrap_options = c('striped',
                          'bordered',
                          'hover',
                          'condensed',
                          'responsive'))

Q41_1. Election officials test every machine used in the election to ensure they are secure.
Q41_2. Election officials conduct audits of ballots after every election...
Q41_3. Poll watchers affiliated with the political parties or candidates observe the election.
Q41_4. Election staff and volunteers include military veterans and their family members from the community.
Q41_5. Election staff and volunteers include lawyers from the community.
Q41_6. Election staff and volunteers include college students from the community.
Q43_1. Election officials ensure that law enforcement officers are present at polling sites.
Q43_2. Poll watchers affiliated with the political parties or candidates observe the election.
Q43_3. People holding signs or giving out literature in support of a candidate or ballot question.
Q43_4. Election staff and volunteers include military veterans and their family members from the community.
Q43_5. Election staff and volunteers include lawyers from the community.
Q43_6. Election staff and volunteers include college students from the community.
Q44_1. Election officials test every machine used in the election to ensure they are secure.
Q44_2. The majority of election staff and volunteers consist of military veterans and their family members from the community.
Q44_3. The majority of election staff and volunteers consist of lawyers from the community.
Q44_4. Election officials conduct audits of ballots after every election to confirm the results were accurate.
Q44_5. Poll watchers affiliated with the political parties or candidates observe the election.
Q44_6. The majority of election staff and volunteers consist of college students from the community.
Q46_1. Election officials ensure that law enforcement officers are present at polling sites.
Q46_2. The majority of election staff and volunteers consist of military veterans and their family members from the community.
Q46_3. The majority of election staff and volunteers consist of lawyers from the community.
Q46_4. Poll watchers affiliated with the political parties or candidates observe the election.
Q46_5. The majority of election staff and volunteers consist of college students from the community.
Q46_6. People holding signs or giving out literature in support of a candidate or ballot question.
  






