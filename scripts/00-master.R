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



# create a table containing the differing statements for question sets A and B
kableExtra::kbl(qsetAB, format = 'html', align = 'l') |> 
  kableExtra::pack_rows("Regardless of whether any of these are actually the case, how would the following impact your confidence in the fairness and accuracy of elections conducted this November?", 1, 6) |>
  kableExtra::pack_rows("How would the following impact your confidence that voters are safe from violence, threats of violence, or intimidation while voting in-person during elections this November?", 7, 12) |>
  kableExtra::row_spec(c(2,9), align = 'l') |> 
  kableExtra::footnote(
    general ="Response options ranged from 'Decrease confidence a lot', 'Decrease confidence somewhat', 'No impact on confidence', 'Increase confidence somewhat', and 'Increase confidence a lot'.",
    footnote_as_chunk = T
  ) |> 
  kableExtra::kable_styling(
    font_size = 13,
    row_label_position = 'l',
    bootstrap_options = c('striped',
                          'bordered',
                          'hover',
                          'condensed',
                          'responsive'))

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####
# save variables of interest as character vectors (or objects or whatever)
# this is only so I can iterate the creation of tables and plots for each
# variable without the need to manually create a table or plot for each one.


# questions related to Maricopa County, AZ 
qs_az <- df |>
  select(
    q19,
    q20,
    q21,
    q22,
    q23,
    q24,
    q25,
    q26,
    q27
  ) |>
  colnames()

# same for collapsed versions
qs_az.clps <- df |>
  select(
    q19.clps,
    q20.clps,
    q21.clps,
    q22.clps,
    q23.clps,
    q24.clps,
    q25.clps,
    q26.clps,
    q27.clps
  ) |>
  colnames()

# expectations of fraud
fraud_exp_az <- df |>
  select(q28_1, q28_2, q28_3, q28_4, q28_5) |>
  colnames()

# expectations of fraud, collapsed versions
fraud_exp_az.clps <- df |>
  select(q28_1.clps, q28_2.clps, q28_3.clps, q28_4.clps, q28_5.clps) |>
  colnames()

# questions related to local area
qs_lcl <- df |>
  select(
    q30,
    q31,
    q32,
    q33,
    q34,
    q35,
    q36,
    q37,
    q38,
    q39
  ) |>
  colnames()

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

fraud_exp_lcl <- df |> select(q40_1:q40_5) |> colnames()

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



# Trying to make plots :::::::::::::::::::::::::::::::::::::::::::::::::::::####
# just some basic bar plots of counts and proportions


# make every level of factor vars a dummy var of its own (Update: I thought I
# needed to do this to make bar plots of factor variables. Turns out I was
# wrong. I'm keeping this anyway though because it will come in handy later)

# method 1
data.table::as.data.table(df) |> 
  select(q19) |> 
  pewmethods::dummify_factors(sep = ".")

# method 2
data.table::as.data.table(df) |> 
  select(q19) |> 
  surveytoolbox::superspread(select_helpers = everything()) |> 
  select(2:5) |> 
  sjmisc::dicho(as.num = T, append = F)



# summary of factor var, easy
summary(df$q19)

# easy way to make barplot of factor var
barplot(summary(df$q19),
        ylab = "Freq",
        xlab = "Q19. How confident are you that votes in Maricopa County, AZ will be counted as voters intend in the elections this November?")

# another way, identical result
barplot(table(df$q19, useNA = "no"),
        ylab = "Freq",
        xlab = "Q19. How confident are you that votes in Maricopa County, AZ will be counted as voters intend in the elections this November?")

# can also use barplot for proportions
barplot(prop.table(table(df$q19, useNA = "no")),
        ylab = "Freq",
        xlab = "Q19. How confident are you that votes in Maricopa County, AZ will be counted as voters intend in the elections this November?",
        ylim = c(0,1) # probably a good idea to set range from 0 to 1 for proportion
)

# barplot of counts (frequency) using ggplot2
df |> 
  ggplot(aes(x = q19))+
  geom_bar()+
  labs(y = "Freq",
       x = "Q19. How confident are you that votes in Maricopa County, AZ will be counted as voters intend in the elections this November?")

# barplot of proportions using ggplot2
df |> 
  ggplot(aes(x = q19, y = ..count../sum(..count..)))+
  geom_bar()+
  labs(y = "Prop",
       x = "Q19. How confident are you that votes in Maricopa County, AZ will be counted as voters intend in the elections this November?")

# We can also use barplot() to illustrate the relationship between two
# categorical variables.
# If beside = F, then result will output a stacked bar chart
barplot(table(df$group, df$q19),
        beside = T,
        legend.text = T,
        xlab = "Q19. How confident are you that votes in Maricopa County, AZ will be counted as voters intend in the elections this November?",
        ylab = 'Freq',
        main = 'Q19 by Group')

# Proportion chart
# recall: when margin =1 for prop.table, then prop.table(table(iv, dv), margin = 1)
barplot(prop.table(table(df$group, df$q19), margin = 1),
        beside = T,
        legend.text = T,
        xlab = "Q19. How confident are you that votes in Maricopa County, AZ will be counted as voters intend in the elections this November?",
        ylab = 'Proportion',
        main = 'Q19 by Group',
        ylim = c(0, 1))

# Proportion chart, where bars sum to 100% within clusters (i.e., within groups)
# recall: when margin =2 for prop.table, then prop.table(table(dv, iv), margin = 2)
# recall: when margin =1 for prop.table, then prop.table(table(iv, dv), margin = 1)
barplot(prop.table(table(df$q19, df$group), margin = 2),
        beside = T,
        legend.text = T,
        xlab = "Q19. How confident are you that votes in Maricopa County, AZ will be counted as voters intend in the elections this November?",
        ylab = 'Proportion',
        main = 'Q19 by Group',
        ylim = c(0, 0.5))

# Base R barplot() requires, as input, a table to compute the counts or
# proportions. In ggplot(), however, you can input the variables directly.
# However, as shown below, to get a proportion chart, we still need to compute
# the proportions first.

# NOTE: "fill" tells ggplot which variable is filled with different colors
df |>  
  # By default, geom_bar() will plot a bar for NA values
  # Filter them out to prevent this
  # filter(!is.na(y) & !is.na(x))  |> 
  
  ggplot(aes(x = q19, fill = group)) +
  
  # Without position = "dodge", output will be a stacked bar chart
  geom_bar(position = "dodge") +
  
  # \n in a string tells R to break the line there
  labs(y = "Frequency",
       x = "Q19",
       fill = "Group\nExperiment\nCondition")


df |> 
  select(group, q19) |> 
  # compute frequency within each combination
  group_by(group, q19) |> 
  count() |> 
  # compute proportion within group
  # n is the default variable created by count()
  group_by(group) |> 
  mutate(Proportion = n/sum(n)) |> 
  
  # add y to inform geom_bar what to put in y-axis
  ggplot(aes(x = q19, y = Proportion, fill = group))+
  geom_bar(position = 'dodge', stat = 'identity')+
  labs(y = "Prop",
       x = "Q19",
       fill = "Group\nExperiment\nCondition")


# crosstab using the janitor package for comparison
# tabyl works like this: tabyl(row_var, col_var)
df |> 
  janitor::tabyl(group, q19, show_na = F) |> 
  tidyr::pivot_longer(
    cols = -group,
    names_to = 'response',
    values_to = 'n'
  ) |> 
  ggplot(aes(y = n, fill = group, x = response))+
  geom_col(position = 'dodge')+
  # stat_count(geom = 'text', aes(label = ..count..), vjust = -0.5)+
  geom_text(aes(label = n), position = position_dodge(1.0), vjust = -0.5)+
  scale_fill_grey(start = 0.5, end = 0.1)+
  theme_bw()

# function to create crosstabs using janitor::tabyl()
tabyl.crabs <- function(x){
  crab_df <- df |> select(group, all_of(x)) |> 
    janitor::tabyl(group, !!sym(x), show_na = F)
}

az_crabs <- map(qs_az, tabyl.crabs)
az_crabs.clps <- map(qs_az.clps, tabyl.crabs)


# run the following to get the list into one dataframe
# warning: questions are no longer apparent
purrr::reduce(az_crabs, dplyr::full_join)
purrr::reduce(az_crabs.clps, dplyr::full_join)

# print the plot
az_crabs[[1]] |> 
  tidyr::pivot_longer(
    cols = -group,
    names_to = 'response',
    values_to = 'n'
  ) |> 
  ggplot(aes(y = n, fill = group, x = response))+
  geom_col(position = 'dodge')+
  # stat_count(geom = 'text', aes(label = ..count..), vjust = -0.5)+
  geom_text(aes(label = n), position = position_dodge(1.0), vjust = -0.5)+
  scale_fill_grey(start = 0.5, end = 0.1)+
  # \n in a string tells R to break the line there
  labs(y = "Count",
       x = "Q19. How confident are you that votes in Maricopa County, AZ \nwill be counted as voters intend in the elections this November?",
       fill = "Group\nExperiment\nCondition")+
  theme_bw()
  
# print the crab
az_crabs[[1]] |> 
  janitor::adorn_totals('both') |> 
  janitor::adorn_percentages(denominator = 'row') |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |> 
  janitor::adorn_ns() |> 
  janitor::adorn_title(
    'combined',
    row_name = "Group",
    col_name = 'Q19') |> 
  kableExtra::kbl() |> 
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "bordered", "condensed", "responsive"),
    latex_options = "basic",
    font_size = 13
  )

# prop plot of q19, collapsed
az_crabs.clps[[1]] |> 
  tidyr::pivot_longer(
    cols = -group,
    names_to = 'response',
    values_to = 'n'
  ) |> 
  dplyr::mutate(prop = round(n/sum(n), 2)) |> 
  ggplot(aes(y = prop, fill = group, x = response))+
  geom_col(position = 'dodge')+
  # stat_count(geom = 'text', aes(label = ..count..), vjust = -0.5)+
  geom_text(aes(label = prop), position = position_dodge(1.0), vjust = -0.5)+
  scale_fill_grey(start = 0.5, end = 0.1)+
  # \n in a string tells R to break the line there
  labs(y = "Proportion",
       x = "Q19. How confident are you that votes in Maricopa County, AZ \nwill be counted as voters intend in the elections this November?",
       fill = "Group\nExperiment\nCondition")+
  theme_bw()

# print the collapsed q19 crab
az_crabs.clps[[1]] |> 
  janitor::adorn_totals('both') |> 
  janitor::adorn_percentages(denominator = 'row') |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |> 
  janitor::adorn_ns() |> 
  janitor::adorn_title(
    'combined',
    row_name = "Group",
    col_name = 'Q19') |> 
  kableExtra::kbl() |> 
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "bordered", "condensed", "responsive"),
    latex_options = "basic",
    font_size = 13
  )

# crab of Q22 by treatment group
az_crabs[[4]] |> 
  janitor::adorn_totals('both') |> 
  janitor::adorn_percentages(denominator = 'row') |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |> 
  janitor::adorn_ns() |> 
  janitor::adorn_title(
    'combined',
    row_name = "Group") |> 
  kableExtra::kbl() |> 
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "bordered", "condensed", "responsive"),
    latex_options = "basic",
    font_size = 13
  )

# collapsed crab of Q22 by treatment group
az_crabs.clps[[4]] |> 
  janitor::adorn_totals('both') |> 
  janitor::adorn_percentages(denominator = 'row') |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |> 
  janitor::adorn_ns() |> 
  janitor::adorn_title(
    'combined',
    row_name = "Group") |> 
  kableExtra::kbl() |> 
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "bordered", "condensed", "responsive"),
    latex_options = "basic",
    font_size = 13
  )




az_crabs[[4]] |> 
  tidyr::pivot_longer(
    cols = -group,
    names_to = 'response',
    values_to = 'n'
  ) |> 
  dplyr::mutate(prop = round(n/sum(n), 2)) |> 
  ggplot(aes(y = prop, fill = group, x = response))+
  geom_col(position = 'dodge')+
  # stat_count(geom = 'text', aes(label = ..count..), vjust = -0.5)+
  geom_text(aes(label = prop), position = position_dodge(1.0), vjust = -0.5)+
  scale_fill_grey(start = 0.5, end = 0.1)+
  # \n in a string tells R to break the line there
  labs(y = "Proportion",
       x = "Q22.",
       fill = "Group\nExperiment\nCondition")+
  theme_bw()

## conducting prop.tests across multiple variables to get a better sense of treatment effects
# Next plan is to produce dot-and-whisker plots to demonstrate effect of treatment

# function to run prop_tests over a list of crosstabs 
tabyl.prop.tests <- function(x){
  crab_df <- df |> select(group, all_of(x)) |> 
    janitor::tabyl(group, !!sym(x), show_na = F) |> 
    select(-1) |> 
    rstatix::prop_test(detailed = T)
}

df |> 
  select(group, q26.clps) |> 
  mutate(q25.clps = fct_rev(q25.clps)) |>
  janitor::tabyl(group, q25.clps, show_na = F) |> 
  select(-1) |> 
  rstatix::prop_test(detailed = T, correct = F, conf.level = 0.95) |> 
  mutate(prop.diff = estimate1 - estimate2, .before = statistic)

df |> 
  select(group, q26) |> 
  # mutate(q26.clps = fct_rev(q26.clps)) |>
  janitor::tabyl(group, q26, show_na = F) |> 
  janitor::adorn_percentages('row') |> 
  janitor::adorn_pct_formatting(digits = 3) |> 
  janitor::adorn_ns()

df |> 
  select(group, q26.clps) |> 
  # mutate(q26.clps = fct_rev(q26.clps)) |>
  janitor::tabyl(group, q26.clps, show_na = F) |> 
  janitor::adorn_percentages('row') |> 
  janitor::adorn_pct_formatting(digits = 3) |> 
  janitor::adorn_ns()

df |> select(group, q26) |> 
  group_by(group) |> 
  sjmisc::frq(q26, show.na = F)

RCPA3::testpropsC(dv = q26.clps, iv = group, data = df, response = "Not_confident", xlim = c(-0.2, 0.2))

# this maps the function tabyl.prop.tests to each character string in 'x'
# the results are put into a list of tibbles, in this case, a list of 4 tibbles
# Note that prop.tests can only be conducted for collapsed dichotomous variables
az_crabs.clps.prop.tests <- map(qs_az.clps, tabyl.fun)

# I combine the 4 tibbles within the list into a single tibble
az_crabs.clps.prop.tests <- purrr::reduce(law_skool_prop_tests, dplyr::full_join)

# add column that identifies rows of different prop_tests
az_crabs.clps.prop.tests$items <- qs_az.clps


# relocate columns to better positions, rename columns, remove df column
az_crabs.clps.prop.tests <- az_crabs.clps.prop.tests |>
  dplyr::relocate(c(items, response), .before = n) |>
  dplyr::relocate(p.signif, .after = p) |>
  dplyr::mutate(prop.diff = estimate1 - estimate2,
                .before = statistic) |> 
  select(-df)


az_crabs[[5]] # q23
az_crabs[[7]] # q25
az_crabs[[8]] # q26


df |> 
  select(group, q26) |> 
  # mutate(q26.clps = fct_rev(q26.clps)) |>
  janitor::tabyl(group, q26, show_na = F) |> 
  janitor::adorn_percentages('row') |> 
  janitor::adorn_pct_formatting(digits = 1) |> 
  janitor::adorn_ns() |> 
  janitor::adorn_title(
    'combined',
    row_name = "Group",
    col_name = 'Q26') 
  kableExtra::kbl() |> 
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "bordered", "condensed", "responsive"),
    latex_options = "basic",
    font_size = 13
  )


az_crabs[[8]] |> 
  tidyr::pivot_longer(
    cols = -group,
    names_to = 'response',
    values_to = 'n'
  ) |> 
  ggplot(aes(y = n, fill = group, x = response))+
  geom_col(position = 'dodge')+
  # stat_count(geom = 'text', aes(label = ..count..), vjust = -0.5)+
  geom_text(aes(label = n), position = position_dodge(1.0), vjust = -0.5)+
  scale_fill_grey(start = 0.5, end = 0.1)+
  # \n in a string tells R to break the line there
  labs(y = "Count",
       x = "Q26. How confident, if at all, are you that in person polling places in Maricopa County, AZ \nwill be safe places for voters to cast their ballots during the upcoming elections in November?",
       fill = "Group\nExperiment\nCondition")+
  theme_bw()


df |> 
  select(group, q26.clps) |> 
  # mutate(q25.clps = fct_rev(q25.clps)) |>
  janitor::tabyl(group, q26.clps, show_na = F) |> 
  janitor::adorn_percentages('row') |> 
  janitor::adorn_pct_formatting(digits = 2) |> 
  janitor::adorn_ns() |>  
  janitor::adorn_title(
    'combined',
    row_name = "Group") 
  kableExtra::kbl() |> 
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "bordered", "condensed", "responsive"),
    latex_options = "basic",
    font_size = 13
  )

az_crabs.clps[[8]] |>
  tidyr::pivot_longer(
    cols = -group,
    names_to = 'response',
    values_to = 'n'
  ) |> 
  dplyr::mutate(prop = round(n/sum(n), 2)) |> 
  ggplot(aes(y = prop, fill = group, x = forcats::fct_rev(response)))+
  geom_col(position = 'dodge')+
  # stat_count(geom = 'text', aes(label = ..count..), vjust = -0.5)+
  geom_text(aes(label = prop), position = position_dodge(1.0), vjust = -0.5)+
  scale_fill_grey(start = 0.5, end = 0.1)+
  # \n in a string tells R to break the line there
  labs(y = "Proportion",
       fill = "Group\nExperiment\nCondition")+
  theme_bw()



