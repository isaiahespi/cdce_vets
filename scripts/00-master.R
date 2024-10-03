# analysis

# load packages
library(tidyverse)
# library(janitor)
# library(kableExtra)
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

# load data dictionary/codebook
df_dict <- read.csv("codebooks/df_dict.csv")

# load raw SPSS export that includes display order vars
# downloaded 2024-09-11 at 11:38 AM
raw_spss <- haven::read_sav(file = "data-raw/vets_spss_do_2024-09-11_11.38.sav") 

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

## This tabyl to gt style table works well. It functions as a good table to use
## for html display. My goal is to have one dv question span above the columns,
## and multiple iv questions of interest are in the rows where the rows are
## grouped by experiment condition. The table below doesn't do that but seems to
## work well as a 3-way cross table.

df |> 
  janitor::tabyl(q5, q19, group, show_na = F) |> 
  # janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Q5. Attention to Political Affairs",
                       col_name = "Q19") |>
  bind_rows(.id = "group_var") |>   
  gt::gt(
    rowname_col = "row_var",
    groupname_col = "group_var") |>  
  gt::row_group_order(groups = c("Control", "Treatment")) |> 
  gt::cols_label(
    c('Q5. Attention to Political Affairs/Q19') ~ "Q5. Attention to Political Affairs ") |>
  # gt::cols_label_with(columns = '...1', fn = ~sjlabelled::get_label(df$q5.clps)) |> 
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups()
  ) |> 
  gt::tab_style(
    style = gt::cell_text(weight = 'bold'),
    locations = gt::cells_column_labels()
  ) |> 
  gt::tab_spanner(
    columns = 3:6,
    label = "Q19. Vote Count Confidence for Maricopa County, AZ"
  ) |> 
  gt::tab_footnote(
    footnote = "Table reflects row percentages. NAs omitted") |> 
  gt::tab_footnote(
    footnote = "Q19. How confident are you that votes in Maricopa County, AZ will be counted as voters intend in the elections this November?",
    locations = gt::cells_column_spanners()) |> 
  gt::tab_footnote(
    footnote = "Q5. How often do you pay attention to what is going on in government and politics?",
    locations = gt::cells_column_labels('Q5. Attention to Political Affairs/Q19')
    ) |> 
  gt::cols_align(align = 'left', columns = everything()) |> 
  gt::tab_options(
    table.font.size = "small",
    data_row.padding = gt::px(1)
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
  select(-1) |> 
  rstatix::prop_test(detailed = T)

# t-test comparison of means between treatment and control group ::::::::::####
# use lm() to confirm

temp <- df |>
  mutate(
    confimpct_vets = dplyr::case_when(
      q41.4.clps == "increase" ~ 1,
      q41.4.clps == "no_impact" ~ 0,
      q41.4.clps == 'decrease' ~ 0
      ))
  

lm(confimpct_vets ~ group, data = temp) |> summary() |> broom::tidy()
plot(parameters::parameters(lm(confimpct_vets ~ group, data = temp)))
rstatix::t_test(temp, formula = confimpct_vets ~ group, paired = F, ref.group = 'Control', detailed = T)
summary(lm(q19_dum~group, data = df))


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


#### :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####
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
# just some basic bar plots of counts and proportions by treatment condition


df |> 
  # compute frequency within each combination
  group_by(group, q19) |> 
  count() |>  
  # compute proportion within group
  # n is the default variable created by count()
  group_by(group) |> 
  mutate(prop = round(n/sum(n), digits = 3),
         pct = prop*100,
         res = str_c(pct,'% (', n, ')', sep = "")) |> 
  ggplot(aes(x = q19, y = pct, fill = group))+
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_text(aes(label = res), position = position_dodge(1.0), size = 3.5, vjust = -0.5)+
  scale_fill_grey(start = 0.5, end = 0.1)+
  # \n in a string tells R to break the line there
  labs(y = "Percentage",
       x = "Q19. How confident are you that votes in Maricopa County, AZ \nwill be counted as voters intend in the elections this November?",
       fill = "Group\nExperiment\nCondition")+
  theme_bw()

# crosstab using the janitor package for comparison
# identical output
df |> 
  janitor::tabyl(group, q19, show_na = F) |> 
  tidyr::pivot_longer(
    cols = -group,
    names_to = 'response',
    values_to = 'n'
  ) |>
  group_by(group) |> 
  mutate(prop = round(n/sum(n),3),
         pct = prop*100,
         res = str_c(pct,'% (', n, ')', sep = "")) |> 
  ggplot(aes(x = response, y = pct, fill = group))+
  geom_col(position = 'dodge')+
  # stat_count(geom = 'text', aes(label = ..count..), vjust = -0.5)+
  geom_text(aes(label = res), position = position_dodge(1.0), vjust = -0.5)+
  scale_fill_grey(start = 0.5, end = 0.1)+
  theme_bw()



## conducting prop.tests across multiple variables to get a better sense of treatment effects

# function to run prop_tests over a list of crosstabs 
tabyl.prop.tests <- function(x){
  crab_df <- df |> select(group, all_of(x)) |> 
    janitor::tabyl(group, !!sym(x), show_na = F) |> 
    select(-1) |> 
    rstatix::prop_test(detailed = T)
}

# treatment effects for Q41 and Q43 ::::::::::::::::::::::::::::::::::::::::####

# Q41.4 (vets), Q41.5 (lawyers), Q41.6 (students), by treatment
df |> 
  ggstats::gglikert(include = c(q41.4:q41.6), y = 'group',  symmetric = T, add_totals = F)+ 
  facet_grid(rows = vars(.question), labeller = label_wrap_gen(15))+
  labs(
    title = "Impact on Confidence in Election Fairness and Accuracy by Experiment Condition",
    subtitle = "Q41. How would the following impact your confidence in the fairness and accuracy of elections conducted this November?"
  )+
  theme_bw()+
  theme(legend.position = 'bottom')

# save plot to images folder in project dir
ggplot2::ggsave(filename = 'q41_by_treatment_likert_plot.png', path = '~/R/GA_work/CDCE/cdce_vets/images')


# Q43.4 (vets), Q43.5 (lawyers), Q43.6 (students) by treatment 
df |> 
  ggstats::gglikert(include = c(q43.4:q43.6), y = 'group', symmetric = T, add_totals = F)+ 
  facet_grid(rows = vars(.question), labeller = label_wrap_gen(15))+
  labs(
    title = "Impact on Confidence in Voter Safety at Polling Sites by Experiment Condition",
    subtitle = "Q43. How would the following impact your confidence that voters are safe from violence, threats of violence, \nor intimidation while voting in-person during elections this November?"
  )+
  theme_bw()+
  theme(legend.position = 'bottom')

# save plot to images folder in project dir
ggplot2::ggsave(filename = 'q43_by_treatment_likert_plot.png', path = '~/R/GA_work/CDCE/cdce_vets/images')



# comparing differences within control group only
df |>  
  ggstats::gglikert(include = c(q41.4:q41.6), y = 'group',  symmetric = T, add_totals = F)+ 
  facet_grid(rows = vars(.question), labeller = label_wrap_gen(15))+
  labs(
    title = "Impact on Confidence in Election Fairness and Accuracy within Control Group Only",
    subtitle = "Q43. How would the following impact your confidence that voters are safe from violence, threats of violence, \nor intimidation while voting in-person during elections this November?"
  )+
  theme_bw()+
  theme(legend.position = 'bottom')


df |> 
  filter(group == "Control") |> 
  ggstats::gglikert(include = c(q43.4:q43.6), y = 'group',  symmetric = T, add_totals = F)+ 
  facet_grid(rows = vars(.question), labeller = label_wrap_gen(15))+
  labs(
    title = "Impact on Confidence in Voter Safety at Polling Sites within Control Group Only",
    subtitle = "Q43. How would the following impact your confidence that voters are safe from violence, threats of violence, \nor intimidation while voting in-person during elections this November?"
  )+
  theme_bw()+
  theme(legend.position = 'bottom')







# combine multiple coef plots into one plot ::::::::::::::::::::::::::::::::####
# show coef plot when q41.4 is converted to a dummy variable where 1 =
# 'Increase' and 0 = "No increase"

# create dummy variables where 1 = 'Increase in confidence' and 0 = "No increase
# in confidence".
# add dummy variables to dataframe
df <- df |> 
  mutate(across(c(q41.1.clps:q43.6.clps), 
                ~ dplyr::case_when(
                . == "increase" ~ 1, 
                . == "no_impact" ~ 0,
                . == 'decrease' ~ 0
                ), .names = "{.col}.dum")) 

mod.q41.1 <- lm(formula = q41.1.clps.dum ~ group, data = df)
mod.q41.2 <- lm(formula = q41.2.clps.dum ~ group, data = df)
mod.q41.3 <- lm(formula = q41.3.clps.dum ~ group, data = df)
mod.q41.4 <- lm(formula = q41.4.clps.dum ~ group, data = df)
mod.q41.5 <- lm(formula = q41.5.clps.dum ~ group, data = df)
mod.q41.6 <- lm(formula = q41.6.clps.dum ~ group, data = df)

# place all the models in a list assigned as 'models' and include them in the
# ggstats::ggcoef_compare() function
models.q41 <- list(
  "Test Every Machine" = mod.q41.1,
  "Audit Ballots" = mod.q41.2,
  "Partisan Poll Watchers" = mod.q41.3,
  "Election Staff Includes Veterans" = mod.q41.4,
  "Election Staff Includes Lawyers" = mod.q41.5,
  "Election Staff Includes Students" = mod.q41.6
)

# print the coef plot and selectively color coefficients
ggstats::ggcoef_compare(
  models.q41, 
  conf.int = T, 
  add_reference_rows = F,
  categorical_terms_pattern = "{level} (ref: {reference_level})")+
  ggplot2::scale_color_manual(
    values = c("black", "black", "black", "red", "black", "black"))+
  labs(
    title = "Impact on Confidence in Election Fairness when Election Staff \nIncludes Veterans and Family by Experiment Condition",
    subtitle = "Means comparison with 95% confidence intervals\n'Increase in confidence' = 1, 'No increase in confidence = 0'",
    caption = "estimate = 0.11635, SE = 0.02799, t-statistic = 4.157, CI [-0.171, -0.0614]"
  )+
  theme_bw()+
  theme(legend.position = 'bottom')


# save plot to images folder in project dir
ggplot2::ggsave(filename = 'q41_by_treatment_coef_plot.png', path = '~/R/GA_work/CDCE/cdce_vets/images')


# Now just the data from the list of models
q41_results <- ggstats::ggcoef_compare(
  models.q41, 
  add_reference_rows = F, 
  categorical_terms_pattern = "{level} (ref: {reference_level})",
  return_data = T) |>
  select(item = model,
         group = label,
         n = n_obs,
         beta_estimate = estimate,
         std.error,
         statistic,
         p.value,
         signif_stars,
         conf.low, conf.high) |> 
  mutate(item = dplyr::case_when(
    item == "Test Every Machine" ~ "Election Officials Test Every Machine",
    item == "Audit Ballots" ~ "Election Officials Conduct Audits of Ballots",
    item == "Partisan Poll Watchers" ~ "Partisan Poll Watchers Observe Election",
    .default = item
  ))

## compare models for q43 ::::::::::::::::::::::::::::::::::::::::::::::::::####
# show coef plot when Q43 are converted to a dummy variable where 1 =
# 'Increase' and 0 = "No increase"

# I could easily use purrr::map() to iterate this part, but I don't wanna right
# now...
mod.q43.1 <- lm(formula = q43.1.clps.dum ~ group, data = df)
mod.q43.2 <- lm(formula = q43.2.clps.dum ~ group, data = df)
mod.q43.3 <- lm(formula = q43.3.clps.dum ~ group, data = df)
mod.q43.4 <- lm(formula = q43.4.clps.dum ~ group, data = df)
mod.q43.5 <- lm(formula = q43.5.clps.dum ~ group, data = df)
mod.q43.6 <- lm(formula = q43.6.clps.dum ~ group, data = df)

# place all the models in a list assigned as 'models' and include them in the
# ggstats::ggcoef_compare() function
models.q43 <- list(
  "Test Every Machine" = mod.q43.1,
  "Audit Ballots" = mod.q43.2,
  "Partisan Poll Watchers" = mod.q43.3,
  "Election Staff Includes Veterans" = mod.q43.4,
  "Election Staff Includes Lawyers" = mod.q43.5,
  "Election Staff Includes Students" = mod.q43.6
)

# print the coef plot and selectively color coefficients
ggstats::ggcoef_compare(
  models.q43, 
  conf.int = T, 
  add_reference_rows = F, 
  categorical_terms_pattern = "{level} (ref: {reference_level})")+
  ggplot2::scale_color_manual(
    values = c("black", "black", "black", "red", "black", "black"))+
  labs(
    title = "Impact on Confidence in Voter Safety when Election Staff \nIncludes Veterans and Family by Experiment Condition",
    subtitle = "Means comparison with 95% confidence intervals\n'Increase in confidence' = 1, 'No increase in confidence = 0'",
    caption = "estimate = 0.0.0841, SE = 0.0280, t-statistic = 3.00, CI [0.0291, 0.139]"
  )+
  theme_bw()+
  theme(legend.position = 'bottom')


# save plot to images folder in project dir
ggplot2::ggsave(filename = 'q43_by_treatment_coef_plot.png', path = '~/R/GA_work/CDCE/cdce_vets/images')


# Now just the data from the list of models
q43_results <- ggstats::ggcoef_compare(
  models.q43, 
  add_reference_rows = F, 
  categorical_terms_pattern = "{level} (ref: {reference_level})", 
  return_data = T) |>
  select(item = model,
         group = label,
         n = n_obs,
         beta_estimate = estimate,
         std.error,
         statistic,
         p.value,
         signif_stars,
         conf.low, conf.high) |> 
  mutate(item = dplyr::case_when(
    item == "Test Every Machine" ~ "Election Officials Test Every Machine",
    item == "Audit Ballots" ~ "Election Officials Conduct Audits of Ballots",
    item == "Partisan Poll Watchers" ~ "Partisan Poll Watchers Observe Election",
    .default = item
  ))

q41_results[4,]
q43_results[4,]
