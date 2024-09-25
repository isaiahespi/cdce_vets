
# install.packages('gtsummary')
# install.packages('lessR')
# install.packages("RCPA3")
# install.packages('rstatix')
# install.packages('likert')
# install.packages('crosstable')
# install.packages('ggpubr')


library(tidyverse)
library(gtsummary)
library(RCPA3)
library(rstatix)
library(ggpubr)
library(easystats)
library(effectsize)
options(es.use_symbols = TRUE) # get nice symbols when printing! (On Windows, requires R >= 4.2.0)
# library(crosstable)
# library(gmodels)
# library(likert)

# load data
load("data/df-2024-09-11.Rdata") # df set as factor with dummy vars
load("data/data_numeric_vets_survey-exp-2024-09-11.Rdata") # data2 numeric only

# reduce those factor levels/val labels
# df <- df |>  
#   mutate(across(
#     c(q41_1:q41_6, q43_1:q43_6, q44_1:q44_6, q46_1:q46_6), ~forcats::fct_recode(.,
#     "decrease_a_lot" = "Decrease confidence a lot",
#     "decrease_some" = "Decrease confidence somewhat",
#     "no_impact" = "No impact on confidence",
#     "increase_some" = "Increase confidence somewhat",
#     "increase_a_lot" = "Increase confidence a lot"
#   )))

df |> select(rowID, group, qset, contains(".clps") & contains("q4"))

map(df[146:169], sjlabelled::get_label)
map(df[146:169], sjlabelled::get_labels) # no labels for numeric dummy vars

df |> select(contains("_clps")) 
  

surveytoolbox::extract_vallab(df, variable = 'q41_1_clps')
surveytoolbox::extract_vallab(df, variable = 'q41_2_clps')
surveytoolbox::extract_vallab(df, variable = 'q41_3_clps')
surveytoolbox::extract_vallab(df, variable = 'q41_4_clps')
surveytoolbox::extract_vallab(df, variable = 'q41_5_clps')
surveytoolbox::extract_vallab(df, variable = 'q41_6_clps')


## 2-sample t-tests for each dummified question by group condition
dums <- df |> select(group, contains("_dum"))

attributes(dums)

# makes df long
melted <- reshape2::melt(dums, id.vars = "group")

# conduct t-tests, assign to dum_ttests
dum_ttests <- melted |> group_by(variable) |> 
  rstatix::t_test(value ~ group, detailed = T) |> 
  rstatix::add_significance()

# relocate p.signif column
dum_ttests <- dum_ttests |> dplyr::relocate(c(p, p.signif), .after = conf.high)

# show only variables with stat.sig differences between experiment condition
dum_ttests |> 
  select(-5, -df, alternative) |> 
  filter(p.signif != "ns") |> 
  arrange(variable) |> 
  kableExtra::kbl(
    caption = "Two-sample T-test results by Experiment Condition",
    booktabs = T
  ) |> 
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "bordered", "condensed", "responsive"),
    latex_options = "basic",
    font_size = 13,
    full_width = T,
    fixed_thead = T
  )
  


kableExtra::kbl(dum_ttests, 
                format = "pipe", 
                digits = 4,
                caption = "Two-sample t-tests results by Experiment Condition",
                booktabs = T) 
  
# use this to save kable output to file
kableExtra::save_kable(dum_ttests, file = "ttests.txt", self_contained = T)


################################################################################
# I need to compare the set above to the majority language set of questions
# Unfortuately I can't use the table1 package

# Just like the group var, the qset var randomly distributed individuals of the
# sample into one of two group, A or B. Group A was presented with a set of
# questions [Q41, Q43] whereas group B was presented with a slightly different
# set of questions [Q44, Q46]. The goal here is to determine whether the
# different question wording resulted in observable differences in response
# patterns between groups A and B.


# re-code factor levels of matrix-style questions and assign to `dums` df
# (If not done already)
# dumf <- df |> 
#   mutate(across(
#     c(q41_1:q41_6, q43_1:q43_6, q44_1:q44_6, q46_1:q46_6), ~forcats::fct_recode(.,
#     "decrease_a_lot" = "Decrease confidence a lot",
#     "decrease_some" = "Decrease confidence somewhat",
#     "no_impact" = "No impact on confidence",
#     "increase_some" = "Increase confidence somewhat",
#     "increase_a_lot" = "Increase confidence a lot"
#   )))

# subset df to only those variables of interest.
# Qset A [q41, q43] and Qset B [q44, q46]
dumf <- df |> select(rowID, qset, group, 203:226)

# check levels of factors in subset df
dumf |> select(where(is.factor)) |> sapply(levels)


# see the item statements (var labels) for factor vars
map(dumf[4:27], sjlabelled::get_label)
# see the value labels (factor levels)
map(dumf[4:27], sjlabelled::get_labels)

### EASIEST WAY, BEST DISPLAY IN CONSOLE ###
# see variable names and their variable labels 
dumf |> 
  select(4:27) |> 
  surveytoolbox::data_dict() |> select(var, label_var, value)

#####:::::::::::::::::::::::::::::: NOTE ::::::::::::::::::::::::::::::::::::###
# The question statements for the two different sets do not match with the
# question number. Below is the correct match
# q41_1 == q44_1
# q41_2 == q44_4
# q41_3 == q44_5
# q41_4 == q44_2
# q41_5 == q44_3
# q41_6 == q44_6

# q43_1 == q46_1
# q43_2 == q46_4
# q43_3 == q46_6
# q43_4 == q46_2
# q43_5 == q46_3
# q43_6 == q46_5
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::##

# see variable names and their variable labels 
dumf |> 
  select(4:27) |> 
  surveytoolbox::data_dict() |> select(var, label_var)

# this is the one to care about
# regarding fairness and accuracy of elections
# factor levels are collapsed to only 3 levels
# no significant difference, fail to reject null
RCPA3::testpropsC(x1 = q41_4, x2 = q44_2, data = dumf, response = "increase", xlim = c(-0.5, 0.5))
RCPA3::freqC(x = q41_4.clps, data = dumf) # 'includes', collapsed to 3 levels
RCPA3::freqC(x = q44_2.clps, data = dumf) # 'majority', collapsed to 3 levels

# I kind of hate the RCPA3 pacakge for its overly exhaustive output and
# arguments. I need a simple way of doing a prop.test and displaying results.

# rstatix provides a simple wrapper around base prop.test
rstatix::prop_test()


dumf |> 
sjmisc::frq(q41_4.clps)

dumf |> 
sjmisc::frq(q44_2.clps)

# this next one is for lawyers, regarding fairness and accuracy of elections
# Q41_5. Election staff include lawyers (q41_5.clps)
# Q44_3. Majority of Election staff are lawyers (q44_3.clps)
# sig diff in proportions for both increase and decrease responses
# direction suggests that language saying the majority of election staff consist
# of lawyers reduces confidence in fairness and accuracy of elections
RCPA3::testpropsC(x1 = q41_5.clps, x2 = q44_3.clps, data = df, response = "increase", xlim = c(-.1, .2))
RCPA3::testpropsC(x1 = q41_5.clps, x2 = q44_3.clps, data = df, response = "decrease", xlim = c(-.1, .2))

df |> 
sjmisc::frq(q41_5.clps)

df |> 
sjmisc::frq(q44_3.clps)

df |> 
  janitor::tabyl(q41_5, show_na = F) |> 
  janitor::adorn_totals("both", na.rm = T) |>
  janitor::adorn_rounding(digits = 2) 

df |> 
  janitor::tabyl(q44_5, show_na = F) |> 
  janitor::adorn_totals("both", na.rm = T) |> 
  janitor::adorn_rounding(digits = 2)


# now for college students
# Q41_6. Election staff include college students (q41_6.clps)
# Q44_6. Majority of Election staff are students (q44_6.clps)
# sig diff in prop for increase 0.108
# sig diff in prop for decrease -0.11
RCPA3::testpropsC(x1 = q41_6.clps, x2 = q44_6.clps, data = df, response = "increase", xlim = c(-.1, .2))
RCPA3::testpropsC(x1 = q41_6.clps, x2 = q44_6.clps, data = df, response = "decrease", xlim = c(-.1, .2))

df |> 
sjmisc::frq(q41_6.clps)

df |> 
sjmisc::frq(q44_6.clps)

dumf |> 
  janitor::tabyl(q41_6, show_na = F) |> 
  janitor::adorn_totals("both", na.rm = T) |>
  janitor::adorn_rounding(digits = 3) 

dumf |> 
  janitor::tabyl(q44_6, show_na = F) |> 
  janitor::adorn_totals("both", na.rm = T) |> 
  janitor::adorn_rounding(digits = 3)


# let me also do this for the first three statements too: 
# test machines, 
# conduct audits, 
# partisan poll watchers

# no diff
RCPA3::testpropsC(
  x1 = q41_1.clps,
  x2 = q44_1.clps,
  data = df,
  response = "increase",
  xlim = c(-.1, .2)
)

RCPA3::testpropsC(
  x1 = q41_1.clps,
  x2 = q44_1.clps,
  data = df,
  response = "decrease",
  xlim = c(-.1, .2)
)

# no diff
RCPA3::testpropsC(x1 = q41_2.clps, x2 = q44_4.clps, data = df, response = "increase", xlim = c(-.1, .2))
RCPA3::testpropsC(x1 = q41_2.clps, x2 = q44_4.clps, data = df, response = "decrease", xlim = c(-.1, .2))

# no diff
RCPA3::testpropsC(x1 = q41_3.clps, x2 = q44_5.clps, data = df, response = "increase", xlim = c(-.1, .2))
RCPA3::testpropsC(x1 = q41_3.clps, x2 = q44_5.clps, data = df, response = "decrease", xlim = c(-.1, .2))


# IMPACT ON SAFETY :::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# testing diff in proportions between items with different question wording
# vets

# Q43_4. Election staff includes veterans (q43_4.clps)
df |> sjmisc::frq(q43_4.clps) 

# Q46_2. Majority of Election staff are veterans (q46_2.clps)
df |> sjmisc::frq(q46_2.clps) 

# No diff
RCPA3::testpropsC(
  x1 = q43_4.clps,
  x2 = q46_2.clps,
  data = df,
  response = "increase", 
  # main = "95% Confidence interval difference of proportions between Q43_4 and Q46_4\n 'Increase confidence a lot' and 'Increase confidence somewhat'",
  # xlab = "Election staff...include veterans\nv. The majority of election staff...consist of veterans",
  printC = F, 
  ci.plot = F,
  xlim = c(-.2, .2))

# no diff
RCPA3::testpropsC(
  x1 = q43_4.clps,
  x2 = q46_2.clps,
  data = df,
  response = "decrease", 
  # main = "95% Confidence interval difference of proportions between Q43_4 and Q46_4\n 'Increase confidence a lot' and 'Increase confidence somewhat'",
  # xlab = "Election staff...include veterans\nv. The majority of election staff...consist of veterans",
  printC = F, 
  ci.plot = F,
  xlim = c(-.1, .1))



# majority of lawyers now
# Q43_5. Election staff includes lawyers (q43_5.clps)
df |> sjmisc::frq(q43_5.clps)

# Q46_3. Majority of Election staff are lawyers (q46_3.clps)
df |> sjmisc::frq(q46_3.clps)

# sig diff in prop, 
# positive difference of 0.0719 suggests that the prospect of
# having the majority of election staff consist of lawyers increases confidence
# in voter safety less than when election staff merely includes lawyers.
RCPA3::testpropsC(
  x1 = q43_5.clps, 
  x2 = q46_3.clps, 
  data = df, 
  response = "increase", 
  xlim = c(-.2, .2))

# sig diff in prop, 
# Negative difference of proportions -0.0593 suggests that the prospect of
# having the majority of election staff consist of lawyers decreases confidence
# in voter safety more than when election staff is said to merely include
# lawyers.
RCPA3::testpropsC(
  x1 = q43_5.clps, 
  x2 = q46_3.clps, 
  data = df, 
  response = "decrease", 
  xlim = c(-.2, .2))


# a majority of college students
# Q43_6. Election staff includes students (q43_6.clps)
df |> sjmisc::frq(q43_6.clps)

# Q46_5. Majority of Election staff are students (q46_5.clps)
df |> sjmisc::frq(q46_5.clps)

# sig diff in prop, |0.099|
# The positive difference of 0.099 suggests that the prospect of
# having the majority of election staff consist of college students increases confidence
# in voter safety less than when election staff merely includes college students.
RCPA3::testpropsC(
  x1 = q43_6.clps, 
  x2 = q46_5.clps, 
  data = df, 
  response = "increase",
  xlim = c(-.2, .2))

# sig diff in prop, -0.111|
# The negative difference of proportions -0.111 suggests that the prospect of
# having the majority of election staff consist of college students decreases
# confidence in voter safety more than when election staff is said to merely
# include college students.
RCPA3::testpropsC(
  x1 = q43_6.clps, 
  x2 = q46_5.clps, 
  data = df, 
  response = "decrease",
  xlim = c(-.2, .2))



# coalesce qset A and B where able::::::::::::::::::::::::::::::::::::::::::####

# The variables where proportions between the two independent samples are
# equivalent (i.e., no significant differences in proportion were found) will be
# coalesced (i.e., merged).
# Q44_1 will coalesce into Q41_1
# Q44_4 will coalesce into Q41_2
# Q44_5 will coalesce into Q41_3
# Q44_2 will coalesce into Q41_4
# Q46_1 will coalesce into Q43_1
# Q46_4 will coalesce into Q43_2
# Q46_6 will coalesce into Q43_3
# Q46_2 will coalesce into Q43_4


# get counts of NA values for each variable grouped by qset
df |> select(
  rowID,
  qset,
  q41_1:q41_4,
  q43_1:q43_4,
  q44_1,
  q44_4,
  q44_5,
  q44_2,
  q46_1,
  q46_4,
  q46_6,
  q46_2
) |>
  group_by(qset) |>
  summarise_all( ~ sum(is.na(.)))

# counts for each response minus the NA values from qset
forcats::fct_count(df$q41_1[!is.na(df$qset)], prop = T)
forcats::fct_count(df$q44_1[!is.na(df$qset)], prop = T)

# merge (or coalesce) the qset vars that can be merged
# this merges some vars from the q44 and q46 set (minus statements regarding
# lawyers and students) into q41 and q43 set since there was no sig diff found
# between response categories
df <- df |> 
  group_by(rowID) |> 
  mutate(q41.1 = dplyr::coalesce(q41_1, q44_1),
         q41.2 = dplyr::coalesce(q41_2, q44_4),
         q41.3 = dplyr::coalesce(q41_3, q44_5),
         q41.4 = dplyr::coalesce(q41_4, q44_2),
         q43.1 = dplyr::coalesce(q43_1, q46_1),
         q43.2 = dplyr::coalesce(q43_2, q46_4),
         q43.3 = dplyr::coalesce(q43_3, q46_6),
         q43.4 = dplyr::coalesce(q43_4, q46_2),
         q41.1.clps = dplyr::coalesce(q41_1.clps, q44_1.clps),
         q41.2.clps = dplyr::coalesce(q41_2.clps, q44_4.clps),
         q41.3.clps = dplyr::coalesce(q41_3.clps, q44_5.clps),
         q41.4.clps = dplyr::coalesce(q41_4.clps, q44_2.clps),
         q43.1.clps = dplyr::coalesce(q43_1.clps, q46_1.clps),
         q43.2.clps = dplyr::coalesce(q43_2.clps, q46_4.clps),
         q43.3.clps = dplyr::coalesce(q43_3.clps, q46_6.clps),
         q43.4.clps = dplyr::coalesce(q43_4.clps, q46_2.clps),) |>
  ungroup() 



df |> 
  select(rowID, qset,
         q41.1:q41.4, 
         q43.1:q43.4,
         q41.1.clps:q41.4.clps,
         q43.1.clps:q43.4.clps) |>
  
  # this filters out all the NAs from the qset var, n = 105
  # filter(!is.na(qset)) |>
  
  # idk why this works, but its the sum of NA values per variable minus the NA
  # from the corresponding set. So NAs independent of the other sample.
  group_by(qset) |> 
  summarise_all(~sum(is.na(.)))

# another way to compare
df |> select(rowID, qset, q41_1, q44_1, q41.1) |>
  filter(!is.na(qset)) |>
  group_by(qset) |> 
  summarise(
    n = sum(n()),
    'q41.1 N' = sum(!is.na(q41.1)),
    'q41.1 NA' = sum(is.na(q41.1)),
    'q41_1 N' = sum(!is.na(q41_1)),
    'q41_1 NA' = sum(is.na(q41_1)-!is.na(q44_1)),
    'q44_1 N' = sum(!is.na(q44_1)),
    'q44_1 NA' = sum(is.na(q44_1) - !is.na(q41_1))
  )


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####
# Now differences by group (treatment, control) can be tested to determine
# whether the treatment vignette had an effect on any of the responses

levels(df$q41.1)

RCPA3::testpropsC(
  dv = q41.1.clps,
  iv = group,
  data = df,
  response = "increase", 
  main = "95% Confidence interval difference of proportions between treatment and control\n 'Increase confidence a lot' and 'Increase confidence somewhat'",
  xlab = "Election Staff Test every machine",
  printC = F, 
  ci.plot = T,
  xlim = c(-.2, .2))

RCPA3::testpropsC(
  dv = q41.1.clps,
  iv = group,
  data = df,
  response = "decrease", 
  main = "95% Confidence interval difference of proportions between treatment and control\n 'Increase confidence a lot' and 'Increase confidence somewhat'",
  xlab = "Election Staff Test every machine",
  printC = F, 
  ci.plot = T,
  xlim = c(-.2, .2))

RCPA3::testpropsC(
  dv = q41.2.clps,
  iv = group,
  data = df,
  response = "increase", 
  main = "95% Confidence interval difference of proportions between treatment and control\n 'Increase confidence a lot' and 'Increase confidence somewhat'",
  xlab = "Q41_2. Election officials conduct audits",
  printC = F, 
  ci.plot = T,
  xlim = c(-.2, .2))

RCPA3::testpropsC(
  dv = q41.2.clps,
  iv = group,
  data = df,
  response = "decrease", 
  main = "95% Confidence interval difference of proportions between treatment and control\n Decrease confidence in Fairness and Accuracy",
  xlab = "Q41_2. Election officials conduct audits",
  printC = F, 
  ci.plot = T,
  xlim = c(-.2, .2))

RCPA3::testpropsC(
  dv = q41.3.clps,
  iv = group,
  data = df,
  response = "increase", 
  main = "95% Confidence interval difference of proportions between treatment and control\n 'Increase confidence a lot' and 'Increase confidence somewhat'",
  xlab = "Partisan Poll watchers observe the election",
  printC = F, 
  ci.plot = T,
  xlim = c(-.2, .2))

RCPA3::testpropsC(
  dv = q41.3.clps,
  iv = group,
  data = df,
  response = "decrease", 
  main = "95% Confidence interval difference of proportions between treatment and control\n Decrease confidence in Fairness and Accuracy",
  xlab = "Partisan Poll watchers observe the election",
  printC = F, 
  ci.plot = T,
  xlim = c(-.2, .2))

# SIG DIFF BETWEEN Treatment and Control groups: having election staff include
# vets and family increases confidence in fairness and accuracy for those who
# read the treatment vignette (n = 646) compared to those who did not (n = 631).
# diff of proportions is 0.113.
RCPA3::testpropsC(
  dv = q41.4.clps,
  iv = group,
  data = df,
  response = "increase", 
  # main = "95% Confidence interval difference of proportions between treatment and control\n Increase confidence in Fairness and Accuracy",
  # xlab = "Election staff include veterans and family",
  printC = F, 
  ci.plot = T,
  xlim = c(-.2, .2))


# get raw counts of increase v no increase in confidence
# mutate vars to dichotomous,
# select two columns from tabyl object,
# then pipe into rstatix::prop_test()
# Sig diff in prop, p < 0.001
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

# try to make decent table of results
pt1 <- df |>
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


pt1 |> 
  kableExtra::kbl(format.args = list(scientific = F),
                  digits = 5) |> 
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "bordered", "condensed", "responsive"),
    latex_options = "basic",
    font_size = 13,
    full_width = T,
    fixed_thead = T
  )


df |>
  mutate(
    confimpct_vets = dplyr::case_when(
      q41.4.clps == "increase" ~ 'Increase Confidence',
      q41.4.clps == "no_impact" ~ 'No increase',
      q41.4.clps == 'decrease' ~ 'No increase'
      )
    ) |>
  janitor::tabyl(group, confimpct_vets,  show_na = F) |> 
  janitor::adorn_totals('col') |> 
  janitor::adorn_percentages('row') |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |> 
  janitor::adorn_ns("rear")




# Now the same thing, except for 'decrease' responses
# no sig diff in prop for decrease responses. 
df |>
  mutate(
    confimpct_vets = dplyr::case_when(
      q41.4.clps == "increase" ~ 'no_decrease',
      q41.4.clps == "no_impact" ~ 'no_decrease',
      q41.4.clps == 'decrease' ~ 'decrease'
      ),
    group = dplyr::case_when(
      group == "Treatment" ~ 1,
      group == "Control" ~ 0
    )
    ) |>
  janitor::tabyl(group, confimpct_vets,  show_na = F) |> 
  select(decrease, no_decrease) |> 
  rstatix::prop_test(detailed = T)


# Simple Crosstab to show response breakdown by experiment condition
# 6 NAs omitted. 
df |>
  mutate(
    confimpct_vets = dplyr::case_when(
      q41.4.clps == "increase" ~ 'Increase Confidence',
      q41.4.clps == "no_impact" ~ 'No Impact',
      q41.4.clps == 'decrease' ~ 'Decrease Confidence'
      )) |> 
  janitor::tabyl(group, confimpct_vets,  show_na = F) |> 
  janitor::adorn_totals('both') |> 
  janitor::adorn_percentages('row') |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = T) |> 
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title('top', row_name = 'Condition', col_name = 'Election staff include veterans and family')


# Impact on safety :::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# just to check, I still don't like RCPA3 but it is comprehensive
RCPA3::testpropsC(
  dv = q43.4.clps,
  iv = group,
  data = df,
  response = "increase", 
  # main = "95% Confidence interval difference of proportions between treatment and control\n Increase confidence in Fairness and Accuracy",
  # xlab = "Election staff include veterans and family",
  printC = F, 
  ci.plot = T,
  xlim = c(-.2, .2))

# using the easy method of prop.test

# get raw counts of increase v no increase in confidence
# mutate vars to dichotomous,
# select two columns from tabyl object,
# then pipe into rstatix::prop_test()
# Sig diff in prop, p < 0.001, CI [-0.139, -0.0264]
df |>
  mutate(
    votersafety_vets = dplyr::case_when(
      q43.4.clps == "increase" ~ 'increase',
      q43.4.clps == "no_impact" ~ 'no_increase',
      q43.4.clps == 'decrease' ~ 'no_increase'
      ),
    group = dplyr::case_when(
      group == "Treatment" ~ 1,
      group == "Control" ~ 0
    )
    ) |>
  janitor::tabyl(group, votersafety_vets,  show_na = F) |> 
  select(increase, no_increase) |> 
  rstatix::prop_test(detailed = T)

# NOTE: I tested diff of proportions for q43.1.clps, q43.2.clps, and q43.3.clps
# and didn't find any significant differences in 'increase' nor in 'decrease' responses for any of
# the variables between treatment and control groups.

# Simple Crosstab to show response breakdown by experiment condition
# 6 NAs omitted. 
df |>
  mutate(
    confimpct_vets = dplyr::case_when(
      q43.4.clps == "increase" ~ 'Increase Confidence',
      q43.4.clps == "no_impact" ~ 'No Impact',
      q43.4.clps == 'decrease' ~ 'Decrease Confidence'
      )) |> 
  janitor::tabyl(group, confimpct_vets,  show_na = F) |> 
  janitor::adorn_totals('both') |> 
  janitor::adorn_percentages('row') |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = T) |> 
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title('top', row_name = 'Condition', col_name = 'Election staff include veterans and family')


hell <- df |> 
  mutate(
    confimpct_vets_safe = dplyr::case_when(
      q43.4.clps == "increase" ~ 1,
      q43.4.clps == "no_impact" ~ 0,
      q43.4.clps == 'decrease' ~ 0
      ),
    confimpct_vets_fair = dplyr::case_when(
      q41.4.clps == "increase" ~ 1,
      q41.4.clps == "no_impact" ~ 0,
      q41.4.clps == 'decrease' ~ 0
      ),
    group = dplyr::case_when(
      group == "Treatment" ~ 1,
      group == "Control" ~ 0
    ))


RCPA3::compmeansC(
    dv = confimpct_vets_fair,
    iv = group,
    data = hell,
    plot = 'line',
    plot.ci = T)


RCPA3::compmeansC(
    dv = confimpct_vets_safe,
    iv = group,
    data = hell,
    plot = 'line',
    plot.ci = T)

# convert to numeric and conduct T-test comparison of means
hell <- df |> 
  mutate(
    confimpct_vets_safe = dplyr::case_when(
      q43.4.clps == "increase" ~ 1,
      q43.4.clps == "no_impact" ~ 0,
      q43.4.clps == 'decrease' ~ 0
      ),
    confimpct_vets_fair = dplyr::case_when(
      q41.4.clps == "increase" ~ 1,
      q41.4.clps == "no_impact" ~ 0,
      q41.4.clps == 'decrease' ~ 0
      ),
    group = dplyr::case_when(
      group == "Treatment" ~ 1,
      group == "Control" ~ 0
    ))

ttq41 <- df |> 
  mutate(
    confimpct_vets_fair = dplyr::case_when(
      q41.4.clps == "increase" ~ 1,
      q41.4.clps == "no_impact" ~ 0,
      q41.4.clps == 'decrease' ~ 0
      )) |> 
  rstatix::t_test(formula = confimpct_vets_fair ~ group, paired = F, conf.level = 0.95, detailed = T) |> 
  rstatix::add_significance()

ttq43 <- df |> 
  mutate(
    confimpct_vets_safe = dplyr::case_when(
      q43.4.clps == "increase" ~ 1,
      q43.4.clps == "no_impact" ~ 0,
      q43.4.clps == 'decrease' ~ 0
      )) |>
  rstatix::t_test(formula = confimpct_vets_safe ~ group, paired = F, conf.level = 0.95, detailed = T) |> 
  rstatix::add_significance()

ttq41 |> 
dplyr::add_row(ttq43)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####
# demonstrating differences of props and comparing means between question sets
# for statements concerning lawyers and college students.

df |> 
sjmisc::frq(q41_5.clps)

df |> 
sjmisc::frq(q44_5.clps)

# I coalesce these vars so I can put them in a cross tab by qset
law_skool <- df |> 
  group_by(rowID) |> 
  mutate(
    q41.5.clps = dplyr::coalesce(q41_5.clps, q44_3.clps),
    q41.6.clps = dplyr::coalesce(q41_6.clps, q44_6.clps),
    q43.5.clps = dplyr::coalesce(q43_5.clps, q46_3.clps),
    q43.6.clps = dplyr::coalesce(q43_6.clps, q46_5.clps),) |>
  ungroup() |>
  filter(!is.na(qset)) 


law_skool |> 
  janitor::tabyl(qset, q41.5.clps, show_na = F) |> 
  janitor::adorn_totals('both') |> 
  janitor::adorn_percentages(denominator = 'row') |> 
  janitor::adorn_pct_formatting(digits = 3, affix_sign = F) |> 
  janitor::adorn_ns() |> 
  janitor::adorn_title('top')


law_skool <- law_skool |> 
  select(qset, q41.5.clps, q41.6.clps, q43.5.clps, q43.6.clps) |> 
  mutate(
    fair_lawyers = dplyr::case_when(
      q41.5.clps == 'increase'  ~ 'increase',
      q41.5.clps == 'no_impact'  ~ 'no_increase',
      q41.5.clps == 'decrease'  ~ 'no_increase',
      TRUE ~ NA
    )) |> 
  mutate(
    fair_students = dplyr::case_when(
      q41.6.clps == 'increase'  ~ 'increase',
      q41.6.clps == 'no_impact'  ~ 'no_increase',
      q41.6.clps == 'decrease'  ~ 'no_increase',
      TRUE ~ NA
    )) |> 
  mutate(
    safe_lawyers = dplyr::case_when(
      q43.5.clps == 'increase'  ~ 'increase',
      q43.5.clps == 'no_impact'  ~ 'no_increase',
      q43.5.clps == 'decrease'  ~ 'no_increase',
      TRUE ~ NA
    )) |> 
  mutate(
    safe_students = dplyr::case_when(
      q43.6.clps == 'increase'  ~ 'increase',
      q43.6.clps == 'no_impact'  ~ 'no_increase',
      q43.6.clps == 'decrease'  ~ 'no_increase',
      TRUE ~ NA
    )) |> 
  select(qset, fair_lawyers, fair_students, safe_lawyers, safe_students) 

law_skool <- law_skool |> 
  mutate(qset = forcats::fct_recode(qset, Includes = "A", Majority = "B"))


# make sure they match
df |> filter(!is.na(qset)) |>  sjmisc::frq(qset)
law_skool |> sjmisc::frq(qset)

var_names <- law_skool[,2:5] |> colnames()

# crabs
law_skool |> 
  janitor::tabyl(qset, fair_lawyers, show_na = F) |> 
  janitor::adorn_totals('both') |> 
  janitor::adorn_percentages(denominator = 'row') |> 
  janitor::adorn_pct_formatting(digits = 3, affix_sign = F) |> 
  janitor::adorn_ns() |> 
  janitor::adorn_title('top')

# crabs
law_skool |> 
  janitor::tabyl(qset, fair_students, show_na = F) |> 
  janitor::adorn_totals('both') |> 
  janitor::adorn_percentages(denominator = 'row') |> 
  janitor::adorn_pct_formatting(digits = 3, affix_sign = F) |> 
  janitor::adorn_ns() |> 
  janitor::adorn_title('top')


# crabs
law_skool |> 
  janitor::tabyl(qset, safe_lawyers, show_na = F) |> 
  janitor::adorn_totals('both') |> 
  janitor::adorn_percentages(denominator = 'row') |> 
  janitor::adorn_pct_formatting(digits = 3, affix_sign = F) |> 
  janitor::adorn_ns() |> 
  janitor::adorn_title('top')


# crabs
law_skool |> 
  janitor::tabyl(qset, safe_students, show_na = F) |> 
  janitor::adorn_totals('both') |> 
  janitor::adorn_percentages(denominator = 'row') |> 
  janitor::adorn_pct_formatting(digits = 3, affix_sign = F) |> 
  janitor::adorn_ns() |> 
  janitor::adorn_title('top')


# function to run prop_tests over a list of crosstabs 
tabyl.fun <- function(x){
  crab_df <- law_skool |> select(qset, all_of(x)) |> 
    janitor::tabyl(qset, !!sym(x), show_na = F) |> 
    select(-1) |> 
    rstatix::prop_test(detailed = T)
}

# this maps the function tabyl.fun to each character string in 'var_names'
# the results are put into a list of tibbles, in this case, a list of 4 tibbles
law_skool_prop_tests <- map(var_names, tabyl.fun)

# I combine the 4 tibbles within the list into a single tibble
pt_lawyers_students <- purrr::reduce(law_skool_prop_tests, dplyr::full_join)

# add column that identifies rows of different prop_tests
pt_lawyers_students$items <- var_names

# add column that identifies response compared
response <- c("Increase", "Increase", "Increase", "Increase")
pt_lawyers_students$response <- response



# relocate columns to better positions, rename columns, remove df column
pt_lawyers_students <- pt_lawyers_students |>
  dplyr::relocate(c(items, response), .before = n) |>
  dplyr::relocate(p.signif, .after = p) |>
  dplyr::rename(valid_n = n,
                Qset_A_n = n1,
                Qset_B_n = n2) |>
  dplyr::mutate(prop.diff = estimate1 - estimate2,
                .before = statistic) |> 
  select(-df)

pt_lawyers_students

# print prop_tests as table 
kableExtra::kbl(pt_lawyers_students) |> 
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "bordered", "condensed", "responsive"),
    latex_options = "basic",
    font_size = 13,
    full_width = T,
    fixed_thead = T
  )

  
# conduct prop_tests 
# use these to confirm results
law_skool |> 
  janitor::tabyl(qset, fair_lawyers, show_na = F) |> 
  select(-1) |> 
  rstatix::prop_test(detailed = T)


law_skool |> 
  janitor::tabyl(qset, safe_lawyers, show_na = F) |> 
  select(-1) |> 
  rstatix::prop_test(detailed = T)

law_skool |> 
  janitor::tabyl(qset, fair_students, show_na = F) |> 
  select(-1) |> 
  rstatix::prop_test(detailed = T)


law_skool |> 
  janitor::tabyl(qset, safe_students, show_na = F) |> 
  select(-1) |> 
  rstatix::prop_test(detailed = T)

# another way to confirm results of prop tests using RCPA3 package
# Q41_6. Election staff include college students (q41_6.clps)
# Q44_6. Majority of Election staff are students (q44_6.clps)
# sig diff in prop for increase 0.108
# sig diff in prop for decrease -0.11
RCPA3::testpropsC(x1 = q41_6.clps, x2 = q44_6.clps, data = df, response = "increase", xlim = c(-.1, .2))
RCPA3::testpropsC(x1 = q41_6.clps, x2 = q44_6.clps, data = df, response = "decrease", xlim = c(-.1, .2))

################## NOW COMPARE MEANS USING DUMMY VARS ::::::::::::::::::::####


# add dummy variables
law_skool <- law_skool |> 
  mutate(across(c(fair_lawyers, fair_students, safe_lawyers, safe_students),
                ~ dplyr::case_when(
                  .x == "increase" ~ 1,
                  .x == "no_increase" ~ 0
                  )))



law_skool |> 
  rstatix::t_test(formula = fair_lawyers ~ qset, paired = F,
                  conf.level = 0.95, detailed = T) |> 
  rstatix::add_significance()
  
law_skool |> 
  rstatix::t_test(formula = fair_students ~ qset, paired = F,
                  conf.level = 0.95, detailed = T) |> 
  rstatix::add_significance()


law_skool |> 
  rstatix::t_test(formula = safe_lawyers ~ qset, paired = F,
                  conf.level = 0.95, detailed = T) |> 
  rstatix::add_significance()

law_skool |> 
  rstatix::t_test(formula = safe_students ~ qset, paired = F,
                  conf.level = 0.95, detailed = T) |> 
  rstatix::add_significance()






###############################################################################
df |> 
sjmisc::frq(q41_6.clps)

df |> 
sjmisc::frq(q44_6.clps)

df |> 
  janitor::tabyl(q41_6, show_na = F) |> 
  janitor::adorn_totals("both", na.rm = T) |>
  janitor::adorn_rounding(digits = 3) 

df |> 
  janitor::tabyl(q44_6, show_na = F) |> 
  janitor::adorn_totals("both", na.rm = T) |> 
  janitor::adorn_rounding(digits = 3)



# majority of lawyers now
# Q43_5. Election staff includes lawyers (q43_5.clps)
df |> sjmisc::frq(q43_5.clps)

# Q46_3. Majority of Election staff are lawyers (q46_3.clps)
df |> sjmisc::frq(q46_3.clps)

# sig diff in prop, 
# positive difference of 0.0719 suggests that the prospect of
# having the majority of election staff consist of lawyers increases confidence
# in voter safety less than when election staff merely includes lawyers.
RCPA3::testpropsC(
  x1 = q43_5.clps, 
  x2 = q46_3.clps, 
  data = df, 
  response = "increase", 
  xlim = c(-.2, .2))

# sig diff in prop, 
# Negative difference of proportions -0.0593 suggests that the prospect of
# having the majority of election staff consist of lawyers decreases confidence
# in voter safety more than when election staff is said to merely include
# lawyers.
RCPA3::testpropsC(
  x1 = q43_5.clps, 
  x2 = q46_3.clps, 
  data = df, 
  response = "decrease", 
  xlim = c(-.2, .2))


# a majority of college students
# Q43_6. Election staff includes students (q43_6.clps)
df |> sjmisc::frq(q43_6.clps)

# Q46_5. Majority of Election staff are students (q46_5.clps)
df |> sjmisc::frq(q46_5.clps)

# sig diff in prop, |0.099|
# The positive difference of 0.099 suggests that the prospect of
# having the majority of election staff consist of college students increases confidence
# in voter safety less than when election staff merely includes college students.
RCPA3::testpropsC(
  x1 = q43_6.clps, 
  x2 = q46_5.clps, 
  data = df, 
  response = "increase",
  xlim = c(-.2, .2))

# sig diff in prop, -0.111|
# The negative difference of proportions -0.111 suggests that the prospect of
# having the majority of election staff consist of college students decreases
# confidence in voter safety more than when election staff is said to merely
# include college students.
RCPA3::testpropsC(
  x1 = q43_6.clps, 
  x2 = q46_5.clps, 
  data = df, 
  response = "decrease",
  xlim = c(-.2, .2))
