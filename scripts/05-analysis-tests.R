

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####
# Now differences by group (treatment, control) can be tested to determine
# whether the treatment vignette had an effect on any of the responses these
# tests are tests of diff of proportions for the questions that were coalesced
# together.

# For reference
# Q44_1 + Q41_1 = q41.1 = "Election officials test machines"
# Q44_4 + Q41_2 = q41.2 = "Election officials conduct audits"
# Q44_5 + Q41_3 = q41.3 = "Partisan Poll watchers observe the election."
# Q44_2 + Q41_4 = q41.4 = "Election staff include/are majority of veterans and family"
# Q44_3 + Q41_5 = q41.5 = "Election staff include/are majority of lawyers"
# Q44_6 + Q41_6 = q41.6 = "Election staff includes/are majority of students"

# Q46_1 + Q43_1 = q43.1 = "Election officials test machines"
# Q46_4 + Q43_2 = q43.2 = "Election officials conduct audits"
# Q46_6 + Q43_3 = q43.3 = "Partisan Poll watchers observe the election."
# Q46_2 + Q43_4 = q43.4 = "Election staff include veterans and family" & "Majority of Election staff are veterans" 
# Q46_3 + Q43_5 = q43.5 = "Election staff include lawyers" & "Majority of Election staff are lawyers"
# Q46_5 + Q43_6 = q43.6 = "Election staff includes students" & "Majority of Election staff are students"

df |> 
  drop_na(q41.1) |> 
  group_by(group) |> 
  sjmisc::frq(q41.1, show.na = F)

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
  select(-1) |> 
  rstatix::prop_test(detailed = T)

# among the control group alone, there are sig diff in proportion of those who
# say veteran poll workers would increase their confidence in election fairness
# compared to those who don't.
df |>
  mutate(
    confimpct_vets = dplyr::case_when(
      q41.4.clps == "increase" ~ 'increase',
      q41.4.clps == "no_impact" ~ 'no_increase',
      q41.4.clps == 'decrease' ~ 'no_increase'
      )
    ) |>
  janitor::tabyl(group, confimpct_vets,  show_na = F) |>
  filter(group == "Control") |> 
  select(-1) |> 
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
  janitor::adorn_title(
    'top', 
    row_name = 'Condition', 
    col_name = 'Election staff include veterans and family')


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
# NAs omitted. 
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


RCPA3::compmeansC(dv = q41.4.clps.dum, iv = group, data = df, plot = 'points', plot.ci = T)

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


###############################################################################

df |> 
  select(group, q41.4.clps.dum) |> 
  group_by(group, q41.4.clps.dum) |> 
  drop_na() |> 
  count()







