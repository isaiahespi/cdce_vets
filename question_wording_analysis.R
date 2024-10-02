# determining whether there were any response effects due to different question wording between two questions sets, A and B.

# Just like the group var, the qset var randomly distributed individuals of the
# sample into one of two group, A or B. Group A was presented with a set of
# questions [Q41, Q43] whereas group B was presented with a slightly different
# set of questions [Q44, Q46]. The goal here is to determine whether the
# different question wording resulted in observable differences in response
# patterns between groups A and B.

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####
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

# sig diff in prop, -0.111
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


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####
# demonstrating differences of props and comparing means between question sets
# for statements concerning lawyers and college students.

df |> 
sjmisc::frq(q41_5.clps, show.na = F)

df |> 
sjmisc::frq(q44_5.clps, show.na = F)

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
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |> 
  janitor::adorn_ns() |> 
  janitor::adorn_title(
    'combined',
    row_name = "Wording") |> 
  kableExtra::kbl() |> 
  kableExtra::footnote(
    general = "Proportion of those who report an 'Increase' in confidence in fairness of elections when election staff either 'Includes' or is a 'majority' of lawyers"
  ) |> 
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "bordered", "condensed", "responsive"),
    latex_options = "basic",
    font_size = 13
  )

# crabs
law_skool |> 
  janitor::tabyl(qset, fair_students, show_na = F) |> 
  janitor::adorn_totals('both') |> 
  janitor::adorn_percentages(denominator = 'row') |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |> 
  janitor::adorn_ns() |> 
  janitor::adorn_title('combined') |> 
  kableExtra::kbl() |> 
  kableExtra::footnote(
    general = "Proportion of those who report an 'Increase' in confidence in fairness of elections when election staff either 'Includes' or is a 'majority' of college students"
  ) |> 
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "bordered", "condensed", "responsive"),
    latex_options = "basic",
    font_size = 13
  )

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
law_skool_num <- law_skool |> 
  mutate(
    qset = dplyr::case_when(
      qset == "Includes" ~ 0,
      qset == "Majority" ~ 1,
      TRUE ~ NA)) |> 
  mutate(across(c(fair_lawyers, fair_students, safe_lawyers, safe_students),
                ~ dplyr::case_when(
                  .x == "increase" ~ 1,
                  .x == "no_increase" ~ 0,
                  .x == "decrease" ~ -1
                  )),
         .keep = 'all')



law_skool_num |> 
  rstatix::t_test(formula = fair_lawyers ~ qset, paired = F,
                  conf.level = 0.95, detailed = T) |> 
  rstatix::add_significance()


# fair_lawyers where 1 = 'increase in confidence' and 0 = 'no increase'
# by Qset where 'Includes' = Qset A, 'Majority' = Qset B
lm(fair_lawyers ~ qset, data = law_skool_num) |> summary()

ggstats::ggcoef_model(
  lm(fair_lawyers ~ qset, data = law_skool_num),
  variable_labels = c(qset = 'Qset'),
  show_p_values = T, 
  conf.int = T, 
  conf.level = 0.95,
  signif_stars = T)


  
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





