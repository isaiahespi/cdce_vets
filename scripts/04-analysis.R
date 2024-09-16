# analysis

# load packages
library(tidyverse)
library(janitor)
library(easystats)
library(gtsummary)
library(gt)
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

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# save variables of interest as character vectors (or objects or whatever)
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

# do the same for the factor variables where levels have been collapsed
# excludes q5 through q8
qs.clps <- df |> 
  select(contains(".clps"), -q5.clps, -q6.clps, -q7, -q8.clps) |>
  colnames()

# now only the collapsed versions of q5 to q8 (q7 doesn't need collapsing)
ex.clps <- df |>
  select(q5.clps, q6.clps, q7, q8.clps) |>
  colnames()


## attempts to produce multiple tables at once :::::::::::::::::::::::::::::####

# so the base prop.table works with just a dataframe
df |> select(q7, q19.clps, group) |> table() |> prop.table(margin = 1) |> round(3)

# I can create a function to make prop.tables by group
make_ptabs <- function(x, y){
  tabyldf <- df |> select(all_of(x), all_of(y), group) |>
    table() |> prop.table(margin = 1) |> round(3)
}

# using map2(), I can iterate a function over two arguments at a time, which
# would be the variables in the dataframe
# ex.clps[3] = q7, qs.clps[1:9] = q19,q20,q22,q23,q24,q26,q30,q31,q33
ptabs_q7 <- map2(ex.clps[3], qs.clps[1:9], make_ptabs)

ptabs_q7[1] # output display is not great

# helps a bit but not great. 
kableExtra::kbl(ptabs_q7[1], format = "simple")

# janitor::tabyl is nicer, simple, and pretty easy.
df |> 
  janitor::tabyl(q7, q26.clps, group, show_na = F) |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |> 
  janitor::adorn_ns(position = "rear") |> 
  janitor::adorn_title("top",
                       row_name = "Q7.Legitimacy",
                       col_name = "Q19.Vote Count Trust")

# I can also write a function for janitor::tabyl() as well
# write function to make tabyl crosstabs 
make_crabs <- function(x, y){
  crab_df <- df |> 
    select(all_of(x), all_of(y), group) |>
    janitor::tabyl(!!sym(x), !!sym(y), group, show_na = F) |>
    janitor::adorn_percentages("row") |>
    janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
    janitor::adorn_ns() |> 
    janitor::adorn_title("top")
}

# same thing
# iterate a function over two arguments at a time
tabyls.q7 <- map2(ex.clps[3], qs.clps[1:9], make_crabs)

# Now I want to print them out one by one
for (i in tabyls.q7){
  print(i)
}

# Now I want to print them out as kableExtra::kbl() one by one
# this works to print the tables in the console
# but this doesn't give a table header to identify the treatment condition
for (i in tabyls.q7){
  print(kableExtra::kbl(i, format = "simple"))
}


# I can make another function that uses `gtsummary` instead of janitor
# write function but using gtsummary::tbl_cross instead of tabyl
# gtsummary alternative
make_crab_gtsummary <- function(x, y){
  crab_gt_df <- df |> select(group, all_of(x), all_of(y)) |>
    dplyr::mutate(group = forcats::fct_rev(group)) |> 
    gtsummary::tbl_strata(
    strata = group,
    ~ .x |>
      gtsummary::tbl_cross(
        row = !!sym(x),
        col = !!sym(y),
        percent = "row",
        statistic = "{p}%",
        margin = "row",
        missing = "no"
      )
  ) |> 
    gtsummary::bold_labels() |> 
    gtsummary::as_gt() |> 
    gt::tab_footnote("NA omitted. Table reflects row percentages") |>
    gt::tab_options(table.font.size = "small",
                    data_row.padding = gt::px(1))
}


# using map2(), I generate multiple crosstabs into a list within a list,
# assigned to 'gttabs.qx'
gttabs.q5 <- map2(ex.clps[1], qs.clps[1:9], make_crab_gtsummary)
gttabs.q6 <- map2(ex.clps[2], qs.clps[1:9], make_crab_gtsummary)
gttabs.q7 <- map2(ex.clps[3], qs.clps[1:9], make_crab_gtsummary)
gttabs.q8 <- map2(ex.clps[4], qs.clps[1:9], make_crab_gtsummary)

# example
gttabs.q7[1]

# now for each crosstab (i) in the list containing all my crosstabs, I print
for (i in gttabs.q7){
  print(i)
}


# Finally, I make another function that uses `gtsummary::as_kable_extra` instead
# of `as_gt` to see if I can generate and print the crosstabs in the style and
# format I want.
# This is supposedly better for PDFs, whereas gt is better for html
# I'm going to try it with HTML anyway, however
make_crab_as_kable <- function(x, y){
  crab_gtkbl_df <- df |> 
    select(group, all_of(x), all_of(y)) |>
    dplyr::mutate(group = forcats::fct_rev(group)) |> 
    gtsummary::tbl_strata(
    strata = group,
    ~ .x |>
      gtsummary::tbl_cross(
        row = !!sym(x),
        col = !!sym(y),
        percent = "row",
        statistic = "{p}%",
        margin = "row",
        missing = "no"
      )
  ) |> 
    gtsummary::bold_labels() |> 
    gtsummary::as_kable_extra(
      booktabs = T,
      longtable = T,
      linesep = ""
    ) |> 
    kableExtra::kable_styling(
      position = "left",
      bootstrap_options = c("striped", "hover", "responsive")
    )
}


# repeating usage of map2(),
kbl_crabs.q5 <- map2(ex.clps[1], qs.clps, make_crab_as_kable)
kbl_crabs.q6 <- map2(ex.clps[2], qs.clps, make_crab_as_kable)
kbl_crabs.q7 <- map2(ex.clps[3], qs.clps, make_crab_as_kable)
kbl_crabs.q8 <- map2(ex.clps[4], qs.clps, make_crab_as_kable)


kbl_crabs.q5[1]
kbl_crabs.q7[1]



# now for each crosstab (i) in the list containing all my crosstabs, I print
for (i in kbl_crabs.q7){
  print(i)
}


# other variables to run across 
party_crabs <- map2("partyid_3cat", qs.clps, make_crab_as_kable)
age_crabs <- map2("age_cat", qs.clps, make_crab_as_kable)
gender_crabs <- map2("gender", qs.clps, make_crab_as_kable)
race_crabs <- map2("race", qs.clps, make_crab_as_kable)

saveRDS(kbl_crabs.q5, file = "data/kbl_crabs.q5.RData")
saveRDS(kbl_crabs.q6, file = "data/kbl_crabs.q6.RData")
saveRDS(kbl_crabs.q7, file = "data/kbl_crabs.q7.RData")
saveRDS(kbl_crabs.q8, file = "data/kbl_crabs.q8.RData")
saveRDS(party_crabs, file = "data/party_crabs.RData")

# these are crosstabs contained in one list each saved as .RData
# each file contains a list of multiple crosstabs where responses from q19 to
# q46 are crossed by one pre-treatment survey question (e.g., q5) and grouped by
# experiment condition. That is, the explanatory variable is in the rows,
# responses from question 19 to question 46 are in the columns, and cross
# tabulation is further stratified by experimental condition, treatment or
# control.
readRDS("data/kbl_crabs.q5.RData")
readRDS("data/kbl_crabs.q6.RData")
readRDS("data/kbl_crabs.q7.RData")
readRDS("data/kbl_crabs.q8.RData")
readRDS("data/party_crabs.RData")














# iv = q5, dv = q19, z = group
kableExtra::kable(q5_crabs[[1]], format = "simple")




kableExtra::kbl(q5_crabs[3], format = "simple")
kableExtra::kbl(q5_crabs, format = "simple")


attr(df$q5.clps, "label")
attr(df$q5.clps, "labels")
attr(df$q5.clps, 'names')
attributes(df$q5.clps)




# iv = q5, dv = q19, z = group
kableExtra::kbl(q5_crabs[[1]]) |> 
  kableExtra::kable_styling(bootstrap_options = c("striped",
                                      "hover",
                                      "condensed",
                                      "responsive"))

# iv = q5, dv = q20, z = group
kableExtra::kbl(q5_crabs[[2]]) |> 
  kableExtra::kable_styling(bootstrap_options = c("striped",
                                      "hover",
                                      "condensed",
                                      "responsive"))


# iv = q5, dv = q21, z = group
kableExtra::kbl(q5_crabs[3]) |>
  kableExtra::kable_styling(bootstrap_options = c("striped",
                                      "hover",
                                      "condensed",
                                      "responsive"))


# This works a bit
do.call(rbind, lapply(combined_gt, tibble::as_tibble))

# this seems to do the same thing
map_df(combined_gt, tibble::as_tibble)


combined_gt_df <- map_df(combined_gt, tibble::as_tibble)
combined_gt_df




