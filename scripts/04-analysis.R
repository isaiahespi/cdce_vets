# analysis

# load packages
library(tidyverse)
library(rlang)
library(janitor)
library(kableExtra)
# library(easystats)
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
  select(group, age_cat, gender_3cat, race, educ, milserv1, milserv2, milservfam) |> 
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
                       col_name = "Q26. Safe to vote in AZ")

# I can also write a function for janitor::tabyl() as well
# write function to make tabyl crosstabs 
make_crabs <- function(x, y){
  crab_df <- df |> 
    select(all_of(x), all_of(y), group) |>
    janitor::tabyl(group, !!sym(y), !!sym(x), show_na = F) |>
    janitor::adorn_percentages("row") |>
    janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
    janitor::adorn_ns() |> 
    janitor::adorn_title("top")
}

# same thing
# iterate a function over two arguments at a time
tabyls.q5 <- map2(ex.clps[1], qs_az, make_crabs)
tabyls.q6 <- map2(ex.clps[2], qs_az, make_crabs)
tabyls.q7 <- map2(ex.clps[3], qs_az, make_crabs)
tabyls.q8 <- map2(ex.clps[4], qs_az, make_crabs)
tabyls.partyid <- map2(political_qs[4], qs_az, make_crabs)

saveRDS(tabyls.q5, file = "data/tabyls.q5.RData")
saveRDS(tabyls.q6, file = "data/tabyls.q6.RData")
saveRDS(tabyls.q7, file = "data/tabyls.q7.RData")
saveRDS(tabyls.q8, file = "data/tabyls.q8.RData")
saveRDS(tabyls.partyid, file = "data/tabyls.partyid.RData")

tabyls.q5[1][[1]]


# Now I want to print them out one by one
for (i in tabyls.q7){
  print(i)
}


# Now I want to print them out as kableExtra::kbl() one by one
# this works to print the tables in the console
for (i in tabyls.q5){
  print(kableExtra::kbl(i, format = "pipe"))
}


for (i in tabyls.q6){
  print(kableExtra::kbl(i, format = "pipe"))
}


for (i in tabyls.q7){
  print(kableExtra::kbl(i, format = "pipe"))
}


for (i in tabyls.q8){
  print(kableExtra::kbl(i, format = "pipe"))
}



# iterate a function over two arguments at a time
tabyls.q5 <- map2(ex.clps[1], qs_az, make_crabs)
tabyls.q6 <- map2(ex.clps[2], qs_az, make_crabs)
tabyls.q7 <- map2(ex.clps[3], qs_az, make_crabs)
tabyls.q8 <- map2(ex.clps[4], qs_az, make_crabs)



# these are crosstabs contained in one list each saved as .RData
# each file contains a list of multiple crosstabs where responses from q19 to
# q46 are crossed by one pre-treatment survey question (e.g., q5) and grouped by
# experiment condition. That is, the explanatory variable is in the rows,
# responses from question 19 to question 46 are in the columns, and cross
# tabulation is further stratified by experimental condition, treatment or
# control.
readRDS("data/tabyls.q5.RData")
readRDS("data/tabyls.q6.RData")
readRDS("data/tabyls.q7.RData")
readRDS("data/tabyls.q8.RData")
readRDS("data/tabyls.partyid.RData")

round(prop.table(table(Q5=df$q5.clps, Q19=df$q19.clps, Group=df$group), margin = 1), 4) |> 
  gt::gt(rowname_col = "rowname", groupname_col = "group_var")


crosstable::crosstable(data = df, cols = c(q5.clps, q19.clps), by = group, 
                       percent_pattern = "{p_row} ({n})", percent_digits = 2,
                       showNA = "no", label = T) |>
  crosstable::as_gt() |>
  gt::tab_footnote("NA omitted. Table reflects row percentages") |>
  gt::tab_options(table.font.size = "small",
                    data_row.padding = gt::px(15), table.width = gt::pct(64))
  

crosstable::crosstable(data = df, cols = c(group, q5.clps), by = q19.clps, 
                       percent_pattern = "{p_row} ({n})", percent_digits = 2,
                       showNA = "no", label = T) |> 
  kableExtra::kbl()

crosstable::crosstable(data = df, cols = group, by = c(q5.clps, q19.clps), 
                       percent_pattern = "{p_row} ({n})", percent_digits = 2,
                       showNA = "no", label = F) |>
  crosstable::as_flextable()



df |> 
  janitor::tabyl(q5.clps, q19.clps, group, show_na = F) |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_title("top", 
                       row_name = "Q5", col_name = "Q19") |>
  bind_rows(.id = "group_var") |>    
  gt::gt(
    rowname_col = "row_var",
    groupname_col = "group_var") |> 
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups()
  )


df |>  
  janitor::tabyl(q5.clps, q19.clps, group, show_na = F) |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_title("top") |>
  kableExtra::kbl() |>  
  kableExtra::add_header_above(c("Attentive" =1, "Inattentive" = 1), align = "l") |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                            row_label_position = "l")



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
  gt::tab_footnote(footnote = "Table reflects row percentages. NAs omitted") |> 
  gt::tab_footnote(footnote = "Q19. How confident are you that votes in Maricopa County, AZ will be counted as voters intend in the elections this November?",
                   locations = gt::cells_column_spanners()) |> 
  gt::tab_footnote(footnote = "Q5. How often do you pay attention to what is going on in government and politics?",
                   locations = gt::cells_column_labels(columns = "...1")) |> 
  gt::tab_options(
    table.font.size = "small",
    data_row.padding = gt::px(1)
  )

df |> select(q19:q29) |> sjlabelled::get_label() |> writeClipboard()


df |>
  mutate(group = forcats::fct_rev(group)) |> 
  gtsummary::tbl_strata(
    strata = q5.clps,
    ~ .x |>
      gtsummary::tbl_cross(
        row = group,
        col = q19.clps,
        statistic = "{p}%",
        percent = "row",
        margin = "row",
        missing = "no"
      )
  ) |>
  gtsummary::bold_labels() |>
  gtsummary::as_gt() |> 
  gt::tab_header(
    title = gt::md("**Confidence all votes will be counted, AZ among politically attentive and inattentive respondents**"))|>
  gt::tab_footnote("NA omitted. Table reflects row percentages") |> 
  gt::tab_options(
    table.font.size = "small",
    table.width = gt::pct(70),
    data_row.padding = gt::px(1))

all_of(qs_az)
df |> 
  select(group, q5.clps, q19.clps) |> 
  datawizard::data_group(select = "group") |> 
  datawizard::data_tabulate(by = "q5.clps", proportions = "col", 
                            drop_levels = T, select = "q19.clps", remove_na = F,
                            collapse = T)


```{r}
#| include: false


# these are crosstabs contained in one list each saved as .RData
# each file contains a list of multiple crosstabs where responses from q19 to
# q46 are crossed by one pre-treatment survey question (e.g., q5) and grouped by
# experiment condition. That is, the explanatory variable is in the rows,
# responses from question 19 to question 46 are in the columns, and cross
# tabulation is further stratified by experimental condition, treatment or
# control.
tabyls.q5 <- readRDS("data/tabyls.q5.RData")
tabyls.q6 <- readRDS("data/tabyls.q6.RData")
tabyls.q7 <- readRDS("data/tabyls.q7.RData")
tabyls.q8 <- readRDS("data/tabyls.q8.RData")
tabyls.partyid <- readRDS("data/tabyls.partyid.RData")



```

  