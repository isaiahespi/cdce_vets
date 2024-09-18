# crabs


# load packages
library(tidyverse)
library(janitor)
library(kableExtra)
library(gt)
library(table1)

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
  gt::tab_footnote(footnote = "Table reflects row percentages. NAs omitted") |> 
  gt::tab_footnote(footnote = "Q19. How confident are you that votes in Maricopa County, AZ will be counted as voters intend in the elections this November?",
                   locations = gt::cells_column_spanners()) |> 
  gt::tab_footnote(footnote = "Q5. How often do you pay attention to what is going on in government and politics?",
                   locations = gt::cells_column_labels(columns = "...1")) |> 
  gt::tab_options(
    table.font.size = "small",
    data_row.padding = gt::px(1)
  )


## Building tables using the table1 package ::::::::::::::::::::::::::::::::####
# descriptive stats for df
t1.demog <- table1::table1(
  ~age_cat
  + gender_3cat
  + race
  + educ
  + partyid_3cat
  + milserv1
  + milserv2
  + milservfam
  + voted2020.clps
  + choice2020
  + voteintent
  | group,
  data = df,
  overall = "Overall", 
  caption = "Survey sample characteristics by treatment condition",
  footnote = "Table reflects column percentages.")


t1.demog <- table1::t1kable(
  t1.demog,
  format = "html",
  booktabs = F ) |>
  kableExtra::kable_styling(
    bootstrap_options = "basic",
    latex_options = "basic",
    fixed_thead = T
  )

## Now I'm going to try to build a 3-way crosstable using the table1 package

# need to 
df <- df |>
  mutate(group = forcats::fct_rev(group))
  
  
# This is great. Only the iv is in the columns, and dv is in rows.
# first three post-vignette questions by experiment condition and q5 (attention to politics)
table1::table1(~q19.clps + q20.clps + q21.clps | group*q5.clps, data = df,
               transpose = F, overall = "Overall")

# function to compute the p-value for continuous or categorical variables
pvalue <- function(x, ...) {
    # Construct vectors of data y, and groups (strata) g
    y <- unlist(x)
    g <- factor(rep(1:length(x), times=sapply(x, length)))
    if (is.numeric(y)) {
        # For numeric variables, perform a standard 2-sample t-test
        p <- t.test(y ~ g)$p.value
    } else {
        # For categorical variables, perform a chi-squared test of independence
        p <- chisq.test(table(y, g))$p.value
    }
    # Format the p-value, using an HTML entity for the less-than sign.
    # The initial empty string places the output on the line below the variable label.
    c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

# Pearson's chi-squared test is performed of the null hypothesis that the joint
# distribution of the cell counts in a 2-dimensional contingency table is the
# product of the row and column marginals

# The p-value function in R takes the "Overall" column into account. Have to run
# Table 1 without 'Overall'

# q19 to q27 by treatment condition. p-value based on chi^2 test of indpendence
t2 <- table1::table1(
  ~ q19.clps
  + q20.clps
  + q21.clps
  + q22.clps
  + q23.clps
  + q24.clps
  + q25.clps
  + q26.clps
  + q27.clps
  + q29.clps
  | group,
  data = df,
  transpose = F,
  overall = F,
  extra.col = list('P-value' = pvalue),
  caption = "Trust and Confidence in Maricopa County, AZ Elections by Treatment Condition",
  footnote = "Table reflects column percentages. \nP-values based on Pearson's Chi-squared test of independence."
)

t2 <- table1::t1kable(
  t2,
  format = "html", align = "l",
  booktabs = F ) |>
  kableExtra::kable_styling(
    bootstrap_options = "basic",
    latex_options = "basic",
    fixed_thead = T
  )

print(t2)

# q28_1 to q29 by treatment condition p-value based on chi^2 test of indpendence
t3 <- table1::table1(
  ~ q28_1.clps
  + q28_2.clps
  + q28_3.clps
  + q28_4.clps
  + q28_5.clps
  | group,
  data = df,
  transpose = F,
  overall = F,
  extra.col = list('P-value' = pvalue),
  caption = "Expectation of Electoral Fraud in Maricopa County, AZ \nby Treatment Condition",
  footnote = "Table reflects column percentages. \nP-values based on Pearson's Chi-squared test of independence."
)

t3 <- table1::t1kable(
  t3,
  format = "html",
  booktabs = F ) |>
  kableExtra::kable_styling(
    bootstrap_options = "basic",
    latex_options = "basic",
    fixed_thead = T
  )

print(t3)


# confidence impact by treatment condition ::::::::::::::
# q41_1 to q43_6 by treatment condition p-value based on chi^2 test
t4 <- table1::table1(
  ~ q41_1.clps
  + q41_2.clps
  + q41_3.clps
  + q41_4.clps
  + q41_5.clps
  + q41_6.clps
  | group,
  data = df,
  transpose = F,
  overall = F,
  extra.col = list('P-value' = pvalue),
  caption = "Circumstantial Impact on Confidence in Fairness and Accuracy of Elections \nby Treatment Condition",
  footnote = "Table reflects column percentages. \nP-values based on Pearson's Chi-squared test of independence.\n Each question listed here was prefaced with the following: Regardless of whether any of these are actually the case, how would the following impact your confidence in the fairness and accuracy of elections conducted this November?" 
)

t4 <- table1::t1kable(
  t4,
  format = "html",
  booktabs = F ) |>
  kableExtra::kable_styling(
    bootstrap_options = "basic",
    latex_options = "basic",
    fixed_thead = T
  )

print(t4)



t5 <- table1::table1(
  ~ q43_1.clps
  + q43_2.clps
  + q43_3.clps
  + q43_4.clps
  + q43_5.clps
  + q43_6.clps
  | group,
  data = df,
  transpose = F,
  overall = F,
  extra.col = list('P-value' = pvalue),
  caption = "Circumstantial Impact on Confidence of voter safety \nby Treatment Condition",
  footnote = "Table reflects column percentages. \nP-values based on Pearson's Chi-squared test of independence.\n Each question listed here was prefaced with the following: How would the following impact your confidence that voters are safe from violence, threats of violence, or intimidation while voting in-person during elections this November?" 
)

t5 <- table1::t1kable(
  t5,
  format = "html",
  booktabs = F ) |>
  kableExtra::kable_styling(
    bootstrap_options = "basic",
    latex_options = "basic",
    fixed_thead = T
  )

print(t5)


# I need to compare the set above to the majority language set of questions
# Unfortuately I can't use the table1 package


























