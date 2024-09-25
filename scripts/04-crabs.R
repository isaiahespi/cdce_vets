# crabs


# load packages
library(tidyverse)
library(janitor)
library(kableExtra)
library(gt)
library(table1)


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

# Now I'm going to try to build a 3-way crosstable using the table1 package ####

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
  render.continuous = "MEAN (SD)",
  render.missing = NULL,
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
  overall = "Overall",
  render.categorical="FREQ (PCTnoNA%)",
  render.missing = NULL,
  # extra.col = list('P-value' = pvalue),
  caption = "Circumstantial Impact on Confidence in Fairness and Accuracy of Elections \nby Treatment Condition",
  footnote = "Table reflects column percentages. Percentages reflect of relative frequencies where NA are excluded from the denominator. \nP-values based on Pearson's Chi-squared test of independence.\n Each question listed here was prefaced with the following: Regardless of whether any of these are actually the case, how would the following impact your confidence in the fairness and accuracy of elections conducted this November?" 
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

stats.default(is.na(t4))

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



























