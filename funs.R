# functions

# fun 1:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# function to create crosstabs using janitor::tabyl() with adorn functions as
# arguments
# function to create crosstabs using janitor::tabyl()
# function to create crosstabs using janitor::tabyl()
tabyl.crabs <- function(dat, var1, var2, var3, show_na, totals = TRUE, margin_pcts = "row", digits = 2, affix_sign = F, ...){
  
  if (!missing(var1) && !missing(var2) && !missing(var3)){
    crab_df <- dat |> janitor::tabyl({{ var1 }}, {{ var2 }}, {{var3}})
  } else if (missing(var3) && !missing(var1) && !missing(var2)){
    crab_df <- dat |> janitor::tabyl({{ var1 }}, {{ var2 }})
  } else {
    stop("please specify var1 & var2 or var1 & var2 & var3. Use janitor::tabyl for tabyl call on single vector")
  }
  
  if (totals == TRUE) {
    crab_df |> 
      janitor::adorn_totals("both") |>
      janitor::adorn_percentages(margin_pcts) |>
      janitor::adorn_pct_formatting(digits = digits, affix_sign = affix_sign) |>
      janitor::adorn_ns("rear") |>
      janitor::adorn_title("combined")
  } else {
    crab_df |> 
      janitor::adorn_percentages(margin_pcts) |>
      janitor::adorn_pct_formatting(digits = digits, affix_sign = affix_sign) |>
      janitor::adorn_ns("rear") |>
      janitor::adorn_title("combined")
  }
  
}

# test it out. It works
tabyl.crabs(
  dat = df,
  var1 = group,
  var2 = q19,
  show_na = F,
  totals = T,
  margin_pcts = "row",
  digits = 2,
  affix_sign = F
)

# works with 3 variables
tabyl.crabs(
  dat = df,
  var1 = group,
  var2 = q19,
  var3 = q7,
  show_na = F,
  totals = T,
  margin_pcts = "row",
  digits = 2,
  affix_sign = F
)

# works with minimal input
tabyl.crabs(df, group, q19)

# fun 2:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# this one isn't mine, but for use with the table1 package

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


# fun 3:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####
# compute counts and proportions

count_prop <- function(df, var, sort = FALSE) {
  df |>
    count({{ var }}, sort = sort) |>
    mutate(prop = n / sum(n),
           n_miss = sum(is.na({{ var }})))
}

count_prop(df, q7)

# fun 4:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####
# counts the number of missing observations in rows

count_missing <- function(df, group_vars, x_var) {
  df |> 
    group_by(pick({{ group_vars }})) |> 
    summarize(
      n_miss = sum(is.na({{ x_var }})),
      .groups = "drop"
  )
}

count_missing(df = df, group_vars = group, x_var = q29 )

# fun 5:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# count using all the variables in the rows and columns, then use pivot_wider()
# to rearrange the counts into a grid

count_wide <- function(data, rows, cols) {
  data |> 
    count(pick(c({{ rows }}, {{ cols }}))) |> 
    pivot_wider(
      names_from = {{ cols }}, 
      values_from = n,
      names_sort = TRUE,
      values_fill = 0
    )
}

df |> count_wide(c(group, q7), q19)
