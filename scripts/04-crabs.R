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

# q28_1 to q29 by treatment condition p-value based on chi^2 test of
# independence
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



#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####
# 


# function to create crosstabs using janitor::tabyl()
tabyl.crabs <- function(x){
  crab_df <- df |> select(group, all_of(x)) |> 
    janitor::tabyl(group, !!sym(x), show_na = F)
}

az_crabs <- map(qs_az, tabyl.crabs)



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
  group_by(group) |> 
  mutate(prop = round(n/sum(n),3),
         pct = prop*100,
         res = str_c(pct,'% (', n, ')', sep = "")) |> 
  ggplot(aes(x = response, y = pct, fill = group))+
  geom_col(position = 'dodge')+
  geom_text(aes(label = res), position = position_dodge(1.0), vjust = -0.5)+
  scale_fill_grey(start = 0.5, end = 0.1)+
  # \n in a string tells R to break the line there
  labs(y = "Percentage",
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


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####
# 3-way cross tables, yet again

# 2-way sjmisc::flat_table, sjmisc::flat_table(data, row_var, col_var, margin = c('row')) 
# only proportions
sjmisc::flat_table(data = df, group, q19, margin = 'row', digits = 3, show.values = T)

# for 3-way crab,
# sjmisc::flat_table(data, group_var, iv, dv, margin, ...)
# good way to quickly see the results of the data, but not able to pipe ftable as a dataframe to print
sjmisc::flat_table(data = df, group, q7, q19, margin = 'row', digits = 2, show.values = T)


# q19, group
df |> 
  janitor::tabyl(group, q19, show_na = F) |> 
  janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Group",
                       col_name = "Q19")


# q19, q7, group
df |> 
  janitor::tabyl(q7, q19, group, show_na = F) |> 
  janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Q7",
                       col_name = "Q19")

# this will place the tables side by side, but that isn't desired here.
df |> 
  janitor::tabyl(q7, q19, group, show_na = F) |>
  kableExtra::kbl()

# this first binds the two tables by row but puts the group column first and
# repeats the group name
df |> 
  janitor::tabyl(q7, q19, group, show_na = F) |>
  dplyr::bind_rows(.id = 'group') |> 
  kableExtra::kbl()

# I think I can omit the 1st column, 'group', and then simply add header rows
# where necessary
df |> 
  janitor::tabyl(q7, q19, group, show_na = F) |>
  dplyr::bind_rows(.id = 'group') |> 
  select(-1) |> 
  kableExtra::kbl() |> 
    kableExtra::pack_rows("Control", 1,2) |> 
    kableExtra::pack_rows("Treatment", 3, 4)

# Now omitting the first 'group' column from the 3-way table, then piping the
# table into a kable with rows 'packed' by the group condition. This output
# works well enough
df |> 
  janitor::tabyl(q7, q19, group, show_na = F) |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title(
    'combined',
    row_name = "Q7",
    col_name = 'Q19') |>
  dplyr::bind_rows(.id = 'group') |> 
  select(-1) |> 
  kableExtra::kbl() |> 
  kableExtra::add_header_above(
    header = c(' ' = 1,
               'Vote Count Confidence for Maricopa County, AZ' = 4)) |> 
    kableExtra::pack_rows("Control", 1,2) |> 
    kableExtra::pack_rows("Treatment", 3, 4)


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# This tabyl-to-gt-style table works well. It functions as a good table to use
# for html display of a 3-way cross table, but the complexity of just putting
# the table together is frustrating. I have to read an entire book just to learn
# how to format the table. That being said, the options for customizing the
# output is really flexible.

df |> 
  janitor::tabyl(q7, q19, group, show_na = F) |> 
  # janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = T) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Q7. Legitimacy of 2020 Election",
                       col_name = "Q19") |>
  bind_rows(.id = "group_var") |>   
  gt::gt(
    rowname_col = "row_var",
    groupname_col = "group_var") |>  
  gt::row_group_order(groups = c("Control", "Treatment")) |> 
  gt::cols_label(
    c('Q7. Legitimacy of 2020 Election/Q19') ~ "Q7. Legitimacy of 2020 Election") |>
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
  gt::tab_style(
    style = gt::cell_text(weight = 'bold'),
    locations = gt::cells_column_spanners()
    ) |> 
  gt::tab_footnote(
    footnote = "Table reflects row percentages. NAs omitted") |> 
  gt::tab_footnote(
    footnote = "Q19. How confident are you that votes in Maricopa County, AZ will be counted as voters intend in the elections this November?",
    locations = gt::cells_column_spanners()) |> 
  gt::tab_footnote(
    footnote = "Q7.Regardless of whom you supported in the 2020 election, \ndo you think Joe Biden's election as president was legitimate, or was he not legitimately elected?",
    locations = gt::cells_column_labels('Q7. Legitimacy of 2020 Election/Q19')
    ) |> 
  gt::cols_align(align = 'left', columns = everything()) |> 
  gt::tab_options(
    table.font.size = "small",
    data_row.padding = gt::px(1)
  )



# q21 by q7 by group

df |> 
  janitor::tabyl(q7, q21, group, show_na = F) |> 
  # janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Q7. Legitimacy of 2020 Election",
                       col_name = "Q21") |>
  bind_rows(.id = "group_var") |>   
  gt::gt(
    rowname_col = "row_var",
    groupname_col = "group_var") |>  
  gt::row_group_order(groups = c("Control", "Treatment")) |> 
  gt::cols_label(
    c('Q7. Legitimacy of 2020 Election/Q21') ~ "Q7. Legitimacy of 2020 Election") |>
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
    label = "Q21. Election staff commitment to fairness and accuracy"
  ) |> 
  gt::tab_style(
    style = gt::cell_text(weight = 'bold'),
    locations = gt::cells_column_spanners()
    ) |> 
  gt::tab_footnote(
    footnote = "Table reflects row percentages. NAs omitted") |> 
  gt::tab_footnote(
    footnote = "Q21. Think about the election staff and volunteers who handle the administration and conduct of elections in Maricopa County, AZ. How committed do you think they will be to making sure the elections held this November are fair and accurate?",
    locations = gt::cells_column_spanners()) |> 
  gt::tab_footnote(
    footnote = "Q7.Regardless of whom you supported in the 2020 election, \ndo you think Joe Biden's election as president was legitimate, or was he not legitimately elected?",
    locations = gt::cells_column_labels('Q7. Legitimacy of 2020 Election/Q21')
    ) |> 
  gt::cols_align(align = 'left', columns = everything()) |> 
  gt::tab_options(
    table.font.size = "small",
    data_row.padding = gt::px(1)
  )


# q22, q7, group

df |> 
  janitor::tabyl(q7, q22, group, show_na = F) |> 
  # janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Q7. Legitimacy of 2020 Election",
                       col_name = "Q22") |>
  bind_rows(.id = "group_var") |>   
  gt::gt(
    rowname_col = "row_var",
    groupname_col = "group_var") |>  
  gt::row_group_order(groups = c("Control", "Treatment")) |> 
  gt::cols_label(
    c('Q7. Legitimacy of 2020 Election/Q22') ~ "Q7. Legitimacy of 2020 Election") |>
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
    label = "Q22. Voting process will be fair"
  ) |> 
  gt::tab_style(
    style = gt::cell_text(weight = 'bold'),
    locations = gt::cells_column_spanners()
    ) |> 
  gt::tab_footnote(
    footnote = "Table reflects row percentages. NAs omitted") |> 
  gt::tab_footnote(
    footnote = "Q22. How confident are you that the voting process will be fair in Maricopa County, AZ?",
    locations = gt::cells_column_spanners()) |> 
  gt::tab_footnote(
    footnote = "Q7.Regardless of whom you supported in the 2020 election, \ndo you think Joe Biden's election as president was legitimate, or was he not legitimately elected?",
    locations = gt::cells_column_labels('Q7. Legitimacy of 2020 Election/Q22')
    ) |> 
  gt::cols_align(align = 'left', columns = everything()) |> 
  gt::tab_options(
    table.font.size = "small",
    data_row.padding = gt::px(1)
  )



# q23, q7, group

df |> 
  janitor::tabyl(q7, q23, group, show_na = F) |> 
  # janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Q7. Legitimacy of 2020 Election",
                       col_name = "Q23") |>
  bind_rows(.id = "group_var") |>   
  gt::gt(
    rowname_col = "row_var",
    groupname_col = "group_var") |>  
  gt::row_group_order(groups = c("Control", "Treatment")) |> 
  gt::cols_label(
    c('Q7. Legitimacy of 2020 Election/Q23') ~ "Q7. Legitimacy of 2020 Election") |>
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
    label = "Q23. Voting outcomes will be fair"
  ) |> 
  gt::tab_style(
    style = gt::cell_text(weight = 'bold'),
    locations = gt::cells_column_spanners()
    ) |> 
  gt::tab_footnote(
    footnote = "Table reflects row percentages. NAs omitted") |> 
  gt::tab_footnote(
    footnote = "Q23. How confident are you that the voting outcomes will be fair in Maricopa County, AZ?",
    locations = gt::cells_column_spanners()) |> 
  gt::tab_footnote(
    footnote = "Q7.Regardless of whom you supported in the 2020 election, \ndo you think Joe Biden's election as president was legitimate, or was he not legitimately elected?",
    locations = gt::cells_column_labels('Q7. Legitimacy of 2020 Election/Q23')
    ) |> 
  gt::cols_align(align = 'left', columns = everything()) |> 
  gt::tab_options(
    table.font.size = "small",
    data_row.padding = gt::px(1)
  )


# q24, q7, group

df |> 
  janitor::tabyl(q7, q24, group, show_na = F) |> 
  # janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Q7. Legitimacy of 2020 Election",
                       col_name = "Q24") |>
  bind_rows(.id = "group_var") |>   
  gt::gt(
    rowname_col = "row_var",
    groupname_col = "group_var") |>  
  gt::row_group_order(groups = c("Control", "Treatment")) |> 
  gt::cols_label(
    c('Q7. Legitimacy of 2020 Election/Q24') ~ "Q7. Legitimacy of 2020 Election") |>
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
    label = "Q24. Confidence in security of election technology"
  ) |> 
  gt::tab_style(
    style = gt::cell_text(weight = 'bold'),
    locations = gt::cells_column_spanners()
    ) |> 
  gt::tab_footnote(
    footnote = "Table reflects row percentages. NAs omitted") |> 
  gt::tab_footnote(
    footnote = "Q24. How confident are you that election systems in Maricopa County, AZ will be secure from hacking and other technological threats?",
    locations = gt::cells_column_spanners()) |> 
  gt::tab_footnote(
    footnote = "Q7.Regardless of whom you supported in the 2020 election, \ndo you think Joe Biden's election as president was legitimate, or was he not legitimately elected?",
    locations = gt::cells_column_labels('Q7. Legitimacy of 2020 Election/Q24')
    ) |> 
  gt::cols_align(align = 'left', columns = everything()) |> 
  gt::tab_options(
    table.font.size = "small",
    data_row.padding = gt::px(1)
  )

# q25, q7, group


df |> 
  janitor::tabyl(q7, q25, group, show_na = F) |> 
  # janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Q7. Legitimacy of 2020 Election",
                       col_name = "Q25") |>
  bind_rows(.id = "group_var") |>   
  gt::gt(
    rowname_col = "row_var",
    groupname_col = "group_var") |>  
  gt::row_group_order(groups = c("Control", "Treatment")) |> 
  gt::cols_label(
    c('Q7. Legitimacy of 2020 Election/Q25') ~ "Q7. Legitimacy of 2020 Election") |>
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
    label = "Q25. Concern for voter intimidation, threats, or violence"
  ) |> 
  gt::tab_style(
    style = gt::cell_text(weight = 'bold'),
    locations = gt::cells_column_spanners()
    ) |> 
  gt::tab_footnote(
    footnote = "Table reflects row percentages. NAs omitted") |> 
  gt::tab_footnote(
    footnote = "Q25. Thinking about Maricopa County, AZ, how concerned should voters feel about potential violence, threats of violence, or intimidation while voting in person at their local polling place?",
    locations = gt::cells_column_spanners()) |> 
  gt::tab_footnote(
    footnote = "Q7.Regardless of whom you supported in the 2020 election, \ndo you think Joe Biden's election as president was legitimate, or was he not legitimately elected?",
    locations = gt::cells_column_labels('Q7. Legitimacy of 2020 Election/Q25')
    ) |> 
  gt::cols_align(align = 'left', columns = everything()) |> 
  gt::tab_options(
    table.font.size = "small",
    data_row.padding = gt::px(1)
  )



# q26, q7, group

df |> 
  janitor::tabyl(q7, q26, group, show_na = F) |> 
  # janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Q7. Legitimacy of 2020 Election",
                       col_name = "Q26") |>
  bind_rows(.id = "group_var") |>   
  gt::gt(
    rowname_col = "row_var",
    groupname_col = "group_var") |>  
  gt::row_group_order(groups = c("Control", "Treatment")) |> 
  gt::cols_label(
    c('Q7. Legitimacy of 2020 Election/Q26') ~ "Q7. Legitimacy of 2020 Election") |>
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
    label = "Q26. In-person voter safety at polling sites"
  ) |> 
  gt::tab_style(
    style = gt::cell_text(weight = 'bold'),
    locations = gt::cells_column_spanners()
    ) |> 
  gt::tab_footnote(
    footnote = "Table reflects row percentages. NAs omitted") |> 
  gt::tab_footnote(
    footnote = "Q26. How confident, if at all, are you that in person polling places in Maricopa County, AZ will be safe places for voters to cast their ballots during the upcoming elections in November?",
    locations = gt::cells_column_spanners()) |> 
  gt::tab_footnote(
    footnote = "Q7.Regardless of whom you supported in the 2020 election, \ndo you think Joe Biden's election as president was legitimate, or was he not legitimately elected?",
    locations = gt::cells_column_labels('Q7. Legitimacy of 2020 Election/Q26')
    ) |> 
  gt::cols_align(align = 'left', columns = everything()) |> 
  gt::tab_options(
    table.font.size = "small",
    data_row.padding = gt::px(1)
  )


# q27, q7, group

df |> 
  janitor::tabyl(q7, q27, group, show_na = F) |> 
  # janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Q7. Legitimacy of 2020 Election",
                       col_name = "Q27") |>
  bind_rows(.id = "group_var") |>   
  gt::gt(
    rowname_col = "row_var",
    groupname_col = "group_var") |>  
  gt::row_group_order(groups = c("Control", "Treatment")) |> 
  gt::cols_label(
    c('Q7. Legitimacy of 2020 Election/Q27') ~ "Q7. Legitimacy of 2020 Election") |>
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
    label = "Q27. Approval of election officials in Maricopa County, AZ"
  ) |> 
  gt::tab_style(
    style = gt::cell_text(weight = 'bold'),
    locations = gt::cells_column_spanners()
    ) |> 
  gt::tab_footnote(
    footnote = "Table reflects row percentages. NAs omitted") |> 
  gt::tab_footnote(
    footnote = "Q27. Do you approve or disapprove of the way election officials in Maricopa County, AZ are handling their jobs?",
    locations = gt::cells_column_spanners()) |> 
  gt::tab_footnote(
    footnote = "Q7.Regardless of whom you supported in the 2020 election, \ndo you think Joe Biden's election as president was legitimate, or was he not legitimately elected?",
    locations = gt::cells_column_labels('Q7. Legitimacy of 2020 Election/Q27')
    ) |> 
  gt::cols_align(align = 'left', columns = everything()) |> 
  gt::tab_options(
    table.font.size = "small",
    data_row.padding = gt::px(1)
  )

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####
df |> 
  group_by(group) |> 
  sjmisc::frq(q26)

df |>  
  sjmisc::frq(q26)

df |> 
  janitor::tabyl(group, q19, show_na = F) |>  
  janitor::adorn_totals('both', na.rm = T) |> 
  janitor::adorn_percentages(denominator = 'row') |>
  janitor::adorn_pct_formatting(digits = 1, affix_sign = F) |>
  janitor::adorn_ns() |> 
  janitor::adorn_title('combined')



df |> 
  janitor::tabyl(group, q22, show_na = F) |>  
  janitor::adorn_totals('both', na.rm = T) |> 
  janitor::adorn_percentages(denominator = 'row') |>
  janitor::adorn_pct_formatting(digits = 1, affix_sign = F) |>
  janitor::adorn_ns() |> 
  janitor::adorn_title(
    'combined',
    row_name = "Group",
    col_name = 'Q22') |> 
  kableExtra::kbl() |> 
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "bordered", "condensed", "responsive"),
    latex_options = "basic",
    font_size = 13
  )



df |> 
  janitor::tabyl(group, q25, show_na = F) |>  
  janitor::adorn_totals('both', na.rm = T) |> 
  janitor::adorn_percentages(denominator = 'row') |>
  janitor::adorn_pct_formatting(digits = 1, affix_sign = F) |>
  janitor::adorn_ns() |> 
  janitor::adorn_title(
    'combined',
    row_name = "Group",
    col_name = 'Q25') |> 
  kableExtra::kbl() |> 
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "bordered", "condensed", "responsive"),
    latex_options = "basic",
    font_size = 13
  )




df |> 
  janitor::tabyl(group, q26, show_na = F) |>  
  janitor::adorn_totals('both', na.rm = T) |> 
  janitor::adorn_percentages(denominator = 'row') |>
  janitor::adorn_pct_formatting(digits = 1, affix_sign = F) |>
  janitor::adorn_ns() |> 
  janitor::adorn_title(
    'combined',
    row_name = "Group",
    col_name = 'Q26') |> 
  kableExtra::kbl() |> 
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "bordered", "condensed", "responsive"),
    latex_options = "basic",
    font_size = 13
  )



df |> 
  janitor::tabyl(group, q27, show_na = F) |>  
  janitor::adorn_totals('both', na.rm = T) |> 
  janitor::adorn_percentages(denominator = 'row') |>
  janitor::adorn_pct_formatting(digits = 1, affix_sign = F) |>
  janitor::adorn_ns() |> 
  janitor::adorn_title(
    'combined',
    row_name = "Group",
    col_name = 'Q27') |> 
  kableExtra::kbl() |> 
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "bordered", "condensed", "responsive"),
    latex_options = "basic",
    font_size = 13
  )



################################################################################

# q41.4 vets
df |> 
  janitor::tabyl(group, q41.4.clps, show_na = F) |>
  janitor::adorn_totals('both') |> 
  janitor::adorn_percentages(denominator = 'row') |>
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns() |> 
  janitor::adorn_title(
    'combined',
    row_name = "Group",
    col_name = 'Q41.4')


# q41.5 lawyers
df |> 
  janitor::tabyl(group, q41.5.clps, show_na = F) |>
  janitor::adorn_totals('both') |> 
  janitor::adorn_percentages(denominator = 'row') |>
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns() |> 
  janitor::adorn_title(
    'combined',
    row_name = "Group",
    col_name = 'Q41.5')

# q41.6 college students
df |> 
  janitor::tabyl(group, q41.6.clps, show_na = F) |>
  janitor::adorn_totals('both') |> 
  janitor::adorn_percentages(denominator = 'row') |>
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns() |> 
  janitor::adorn_title(
    'combined',
    row_name = "Group",
    col_name = 'Q41.6')

# just an easier way for me to see all the counts and proportions I need in one
# output
df |> 
  select(group, q41.4, q41.5, q41.6) |> 
  drop_na() |> 
  # compute frequency within each combination
  group_by(group) |> 
  summarise(
    vets = sum(q41.4 == "Increase confidence a lot" | q41.4 == "Increase confidence somewhat", na.rm = T),
    vets_prop = vets/sum(group == "Control" | group == "Treatment", na.rm = T),
    lawyers = sum(q41.5 == "Increase confidence a lot" | q41.5 == "Increase confidence somewhat", na.rm = T),
    lawyers_prop = lawyers/sum(group == "Control" | group == "Treatment", na.rm = T),
    students = sum(q41.6 == "Increase confidence a lot" | q41.6 == "Increase confidence somewhat", na.rm = T),
    students_prop = students/sum(group == "Control" | group == "Treatment", na.rm = T)
  )
