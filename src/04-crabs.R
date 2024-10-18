# crabs


# load packages
library(tidyverse)
library(janitor)
library(kableExtra)
library(gt)
library(table1)

## :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####
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

## process for creating multiple crosstabs at once :::::::::::::::::::::::::####

# selected survey questions/vars from dataframe 
qs <- df |>
  select(
    age_cat, gender_3cat, hisp, race, milserv1, milserv2, milservfam,
    voted2020, voted2020.clps, choice2020, voteintent,
    partyid_3cat, ideo, ideolean,
    q5, q6, q7, q8,
    q19,
    q20,
    q21,
    q22,
    q23,
    q24,
    q25,
    q26,
    q27,
    q28_1,
    q28_2,
    q28_3,
    q28_4,
    q28_5,
    q29,
    q30:q39,
    q40_1:q40_5,
    q41.1:q43.6,
    q48,
    q49,
    q50
  ) |>
  colnames()

# function to create crosstabs using janitor::tabyl()
tabyl.maps <- function(x){
  crabs <- df |> select(group, all_of(x)) |> 
    tabyl.crabs(var1= group, var2 = !!sym(x), 
                show_na = F, 
                totals = T, 
                margin_pcts = "row", digits = 2, affix_sign = F)
}

# this will run the function tabyl.maps over every question of interest in the
# survey. A tabyl style cross tab will be constructed for each question by group
# condition, and all the crosstabs will be assigned to a list.
crabs <- map(qs, tabyl.maps)

# run this to output all the crosstabs in the console
crabs |> kableExtra::kbl(format = 'simple')

# to avoid having to call it ever again, save all the results output to a text
# file (.txt) using the sink() function.
sink(file = "~/R/GA_work/CDCE/cdce_vets/notes/crosstabs.txt") # Redirect output to file

crabs |> kableExtra::kbl(format = 'simple')

sink() # Resume writing output to console

crabs[[19]]

# run the following to get the list into one dataframe
# warning: questions are no longer apparent
purrr::reduce(crabs, dplyr::full_join)

# print the crosstab to kable style output
crabs[[19]] |> 
  kableExtra::kbl() |> 
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "bordered", "condensed", "responsive"),
    latex_options = "basic",
    font_size = 13
  )

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####
# Basic 2-way cross tables for reference

# instead of using the function, this is the manual way to go about it

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



# q20, group
df |> 
  janitor::tabyl(group, q20, show_na = F) |> 
  janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Group",
                       col_name = "Q20")



# q21, group
df |> 
  janitor::tabyl(group, q21, show_na = F) |> 
  janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Group",
                       col_name = "Q21")

# q22, group
df |> 
  janitor::tabyl(group, q22, show_na = F) |> 
  janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Group",
                       col_name = "Q22")


# q23, group
df |> 
  janitor::tabyl(group, q23, show_na = F) |> 
  janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Group",
                       col_name = "Q23")

# q24, group
df |> 
  janitor::tabyl(group, q24, show_na = F) |> 
  janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Group",
                       col_name = "Q24")

# q25, group
df |> 
  janitor::tabyl(group, q25, show_na = F) |> 
  janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Group",
                       col_name = "Q25")

# q26, group
df |> 
  janitor::tabyl(group, q26, show_na = F) |> 
  janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Group",
                       col_name = "Q26")


# q27, group
df |> 
  janitor::tabyl(group, q27, show_na = F) |> 
  janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Group",
                       col_name = "Q27")


# Need to skip Q28 here since it is a series....

# q29, group
df |> 
  janitor::tabyl(group, q29, show_na = F) |> 
  janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Group",
                       col_name = "Q29")

df |> 
  janitor::tabyl(group, q29.clps, show_na = F) |> 
  janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Group",
                       col_name = "Q29")

# q36, group
df |> 
  janitor::tabyl(group, q36, show_na = F) |> 
  janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Group",
                       col_name = "Q36")
# q37, group
df |> 
  janitor::tabyl(group, q37, show_na = F) |> 
  janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Group",
                       col_name = "Q37")
# q38, group
df |> 
  janitor::tabyl(group, q38, show_na = F) |> 
  janitor::adorn_totals("both") |> 
  janitor::adorn_percentages("row") |> 
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns("rear") |> 
  janitor::adorn_title("combined", 
                       row_name = "Group",
                       col_name = "Q38")


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


# q43.4 vets
df |> 
  janitor::tabyl(group, q43.4, show_na = F) |>
  janitor::adorn_totals('both') |> 
  janitor::adorn_percentages(denominator = 'row') |>
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns() |> 
  janitor::adorn_title(
    'combined',
    row_name = "Group")

df |> 
  janitor::tabyl(group, q43.5, show_na = F) |>
  janitor::adorn_totals('both') |> 
  janitor::adorn_percentages(denominator = 'row') |>
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns() |> 
  janitor::adorn_title(
    'combined',
    row_name = "Group")

df |> 
  janitor::tabyl(group, q43.6, show_na = F) |>
  janitor::adorn_totals('both') |> 
  janitor::adorn_percentages(denominator = 'row') |>
  janitor::adorn_pct_formatting(digits = 2, affix_sign = F) |>
  janitor::adorn_ns() |> 
  janitor::adorn_title(
    'combined',
    row_name = "Group")
