# Dummify all these factors

library(tidyverse)
library(easystats)
library(pewmethods)
library(surveytoolbox)

# dummy q19, q20, q22, q23, q24, q26, q30, q31, q33, q34, q35, q37 :::::::::####

# collapse factor vars and add new collapsed factors to data frame
dumdf <- data |>
  dplyr::select(rowID, q19, q20, q22, q23, q24, q26, q30, q31, q33, q34, q35, q37) |>
  dplyr::mutate(across(
    c(q19, q20, q22, q23, q24, q26, q30, q31, q33, q34, q35, q37),
    ~ forcats::fct_collapse(
      .,
      Not_confident = c("Not at all confident", "Not too confident"),
      Confident = c("Somewhat confident", "Very confident")
    ),
    .names = "{.col}_2lvl"
  ))

dumdf <- dumdf |> 
  dplyr::select(rowID, contains("_2lvl")) |> 
  pewmethods::dummify_factors(sep = "_")

dumdf <- dumdf |>
  dplyr::select(rowID, contains("_2lvl")) |>
  dplyr::mutate(
    q19_dum = dplyr::case_when(
      q19_2lvl_Not_confident == 1 ~ 0,
      q19_2lvl_Confident == 1 ~ 1,
      TRUE ~ NA
    ),
    q20_dum = dplyr::case_when(
      q20_2lvl_Not_confident == 1 ~ 0,
      q20_2lvl_Confident == 1 ~ 1,
      TRUE ~ NA
    ),
    q22_dum = dplyr::case_when(
      q22_2lvl_Not_confident == 1 ~ 0,
      q22_2lvl_Confident == 1 ~ 1,
      TRUE ~ NA
    ),
    q23_dum = dplyr::case_when(
      q23_2lvl_Not_confident == 1 ~ 0,
      q23_2lvl_Confident == 1 ~ 1,
      TRUE ~ NA
    ),
    q24_dum = dplyr::case_when(
      q24_2lvl_Not_confident == 1 ~ 0,
      q24_2lvl_Confident == 1 ~ 1,
      TRUE ~ NA
    ),
    q26_dum = dplyr::case_when(
      q26_2lvl_Not_confident == 1 ~ 0,
      q26_2lvl_Confident == 1 ~ 1,
      TRUE ~ NA
    ),
    q30_dum = dplyr::case_when(
      q30_2lvl_Not_confident == 1 ~ 0,
      q30_2lvl_Confident == 1 ~ 1,
      TRUE ~ NA
    ),
    q31_dum = dplyr::case_when(
      q31_2lvl_Not_confident == 1 ~ 0,
      q31_2lvl_Confident == 1 ~ 1,
      TRUE ~ NA
    ),
    q33_dum = dplyr::case_when(
      q33_2lvl_Not_confident == 1 ~ 0,
      q33_2lvl_Confident == 1 ~ 1,
      TRUE ~ NA
    ),
    q34_dum = dplyr::case_when(
      q34_2lvl_Not_confident == 1 ~ 0,
      q34_2lvl_Confident == 1 ~ 1,
      TRUE ~ NA
    ),
    q35_dum = dplyr::case_when(
      q35_2lvl_Not_confident == 1 ~ 0,
      q35_2lvl_Confident == 1 ~ 1,
      TRUE ~ NA
    ),
    q37_dum = dplyr::case_when(
      q37_2lvl_Not_confident == 1 ~ 0,
      q37_2lvl_Confident == 1 ~ 1,
      TRUE ~ NA
    ),
    .keep = "unused"
  )

# dummy q21 q32 ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

dum_q2132 <- data |>
  dplyr::select(rowID, q21, q32) |> 
  dplyr::mutate(across(c(q21, q32), ~forcats::fct_collapse(
    .,
    not_committed = c("Not at all committed", "Not too committed"),
    committed = c("Somewhat committed", "Very committed")))) 

dum_q2132 <- dum_q2132 |> 
  dplyr::select(rowID, q21, q32) |> 
  pewmethods::dummify_factors(sep = "_") |> 
  dplyr::mutate(
    q21_dum = dplyr::case_when(
      q21_not_committed == 1 ~ 0,
      q21_committed == 1 ~ 1,
      TRUE ~ NA),
    q32_dum = dplyr::case_when(
      q32_not_committed == 1 ~ 0,
      q32_committed == 1 ~ 1,
      TRUE ~ NA),
    .keep = "unused"
  )


# dummy q25 q36 ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# data |> select(q25, q36) |> summarise_all(~sum(is.na(.)))
# 
# 
# forcats::fct_na_value_to_level(data$q25) |> levels()
# forcats::fct_na_value_to_level(data$q36) |> levels()

dum_q2536 <- data |>
  dplyr::select(rowID, q25, q36) |>
  # whoops. Need to change "somewhat unconcerned" to "Not too concerned"
  # recall: fct_recode(x, old_level = new_level)
  dplyr::mutate(q36 =forcats::fct_recode(
    q36, 
    "Not at all concerned" = "Not at all concerned",
    "Not too concerned" = "Somewhat unconcerned",
    "Somewhat concerned" = "Somewhat concerned",
    "Very concerned" = "Very concerned")
    ) |>
  dplyr::mutate(across(
    c(q25, q36),
    ~ forcats::fct_collapse(
      .,
      not_concerned = c("Not at all concerned", "Not too concerned"),
      concerned = c("Somewhat concerned", "Very concerned")
    )
  )) 

dum_q2536 <- dum_q2536 |> 
  dplyr::select(rowID, q25, q36) |> 
  pewmethods::dummify_factors(sep = "_") |> 
  dplyr::mutate(
    q25_dum = dplyr::case_when(
      q25_not_concerned == 1 ~ 0,
      q25_concerned == 1 ~ 1,
      TRUE ~ NA),
    q36_dum = dplyr::case_when(
      q36_not_concerned == 1 ~ 0,
      q36_concerned == 1 ~ 1,
      TRUE ~ NA),
    .keep = "unused"
  )

# dummy q38 ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# data |> select(q38) |> summarise_all(~sum(is.na(.)))
# table(data$q38, useNA = "always")
# forcats::fct_na_value_to_level(data$q38) |> levels()

dum_q38 <- data |>
  dplyr::select(rowID, q38) |>
  dplyr::mutate(q38 =
           forcats::fct_collapse(
             q38,
             unsafe = c("Not safe at all", "Not too safe"),
             safe = c("Somewhat safe", "Very safe")))


dum_q38 <- dum_q38 |> 
  dplyr::select(rowID, q38) |> 
  pewmethods::dummify_factors(sep = "_") |> 
  dplyr::mutate(
    q38_dum = dplyr::case_when(
      q38_unsafe == 1 ~ 0,
      q38_safe == 1 ~ 1,
      TRUE ~ NA),
    .keep = "unused"
  )

# dum_q38 |> select(q38_dum) |> summarise_all(~sum(is.na(.)))
# table(dum_q38$q38_dum, useNA = "always")



# dummy q27 q39 ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

data |> dplyr::select(q27, q39) |> summarise_all(~sum(is.na(.)))
table(data$q27)
table(data$q39)

forcats::fct_na_value_to_level(data$q27) |> levels()
forcats::fct_na_value_to_level(data$q39) |> levels()

dum_q2739 <- data |>
  dplyr::select(rowID, q27, q39) |>
  dplyr::mutate(across(
    c(q27, q39),
    ~ forcats::fct_collapse(
      .,
      disapprove = c("Strongly disapprove", "Somewhat disapprove" ),
      approve = c("Somewhat approve", "Strongly approve")
    )
  )) 


dum_q2739 <- dum_q2739 |> 
  dplyr::select(rowID, q27, q39) |> 
  pewmethods::dummify_factors(sep = "_") |> 
  dplyr::mutate(
    q27_dum = dplyr::case_when(
      q27_disapprove == 1 ~ 0,
      q27_approve == 1 ~ 1,
      TRUE ~ NA),
    q39_dum = dplyr::case_when(
      q39_disapprove == 1 ~ 0,
      q39_approve == 1 ~ 1,
      TRUE ~ NA),
    .keep = "unused"
  )

# dum_q2739 |> select(q27_dum, q39_dum) |> summarise_all(~sum(is.na(.)))



# dummy q29 ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####


data |> dplyr::select(q29) |> summarise_all(~sum(is.na(.)))
table(data$q29)

forcats::fct_na_value_to_level(data$q29) |> levels()

dum_q29 <- data |>
  dplyr::select(rowID, q29) |>
  dplyr::mutate(q29 = forcats::fct_collapse(q29,
      oppose = c("Definitely should not adopt", "Probably should not adopt"),
      support = c("Probably should adopt", "Definitely should adopt")
    )) 

dum_q29 <- dum_q29 |> 
  dplyr::select(rowID, q29) |> 
  pewmethods::dummify_factors(sep = "_") |> 
  dplyr::mutate(
    q29_dum = dplyr::case_when(
      q29_oppose == 1 ~ 0,
      q29_support == 1 ~ 1,
      TRUE ~ NA),
    .keep = "unused"
  )

# dum_q29 |> select(q29_dum) |> summarise_all(~sum(is.na(.)))


# dummy q28 q40 ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# get levels for matrix style question Q28
data |>  dplyr::select(contains("28_"), contains("40_")) |>  sapply(levels)

data |> dplyr::select(rowID, contains("28_"), contains("40_")) |> 
  dplyr::select(contains("_"))

# collapse factor vars and add new collapsed factors to data frame
dum_2840 <- data |>
  dplyr::select(rowID, contains("28_"), contains("40_")) |> 
  dplyr::mutate(across(contains("_"), ~forcats::fct_collapse(
    .,
    Not_likely = c("Not likely at all", "Not too likely"),
    Likely = c("Somewhat likely", "Very likely")),
    .names = "{.col}_2lvl"))

dum_2840 <- dum_2840 |> 
  dplyr::select(rowID, contains("_2lvl")) |> 
  pewmethods::dummify_factors(sep = "_") |>  
  dplyr::mutate(
    q28_1_dum = dplyr::case_when(
    q28_1_2lvl_Not_likely == 1 ~ 0,
    q28_1_2lvl_Likely == 1 ~ 1,
    TRUE ~ NA),
    q28_2_dum = dplyr::case_when(
    q28_2_2lvl_Not_likely == 1 ~ 0,
    q28_2_2lvl_Likely == 1 ~ 1,
    TRUE ~ NA),
    q28_3_dum = dplyr::case_when(
    q28_3_2lvl_Not_likely == 1 ~ 0,
    q28_3_2lvl_Likely == 1 ~ 1,
    TRUE ~ NA),
    q28_4_dum = dplyr::case_when(
    q28_4_2lvl_Not_likely == 1 ~ 0,
    q28_4_2lvl_Likely == 1 ~ 1,
    TRUE ~ NA),
    q28_5_dum = dplyr::case_when(
    q28_5_2lvl_Not_likely == 1 ~ 0,
    q28_5_2lvl_Likely == 1 ~ 1,
    TRUE ~ NA),
    q40_1_dum = dplyr::case_when(
    q40_1_2lvl_Not_likely == 1 ~ 0,
    q40_1_2lvl_Likely == 1 ~ 1,
    TRUE ~ NA),
    q40_2_dum = dplyr::case_when(
    q40_2_2lvl_Not_likely == 1 ~ 0,
    q40_2_2lvl_Likely == 1 ~ 1,
    TRUE ~ NA),
    q40_3_dum = dplyr::case_when(
    q40_3_2lvl_Not_likely == 1 ~ 0,
    q40_3_2lvl_Likely == 1 ~ 1,
    TRUE ~ NA),
    q40_4_dum = dplyr::case_when(
    q40_4_2lvl_Not_likely == 1 ~ 0,
    q40_4_2lvl_Likely == 1 ~ 1,
    TRUE ~ NA),
    q40_5_dum = dplyr::case_when(
    q40_5_2lvl_Not_likely == 1 ~ 0,
    q40_5_2lvl_Likely == 1 ~ 1,
    TRUE ~ NA),
    .keep = "unused"
    )




# dummy q41 q43 q44 q46 ::::::::::::::::::::::::::::::::::::::::::::::::::::####

# collapse factor vars and add new collapsed factors to data frame
# I can't really "dummify" these factors into simply binary variables with
# values 1, 0. There were five response options with a neutral middle.
dum_q41434446 <- data |>
  dplyr::select(rowID, 
         contains("41_"),
         contains("43_"), 
         contains("44_"),
         contains("46_")
         ) |>  
  dplyr::mutate(across(contains("_"), ~forcats::fct_collapse(
    .,
    decrease = c("Decrease confidence a lot",
                      "Decrease confidence somewhat"),
    no_impact = "No impact on confidence",
    increase = c("Increase confidence somewhat",
                      "Increase confidence a lot"))))
  

dum_q41434446 |> 
  summarise_all(~sum(is.na(.)))

dum_q41434446 <- dum_q41434446 |> 
  dplyr::select(rowID, contains("_")) |> 
  pewmethods::dummify_factors(sep = "_") |>
  dplyr::mutate(
    q41_1_clps = dplyr::case_when(
    q41_1_decrease == 1 ~ -1,
    q41_1_no_impact == 1 ~ 0,
    q41_1_increase == 1 ~ 1,
    TRUE ~ NA),
    q41_2_clps = dplyr::case_when(
    q41_2_decrease == 1 ~ -1,
    q41_2_no_impact == 1 ~ 0,
    q41_2_increase == 1 ~ 1,
    TRUE ~ NA),
    q41_3_clps = dplyr::case_when(
    q41_3_decrease == 1 ~ -1,
    q41_3_no_impact == 1 ~ 0,
    q41_3_increase == 1 ~ 1,
    TRUE ~ NA),
    q41_4_clps = dplyr::case_when(
    q41_4_decrease == 1 ~ -1,
    q41_4_no_impact == 1 ~ 0,
    q41_4_increase == 1 ~ 1,
    TRUE ~ NA),
    q41_5_clps = dplyr::case_when(
    q41_5_decrease == 1 ~ -1,
    q41_5_no_impact == 1 ~ 0,
    q41_5_increase == 1 ~ 1,
    TRUE ~ NA),
    q41_6_clps = dplyr::case_when(
    q41_6_decrease == 1 ~ -1,
    q41_6_no_impact == 1 ~ 0,
    q41_6_increase == 1 ~ 1,
    TRUE ~ NA),
    q43_1_clps = dplyr::case_when(
    q43_1_decrease == 1 ~ -1,
    q43_1_no_impact == 1 ~ 0,
    q43_1_increase == 1 ~ 1,
    TRUE ~ NA),
    q43_2_clps = dplyr::case_when(
    q43_2_decrease == 1 ~ -1,
    q43_2_no_impact == 1 ~ 0,
    q43_2_increase == 1 ~ 1,
    TRUE ~ NA),
    q43_3_clps = dplyr::case_when(
    q43_3_decrease == 1 ~ -1,
    q43_3_no_impact == 1 ~ 0,
    q43_3_increase == 1 ~ 1,
    TRUE ~ NA),
    q43_4_clps = dplyr::case_when(
    q43_4_decrease == 1 ~ -1,
    q43_4_no_impact == 1 ~ 0,
    q43_4_increase == 1 ~ 1,
    TRUE ~ NA),
    q43_5_clps = dplyr::case_when(
    q43_5_decrease == 1 ~ -1,
    q43_5_no_impact == 1 ~ 0,
    q43_5_increase == 1 ~ 1,
    TRUE ~ NA),
    q43_6_clps = dplyr::case_when(
    q43_6_decrease == 1 ~ -1,
    q43_6_no_impact == 1 ~ 0,
    q43_6_increase == 1 ~ 1,
    TRUE ~ NA),
    q44_1_clps = dplyr::case_when(
    q44_1_decrease == 1 ~ -1,
    q44_1_no_impact == 1 ~ 0,
    q44_1_increase == 1 ~ 1,
    TRUE ~ NA),
    q44_2_clps = dplyr::case_when(
    q44_2_decrease == 1 ~ -1,
    q44_2_no_impact == 1 ~ 0,
    q44_2_increase == 1 ~ 1,
    TRUE ~ NA),
    q44_3_clps = dplyr::case_when(
    q44_3_decrease == 1 ~ -1,
    q44_3_no_impact == 1 ~ 0,
    q44_3_increase == 1 ~ 1,
    TRUE ~ NA),
    q44_4_clps = dplyr::case_when(
    q44_4_decrease == 1 ~ -1,
    q44_4_no_impact == 1 ~ 0,
    q44_4_increase == 1 ~ 1,
    TRUE ~ NA),
    q44_5_clps = dplyr::case_when(
    q44_5_decrease == 1 ~ -1,
    q44_5_no_impact == 1 ~ 0,
    q44_5_increase == 1 ~ 1,
    TRUE ~ NA),
    q44_6_clps = dplyr::case_when(
    q44_6_decrease == 1 ~ -1,
    q44_6_no_impact == 1 ~ 0,
    q44_6_increase == 1 ~ 1,
    TRUE ~ NA),
    q46_1_clps = dplyr::case_when(
    q46_1_decrease == 1 ~ -1,
    q46_1_no_impact == 1 ~ 0,
    q46_1_increase == 1 ~ 1,
    TRUE ~ NA),
    q46_2_clps = dplyr::case_when(
    q46_2_decrease == 1 ~ -1,
    q46_2_no_impact == 1 ~ 0,
    q46_2_increase == 1 ~ 1,
    TRUE ~ NA),
    q46_3_clps = dplyr::case_when(
    q46_3_decrease == 1 ~ -1,
    q46_3_no_impact == 1 ~ 0,
    q46_3_increase == 1 ~ 1,
    TRUE ~ NA),
    q46_4_clps = dplyr::case_when(
    q46_4_decrease == 1 ~ -1,
    q46_4_no_impact == 1 ~ 0,
    q46_4_increase == 1 ~ 1,
    TRUE ~ NA),
    q46_5_clps = dplyr::case_when(
    q46_5_decrease == 1 ~ -1,
    q46_5_no_impact == 1 ~ 0,
    q46_5_increase == 1 ~ 1,
    TRUE ~ NA),
    q46_6_clps = dplyr::case_when(
    q46_6_decrease == 1 ~ -1,
    q46_6_no_impact == 1 ~ 0,
    q46_6_increase == 1 ~ 1,
    TRUE ~ NA),
    .keep = "unused"
    )

# :::::::: ####

# 1st
df <- datawizard::data_merge(data, dumdf, join = "left", by = "rowID")

# 2nd
df <- datawizard::data_merge(df, dum_q2132, join = "left", by = "rowID") |> 
  dplyr::relocate(q21_dum, .after = q20_dum) |> 
  dplyr::relocate(q32_dum, .after = q31_dum)

# 3rd
df <- datawizard::data_merge(df, dum_q2536, join = "left", by = "rowID")

# 4th
df <- df |> dplyr::relocate(q25_dum, .before = q26_dum)

# 5th
df <- datawizard::data_merge(df, dum_q38, join = "left", by = "rowID") |> 
  dplyr::relocate(q38_dum, .after = q37_dum)

# 6th
df <- datawizard::data_merge(df, dum_q2739, join = "left", by = "rowID") |> 
  dplyr::relocate(q27_dum, .after = q26_dum) |> 
  dplyr::relocate(q39_dum, .after = q38_dum)

#7th
df <- datawizard::data_merge(df, dum_q29, join = "left", by = "rowID") |> 
  dplyr::relocate(q29_dum, .after = q27_dum)

#8th
df <- datawizard::data_merge(df, dum_2840, join = "left", by = "rowID") |> 
  dplyr::relocate(q28_1_dum:q28_5_dum, .after = q27_dum) |> 
  dplyr::relocate(q40_1_dum:q40_5_dum, .after = q39_dum)

# 9th
df <- datawizard::data_merge(df, dum_q41434446, join = "left", by = "rowID") |> 
  dplyr::relocate(q41_1_clps:q41_6_clps, .after = q40_5_dum) |> 
  dplyr::relocate(q43_1_clps:q43_6_clps, .after = q41_6_clps) |> 
  dplyr::relocate(q44_1_clps:q44_6_clps, .after = q43_6_clps) |> 
  dplyr::relocate(q46_1_clps:q46_6_clps, .after = q44_6_clps)

# Remove dummied dfs :::::::::::::::::::::::::::::::::::::::::::::::::::::::####

rm(dumdf,
   dum_2840,
   dum_q2132,
   dum_q2536,
   dum_q2739,
   dum_q29,
   dum_q38,
   dum_q41434446)

# copy variable labels to dummy vars :::::::::::::::::::::::::::::::::::::::####

# column 36 to 89 are factor vars with levels
# column 114 to 167 are the dummy vars

# check that lengths of selected vars are equal
# df |> dplyr::select(36:89) |> colnames() |> length()
# df |> dplyr::select(114:167) |> colnames() |> length()

# look at the var labels (i.e., 'label_var' column)
# df |> surveytoolbox::data_dict() |> View()


# create a `dum` df containing only the dummy vars
dum <- df |> dplyr::select(contains("_dum") | contains("clps"))

# create a `smrt` df containing only the associated factor vars
smrt <- df |> dplyr::select(36:89)

# Briefly, here are the steps
# 1. create named vector of smart_labels containing variable and variable label

# To quickly assign the variable labels, first create a named vector via
# deframe() with values as the variable labels and names as the variable names.
smrt_labels <- smrt |> labelled::generate_dictionary() |>
  dplyr::select(variable, label) |> 
  tibble::deframe()

# 2. pull `smrt_names` and `dum_names` each into a character vector

smrt_names <- smrt |> colnames()
dum_names <- dum |> colnames()

# 3. make a `real_dum_names` character vector. For some reason, when `colnames`
# are replaced via `data.table::set_names()`, the character vector dum_names
# ALSO changes. So an independent 'real_dum_names' is needed.
# dum_names |> writeClipboard()

real_dum_names <- c(
  "q19_dum",
  "q20_dum",
  "q21_dum",
  "q22_dum",
  "q23_dum",
  "q24_dum",
  "q25_dum",
  "q26_dum",
  "q27_dum",
  "q28_1_dum",
  "q28_2_dum",
  "q28_3_dum",
  "q28_4_dum",
  "q28_5_dum",
  "q29_dum",
  "q30_dum",
  "q31_dum",
  "q32_dum",
  "q33_dum",
  "q34_dum",
  "q35_dum",
  "q36_dum",
  "q37_dum",
  "q38_dum",
  "q39_dum",
  "q40_1_dum",
  "q40_2_dum",
  "q40_3_dum",
  "q40_4_dum",
  "q40_5_dum",
  "q41_1_clps",
  "q41_2_clps",
  "q41_3_clps",
  "q41_4_clps",
  "q41_5_clps",
  "q41_6_clps",
  "q43_1_clps",
  "q43_2_clps",
  "q43_3_clps",
  "q43_4_clps",
  "q43_5_clps",
  "q43_6_clps",
  "q44_1_clps",
  "q44_2_clps",
  "q44_3_clps",
  "q44_4_clps",
  "q44_5_clps",
  "q44_6_clps",
  "q46_1_clps",
  "q46_2_clps",
  "q46_3_clps",
  "q46_4_clps",
  "q46_5_clps",
  "q46_6_clps"
)

# 4. set the colnames of `dum` using the `smrt_names` character vector. Note how
# both the dataframe `dum` and the character vector `dum_names` change. But
# `real_dum_names` does not change.

data.table::setnames(dum, old = colnames(dum), new = smrt_names)
# dum
# dum_names
# real_dum_names

# 5. assign variable labels from `smrt_labels` to `dum` dataframe
# Now assign the labels using the splice operator. Using the splice operator,
# labels are assigned via matching against the variable name, which means that
# variable order does not matter.
dum <- dum |> 
  labelled::set_variable_labels(!!!smrt_labels)

# 6. set colnames of `dum` using `real_dum_names`
data.table::setnames(dum, old = dum_names, new = real_dum_names)
# dum

# 7. rip out `dum_labels` from `dum` dataframe using the deframe() method from
# before
dum_labels <- dum |> labelled::generate_dictionary() |> 
  dplyr::select(variable, label) |> 
  tibble::deframe()


# 8. assign those `dum_labels` to the dummy vars in the main df, achieving the
# final goal. 
df <- df |> 
  labelled::set_variable_labels(!!!dum_labels)

# 9. remove `smrt`, `dum`, `smrt_labels`, `smrt_names`, `dum_names`, `dum_labels`, and `real_dum_names`

rm(smrt, dum, smrt_labels, smrt_names, dum_names, dum_labels, real_dum_names)

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# collapse categorical variables with 4-point response items into factors with
# two levels. Those with 5-point response options are converted into factors
# with 3 levels. Affix ".clps" to colname to identify collapsed factor vars,
# e.g., q19.clps

df <- df |>
  dplyr::mutate(across(
    c(q19, q20, q22, q23, q24, q26, q30, q31, q33, q34, q35, q37),
    ~ forcats::fct_collapse(
      .,
      Not_confident = c("Not at all confident", "Not too confident"),
      Confident = c("Somewhat confident", "Very confident")
    ),
    .names = "{.col}.clps"
  )) |>
  dplyr::mutate(across(
    c(q21, q32),
    ~ forcats::fct_collapse(
      .,
      not_committed = c("Not at all committed", "Not too committed"),
      committed = c("Somewhat committed", "Very committed")
    ),
    .names = "{.col}.clps"
  )) |>
  # whoops. Need to change "somewhat unconcerned" to "Not too concerned"
  # recall: fct_recode(x, old_level = new_level)
  dplyr::mutate(
    q36 = forcats::fct_recode(
      q36,
      "Not at all concerned" = "Not at all concerned",
      "Not too concerned" = "Somewhat unconcerned",
      "Somewhat concerned" = "Somewhat concerned",
      "Very concerned" = "Very concerned"
    )
  ) |>
  dplyr::mutate(across(
    c(q25, q36),
    ~ forcats::fct_collapse(
      .,
      not_concerned = c("Not at all concerned", "Not too concerned"),
      concerned = c("Somewhat concerned", "Very concerned")
    ),
    .names = "{.col}.clps"
  )) |>
  dplyr::mutate(q38 =
           forcats::fct_collapse(
             q38,
             unsafe = c("Not safe at all", "Not too safe"),
             safe = c("Somewhat safe", "Very safe")
           )) |>
  dplyr::mutate(across(
    c(q27, q39),
    ~ forcats::fct_collapse(
      .,
      disapprove = c("Strongly disapprove", "Somewhat disapprove"),
      approve = c("Somewhat approve", "Strongly approve")
    ),
    .names = "{.col}.clps"
  )) |>
  dplyr::mutate(q29 = forcats::fct_collapse(
    q29,
    oppose = c("Definitely should not adopt", "Probably should not adopt"),
    support = c("Probably should adopt", "Definitely should adopt")
  )) |> 
  dplyr::mutate(across(q28_1:q28_5 | q40_1:q40_5, ~forcats::fct_collapse(
    .,
    Not_likely = c("Not likely at all", "Not too likely"),
    Likely = c("Somewhat likely", "Very likely")))) |>
  dplyr::mutate(
    across(
      q41_1:q41_6 |
        q43_1:q43_6 | q44_1:q44_6 | q46_1:q46_6,
      ~ forcats::fct_collapse(
        .,
        decrease = c("Decrease confidence a lot", "Decrease confidence somewhat"),
        no_impact = "No impact on confidence",
        increase = c("Increase confidence somewhat", "Increase confidence a lot")
      ),
    .names = "{.col}.clps"
    )
  )

df <- df |> 
  dplyr::mutate(
    q5.clps = datawizard::categorize(q5, "mean", labels = c("Inattentive", "Attentive")),
    q6.clps = datawizard::categorize(q6, "equal_length", n_groups = 3, labels = c("Low Favor", "Mid Favor", "High Favor")),
    q8.clps = datawizard::categorize(q8, "equal_length", n_groups = 3, labels = c("Low Trust", "Moderate Trust", "High Trust")))


df <- df |> 
  dplyr::mutate(gender_3cat = forcats::fct_collapse(
    gender,
    "Male" = "Male",
    "Female" = "Female",
    "Other/Refused" = c("Non-binary / third gender", "Prefer not to say")))


 