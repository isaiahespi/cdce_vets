# using data exported from Qualtrics on 2024-09-11 at 11:39AM 

# load packages
library(tidyverse)
library(haven)
library(surveytoolbox)
library(labelled)
library(sjlabelled)

# load raw SPSS export that includes display order vars
# downloaded 2024-09-11 at 11:38 AM
raw_spss <- haven::read_sav(file = "data-raw/vets_spss_do_2024-09-11_11.38.sav") 


# despite being a .sav file imported using `haven::read_sav()`, 
# the data set is not "haven_labelled"
# haven::is.labelled(raw_spss)

# take a look
# glimpse(raw_spss)

# A data dictionary contains metadata about the data. The
# `labelled::generate_dictionary` function is used to create a data
# dictionary.
# a data frame is loaded into the R environment with the number of
# observations equal to number of variables in the original data set.
raw_spss_dict <- raw_spss |> surveytoolbox::data_dict()
write.csv(raw_spss_dict, file = "codebooks/raw_spss_dict.csv")


# 1553 total observations, including non-citizens and non-consenters 
# dim(raw_spss)

# clean and process (wrangle) data set
data_spss <- raw_spss |>
  janitor::clean_names() |>
  # get rid of superfluous Qualtrics columns
  # Keep Qualtrics `response_id`
  select(!1:8 & !10:17 & !265) |>
  # put demographic data up front
  relocate(q59:q74, .before = q1) |>
  # rename demographic vars and timing vars and identify lucid vars
  dplyr::rename(
    q13_treat_time = q13_page_submit,
    q16_control_time = q16_page_submit,
    lucid_gender = gender,
    lucid_age = age,
    lucid_hhi = hhi,
    lucid_ethnicity = ethnicity,
    lucid_hispanic = hispanic,
    lucid_education = education,
    lucid_political_party = political_party,
    lucid_region = region,
    lucid_zip = zip,
    lucid_rid = rid,
    gender = q59,
    hisp = q60,
    race = q61,
    educ = q62,
    milserv1 = q63,
    milserv2 = q64,
    milservfam = q65,
    voted2020 = q66,
    choice2020 = q67,
    voteintent = q68,
    partyid = q69,
    partystr_rep = q70,
    partystr_dem = q71,
    partylean = q72,
    ideo = q73,
    ideolean = q74
  ) |>
  relocate(lucid_rid, .after = response_id) |>
  # set variable labels for embedded lucid vars
  # set variable labels for select questions
  labelled::set_variable_labels(
    lucid_age = "Lucid: age",
    lucid_gender = "Lucid: What is your gender?",
    lucid_hhi = "Lucid: What is your current Annual household income before taxes?",
    lucid_ethnicity = "Lucid: What is your race?",
    lucid_hispanic = "Lucid: Are you Hispanic, Latino, or Spanish Origin?",
    lucid_education = "Lucid: What is the highest level of education you have completed?",
    lucid_political_party = "Lucid: Generally Speaking, do you usually think yourself as a Republican, a Democrat, an Independent, or what?",
    lucid_region = "Lucid: For Region, ZIP is asked which is automatically mapped to the Region",
    lucid_zip = "Lucid: What is your ZIP code?",
    lucid_rid = "Lucid respondent ID",
    q1 = "Consent to Participate...",
    q28_1 = "There will be voter fraud...",
    q28_2 = "Many votes will not actually be counted",
    q28_3 = "Many people will show up to vote and be told they are not eligible",
    q28_4 = "A foreign country will tamper with the votes cast in Maricopa County, Arizona...",
    q28_5 = "Election officials in Maricopa County, Arizoa will try to discourage some people from voting",
    q40_1 = "There will be voter fraud...",
    q40_2 = "Many votes will not actually be counted",
    q40_3 = "Many people will show up to vote and be told they are not eligible",
    q40_4 = "A foreign country will tamper with the votes cast in your local area...",
    q40_5 = "Election officials in your community will try to discourage some people from voting",
    q41_1 = "Election officials test every machine used in the election to ensure they are secure.",
    q41_2 = "Election officials conduct audits of ballots after every election...",
    q41_3 = "Poll watchers affiliated with the political parties or candidates observe the election.",
    q41_4 = "Election staff and volunteers include military veterans and their family members from the community.",
    q41_5 = "Election staff and volunteers include lawyers from the community.",
    q41_6 = "Election staff and volunteers include college students from the community.",
    q43_1 = "Election officials ensure that law enforcement officers are present at polling sites.",
    q43_2 = "Poll watchers affiliated with the political parties or candidates observe the election.",
    q43_3 = "People holding signs or giving out literature in support of a candidate or ballot question.",
    q43_4 = "Election staff and volunteers include military veterans and their family members from the community.",
    q43_5 = "Election staff and volunteers include lawyers from the community.",
    q43_6 = "Election staff and volunteers include college students from the community.",
    q44_1 = "Election officials test every machine used in the election to ensure they are secure.",
    q44_2 = "Election officials conduct audits of ballots after every election to confirm the results were accurate.",
    q44_3 = "Poll watchers affiliated with the political parties or candidates observe the election.",
    q44_4 = "The majority of election staff and volunteers consist of military veterans and their family members from the community.",
    q44_5 = "The majority of election staff and volunteers consist of lawyers from the community.",
    q44_6 = "The majority of election staff and volunteers consist of college students from the community.",
    q46_1 = "Election officials ensure that law enforcement officers are present at polling sites.",
    q46_2 = "People holding signs or giving out literature in support of a candidate or ballot question.",
    q46_3 = "Poll watchers affiliated with the political parties or candidates observe the election.",
    q46_4 = "The majority of election staff and volunteers consist of military veterans and their family members from the community.",
    q46_5 = "The majority of election staff and volunteers consist of lawyers from the community.",
    q46_6 = "The majority of election staff and volunteers consist of college students from the community.",
    group = "Experimental condition",
    qset = "Condition A = question set [q41, q43] ; Condition B = question set [q44, q46]"
  ) |> 
  # exclude all non-consent 
  # exclude non-citizens
  #exclude those who failed 1st and 2nd attn check
  filter(q1 == 1,
         q3 != 3,
         q9 == 1 | q9 == 0 & q10 == 1) |>  
  # add a column of ascending sequential row ids starting at 1 at start of df
  tibble::rowid_to_column("rowID")

# dim(data_spss)
# 1388 valid respondents

# dplyr::glimpse(data_spss)

# verify that `age` and `q2` do not match at all in the numeric data frame
# due to `q2` reflecting codes, not ages
# note that in the output, the 'lbl' of the '<dbl+lbl>' reflects age
data_spss |> 
  select(lucid_age, q2) |> 
  mutate("match" = ifelse(as.numeric(lucid_age) == q2, "match", "no match")) 

# if `q2` is simply converted to a numeric vector, than the values would
# incorrectly reflect the response codes rather than age of respondent
# To resolve this problem, I use `dplyr::mutate` and `dplyr::case_match` to
# re-name the variable lable and re-code to reflect age and convert to numeric

data_spss <- data_spss |>
  mutate(
    age = case_match(
      q2,
      1 ~ 18,
      2 ~ 19,
      3 ~ 20,
      4 ~ 21,
      5 ~ 22,
      6 ~ 23,
      7 ~ 24,
      8 ~ 25,
      9 ~ 26,
      10 ~ 27,
      11 ~ 28,
      12 ~ 29,
      13 ~ 30,
      14 ~ 31,
      15 ~ 32,
      16 ~ 33,
      17 ~ 34,
      18 ~ 35,
      19 ~ 36,
      20 ~ 37,
      21 ~ 38,
      22 ~ 39,
      23 ~ 40,
      24 ~ 41,
      25 ~ 42,
      26 ~ 43,
      27 ~ 44,
      28 ~ 45,
      29 ~ 46,
      30 ~ 47,
      31 ~ 48,
      32 ~ 49,
      33 ~ 50,
      34 ~ 51,
      35 ~ 52,
      36 ~ 53,
      37 ~ 54,
      38 ~ 55,
      39 ~ 56,
      40 ~ 57,
      41 ~ 58,
      42 ~ 59,
      43 ~ 60,
      44 ~ 61,
      45 ~ 62,
      46 ~ 63,
      47 ~ 64,
      48 ~ 65,
      49 ~ 66,
      50 ~ 67,
      51 ~ 68,
      52 ~ 69,
      53 ~ 70,
      54 ~ 71,
      55 ~ 72,
      56 ~ 73,
      57 ~ 74,
      58 ~ 75,
      59 ~ 76,
      60 ~ 77,
      61 ~ 78,
      62 ~ 79,
      63 ~ 80,
      64 ~ 81,
      65 ~ 82,
      66 ~ 83,
      67 ~ 84,
      68 ~ 85,
      69 ~ 86,
      70 ~ 87,
      71 ~ 88,
      72 ~ 89,
      73 ~ 92 # one person indicated 90 or older. R's age in Lucid data is 92
    ),
    .after = q2,
    .keep = "unused"
  ) |> 
  labelled::set_variable_labels(
    age = "What is your age?"
  )

# it is still possible that `lucid_age` doesn't match the re-coded `age`
# This is the case, in fact. There are a total of 170 discrepancies, which
# accounts to a proportion of 0.122 or 12.2% of sample
# data_spss |> 
#   select(rowID, lucid_age, age) |> 
#   mutate(match = ifelse(age == lucid_age, TRUE, FALSE)) |>
#   summarise(
#     "Matches" = sum(match == TRUE, na.rm = T),
#     "Non-match" = sum(match == FALSE, na.rm = T)
#   )


# Question items Q51 to Q58 were split up into two question sets, Q51:Q54 and
# Q55:Q58. The order of these question sets were randomly reversed. For order 1,
# questions Q51:Q54 ('agent-aim') were presented first, followed by Q55:Q58
# ('agent-action-aim'). For order 2, the presentation was reversed. Two
# variables (columns) in the data set identify the question display order. Here
# I use those two variables to construct a single dummy variable where order 1 = 1,
# and order 2 = 0

# create var to identify display order of popular efficacy questions
data_spss <- data_spss |>  
  mutate(popeff_qdo = case_when(
    fl_22_do_popular_efficacy_agent_aim == 1 & fl_22_do_popularefficacy_agent_action_aim == 2 ~ 1,
    fl_22_do_popular_efficacy_agent_aim == 2 & fl_22_do_popularefficacy_agent_action_aim == 1 ~ 0),
    .after = qset,
    .keep = "unused")


# create data dictionary/codebook ::::::::::::::::::::::::::::::::::::::::::####
# create data dictionary using `labelled::generate_dictionary()`
data_spss_dict <- data_spss |> 
  # subset spss data set to omit the display order variables
  select(!contains("_do_")
         & !contains("_ado_")
         & !contains("_click")
         & !contains("_count")) |>
  labelled::set_variable_labels(
    popeff_qdo = "Popular efficacy question display order"
  ) |> 
  labelled::generate_dictionary()

# further process data set for later analysis ::::::::::::::::::::::::::::::####

# check to see if there are missing values in the data set (there are)
# any(is.na(data_spss))
# 
# # determine which variables (columns) contain missing values and how many
# colSums(is.na(data_spss)) |> broom::tidy() |> 
#   filter(x > 0)


# since SPSS data is labelled, usually, the variables are <dbl+lbl> instead of
# factors <fct>. It's easier to analyze vars in data as factors, but losing the
# variable labels (i.e., question text) is kind of a bummer. The following puts
# the variable labels form the SPSS data set back into the 'factor-ized' dataset

# covert all <dbl+lbl> vars to factor <fct> vars
# remove all display order variables, as well as click counts
# specifically convert `group` and `qset` to factor vars and set value labels
# with associated values, i.e, levels with labels
# re-code all blank levels "" to NA

data <- data_spss |>
  # subset spss data set to omit the display order variables
  select(!contains("_do_")
         & !contains("_ado_")
         & !contains("_click")
         & !contains("_count")) |>
  # make `group` and `qset` factors
  mutate(group = forcats::fct(group, levels = c("Treatment", "Control")),
         qset = forcats::fct(qset, levels = c("A", "B"), na = "" )) |> 
  sjlabelled::set_labels(group, labels = c("Treatment" = 1, "Control" = 0)) |>
  sjlabelled::set_labels(qset, labels = c("A" = 1, "B" = 0)) |> 
  # convert all labelled variables to factors
  haven::as_factor(only_labelled = T) |>
  # re-code any empty/blank levels to NA
  mutate(across(where(is.factor), ~ fct_recode(., NULL = ""))) |> 
  # reverse order of identified factor levels for consist direction
  mutate(across(c(q8, q20, q23, q24, q31, q34, q35, q49, q50), .fns = ~fct_rev(.))) 



# save html output of codebook :::::::::::::::::::::::::::::::::::::::::::: ####
# This codebook has the ID, name (variable code), variable label (Label), values
# (e.g., 1, 2), value labels (e.g., Male, Female), frequency, and proportion.

# save into output folder as html file
# omit display order, answer display order, and needless timing variables
# data |> 
#   sjPlot::view_df(file = "output/vets_survey_data_dict.html",
#                   show.frq = T, show.prc = T)
# 
# data_spss |>
#   sjPlot::view_df(file = "output/spss_survey_dict.html",
#                   show.frq = T, show.prc = T)
# 


# add variable labels back into factorized data set ::::::::::::::::::::::::####

# variable labels are nice to have for plenty of reasons

# To quickly assign the variable labels, first create a named vector via
# deframe() with values as the variable labels and names as the variable names.
data_labels <- data_spss_dict |>
  select(variable, label) |> 
  deframe()

# Now assign the labels using the splice operator. Using the splice operator,
# labels are assigned via matching against the variable name, which means that
# variable order does not matter.

data <- data |> 
  labelled::set_variable_labels(!!!data_labels)

# create party id variable and age group variable ::::::::::::::::::::::::::####

# construct the partyid var with 3 categories, true independents
data <- data |>
  mutate(
    partyid_3cat = pewmethods::fct_case_when(
      partyid == "Republican" ~ "Republican",
      partystr_rep == "Strong" ~ "Republican",
      partystr_rep == "Not very strong" ~ "Republican",
      partylean == "Republican" ~ "Republican",
      partyid == "Democrat" ~ "Democrat",
      partystr_dem == "Strong" ~ "Democrat",
      partystr_dem == "Not very strong" ~ "Democrat",
      partylean == "Democratic" ~ "Democrat",
      partyid == "Independent" ~ "Independent",
      partyid == "Other" ~ "Independent",
      partylean == "Neither" ~ "Independent"
    ),
    .before = partyid) |> 
  labelled::set_variable_labels(
    partyid_3cat = "Party ID 3 categories, with true Independents")


# create an age group variable
data <- data |>    
  mutate(age_cat = 
           forcats::fct_collapse(
             as_factor(age),
             "18-24" = c(18:24),
             "25-34" = c(25:34),
             "35-44" = c(35:44),
             "45-54" = c(45:54),
             "55-64" = c(55:64),
             "65-74" = c(65:74),
             "75-84" = c(75:82, 84),
             "85-92" = c(85:88, 92)
             ),
         .after = age) |> 
  labelled::set_variable_labels(
    age_cat = "Age categorized into eight groups"
  )


# save SPSS data set, slightly cleaned :::::::::::::::::::::::::::::::::::::####

haven::write_dta(data_spss, 
                 path = "data/vets-survey-exp_2024-09-11.dta",
                 version = 13)

# save as .Rdata file. faster, easier, lightweight. Or something like that
save(data_spss, file = "data/vets-survey-exp-2024-09-11_spss.Rdata")
write.csv(data_spss, file = "data/vets-survey-exp-2024-09-11_spss.csv")

# save data_spss_dict as .csv
# it won't save 'as-is', so use `surveytoolbox::data_dict`
data_dict_spss <- surveytoolbox::data_dict(data_spss)
write.csv(data_dict_spss, file = "codebooks/vets_survey_exp_data_dict_spss_2024-09-11.csv")


# Create numeric only dataframe ::::::::::::::::::::::::::::::::::::::::::::####

# call the numeric.R script, sourced from the 'scripts/' directory
source(here::here('scripts', '02-data-numeric.R'))



data2_dict_num <- surveytoolbox::data_dict(data2)
write.csv(data2_dict_num, file = "codebooks/data2_dict_num.csv")


# Dummify factors ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# call the dummify script sourced from the 'scripts/' directory
source(here::here('scripts', '03-data-dummify.R'))


# save dataframe and data dictionary :::::::::::::::::::::::::::::::::::::::####

# save processed data set. (this one is prior to adding any dummy vars)
save(data, file = "data/data_vets_survey-exp-2024-09-11.Rdata")
write.csv(data, file = "data/data_vets_survey-exp-2024-09-11.csv")

# save df processed data set.
save(df, file = "data/df-2024-09-11.Rdata")
write.csv(df, file = "data/df-2024-09-11.csv")

# save the numeric only processed data set.
save(data2, file = "data/data_numeric_vets_survey-exp-2024-09-11.Rdata")
write.csv(data2, file = "data/data_numeric_vets_survey-exp-2024-09-11.csv")


# save a data dict that can be saved as .csv
# generate data dictionary
df_dict <- surveytoolbox::data_dict(df) # I prefer this to the labelled dict
write.csv(df_dict, file = "codebooks/df_dict.csv")



# remove uneeded dataframes and leave df and df_dict
rm(data, data_dict_spss, data_spss, data_spss_dict, data2_dict_num, raw_spss, raw_spss_dict)
