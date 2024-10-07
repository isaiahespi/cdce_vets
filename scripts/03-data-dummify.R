# Dummify all these factors

library(tidyverse)
library(pewmethods)
library(surveytoolbox)



# collapse categorical variables with 4-point response items into factors with
# two levels. Those with 5-point response options are converted into factors
# with 3 levels. Affix ".clps" to colname to identify collapsed factor vars,
# e.g., q19.clps

df <- data |>
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
  dplyr::mutate(q38.clps =
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
  dplyr::mutate(q29.clps = forcats::fct_collapse(
    q29,
    oppose = c("Definitely should not adopt", "Probably should not adopt"),
    support = c("Probably should adopt", "Definitely should adopt")
  )) |> 
  dplyr::mutate(across(q28_1:q28_5 | q40_1:q40_5,~forcats::fct_collapse(
    .,
    Not_likely = c("Not likely at all", "Not too likely"),
    Likely = c("Somewhat likely", "Very likely")),
    .names = "{.col}.clps"
    )) |>
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
  dplyr::mutate(gender_3cat = forcats::fct_collapse(
    gender,
    "Male" = "Male",
    "Female" = "Female",
    "Other/Refused" = c("Non-binary / third gender", "Prefer not to say")),
    voted2020.clps = forcats::fct_collapse(
      voted2020,
      "Voted" = "Yes, I'm sure I voted",
      "Didn't vote" = c("I'm sure I didn't vote", "I don't think I voted"),
      "Unsure/Ineligible" = c("I think I voted","I was not eligible to vote")
      )) |>
  labelled::set_variable_labels(
    voted2020.clps = "Turnout 2020"
  )

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# set shorter levels for some demographic vars
df <- df |> labelled::set_variable_labels(
  gender_3cat = "What is your current gender?",
  race = "Primary race reported by respondent",
  educ = "Highest level of education completed",
  milserv1 = "Have you ever served in the Armed Forces?",
  milserv2 = "Are you now serving in the Armed Forces?",
  milservfam = "Member of immediate family served or is currently serving",
  choice2020 = "Who did you vote for?",
  voteintent = "Do you plan to vote?"
)

df <- df |>
  labelled::set_variable_labels(
    q5         = "Attention to Political Affairs",
    q6         = "EO Favorability",
    q7         = "Legitimacy of 2020 election",
    q8         = "General Trust in others",
    q19.clps   = "Confidence vote counts will be accurate, AZ",
    q20.clps   = "Competence of Election Staff, AZ",
    q21.clps   = "Commitment of Election Staff, AZ",
    q22.clps   = "Fair Process, AZ",
    q23.clps   = "Fair Outcomes, AZ",
    q24.clps   = "Security of Voting Tech, AZ",
    q25.clps   = "Voters Will Be Free From Intimidation/Violence, AZ",
    q26.clps   = "Safe In-person Voting, AZ",
    q27.clps   = "Election Official Approval, AZ",
    q28_1.clps = "Voter fraud, AZ",
    q28_2.clps = "Votes won't be counted, AZ",
    q28_3.clps = "People will turned away, AZ",
    q28_4.clps = "Foreign interference with votes, AZ",
    q28_5.clps = "EOs discourage people from voting, AZ",
    q29.clps   = "Adopt AZ Program",
    q30.clps   = "Accurate counts, Local",
    q31.clps   = "Competence of Election Staff, Local" ,
    q32.clps   = "Commitment of Election Staff, Local",
    q33.clps   = "Fair Process, Local",
    q34.clps   = "Fair Outcomes, Local",
    q35.clps   = "Secure Voting Tech, Local",
    q36.clps   = "Voter Intimidation/Violence, Local",
    q37.clps   = "Safe in-person voting, Local",
    q38.clps   = "Safe to vote in-person, Local",
    q39.clps   = "EO Approval, Local",
    q40_1.clps = "Voter fraud, Local",
    q40_2.clps = "Votes won't be counted, Local",
    q40_3.clps = "People will be turned away, Local",
    q40_4.clps = "Foreign interference with votes, Local",
    q40_5.clps = "EO discourage people from voting, Local",
    
    q41_1.clps = "Election officials test machines",
    q41_2.clps = "Election officials conduct audits",
    q41_3.clps = "Partisan Poll watchers observe the election.",
    q41_4.clps = "Election staff include veterans and family",
    q41_5.clps = "Election staff include lawyers",
    q41_6.clps = "Election staff include college students",
    
    q43_1.clps = "Law enforcement presence.",
    q43_2.clps = "Partisan Poll watchers observe the election",
    q43_3.clps = "People holding signs or giving out literature",
    q43_4.clps = "Election staff includes veterans",
    q43_5.clps = "Election staff includes lawyers",
    q43_6.clps = "Election staff includes students",
    
    q44_1.clps = "Election officials test machines",
    q44_2.clps = "Majority of Election staff are veterans",
    q44_3.clps = "Majority of Election staff are lawyers",
    q44_4.clps = "Election officials conduct audits",
    q44_5.clps = "Partisan Poll watchers observe the election",
    q44_6.clps = "Majority of Election staff are students",
    
    q46_1.clps = "Law enforcement presence.",
    q46_2.clps = "Majority of Election staff are veterans",
    q46_3.clps = "Majority of Election staff are lawyers",
    q46_4.clps = "Partisan Poll watchers observe the election",
    q46_5.clps = "Majority of Election staff are students",
    q46_6.clps = "People holding signs or giving out literature",
  )


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####
# Now do some heavy duty relocating of columns in the dataframe
df <- df |>  
  dplyr::relocate(
    gender_3cat, .after = gender
  ) |> 
  dplyr::relocate(
    voted2020.clps, .after = voted2020
  ) |>
  dplyr::relocate(
  c(
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
    q29.clps,
    q30.clps,
    q31.clps,
    q32.clps,
    q33.clps,
    q34.clps,
    q35.clps,
    q36.clps,
    q37.clps,
    q38.clps,
    q39.clps,
    q40_1.clps,
    q40_2.clps,
    q40_3.clps,
    q40_4.clps,
    q40_5.clps,
    q41_1.clps,
    q41_2.clps,
    q41_3.clps,
    q41_4.clps,
    q41_5.clps,
    q41_6.clps,
    q43_1.clps,
    q43_2.clps,
    q43_3.clps,
    q43_4.clps,
    q43_5.clps,
    q43_6.clps,
    q44_1.clps,
    q44_2.clps,
    q44_3.clps,
    q44_4.clps,
    q44_5.clps,
    q44_6.clps,
    q46_1.clps,
    q46_2.clps,
    q46_3.clps,
    q46_4.clps,
    q46_5.clps,
    q46_6.clps
  ),
  .after = q19.clps)

# merge question sets that show no significant difference ::::::::::::::::::####

# merge (or coalesce) the qset vars that can be merged this merges some vars
# from the q44 and q46 set into q41 and q43 there was no sig diff found for all
# but the qset B questions regarding lawyers and students. However these are
# being coalesced into single question vars anyway since I am able to to still
# distinguish responses by the grouping var `qset`
df <- df |> 
  group_by(rowID) |> 
  mutate(q41.1 = dplyr::coalesce(q41_1, q44_1),
         q41.2 = dplyr::coalesce(q41_2, q44_4),
         q41.3 = dplyr::coalesce(q41_3, q44_5),
         q41.4 = dplyr::coalesce(q41_4, q44_2),
         q41.5 = dplyr::coalesce(q41_5, q44_3),
         q41.6 = dplyr::coalesce(q41_6, q44_6),
         
         q43.1 = dplyr::coalesce(q43_1, q46_1),
         q43.2 = dplyr::coalesce(q43_2, q46_4),
         q43.3 = dplyr::coalesce(q43_3, q46_6),
         q43.4 = dplyr::coalesce(q43_4, q46_2),
         q43.5 = dplyr::coalesce(q43_5, q46_3),
         q43.6 = dplyr::coalesce(q43_6, q46_5),
         
         q41.1.clps = dplyr::coalesce(q41_1.clps, q44_1.clps),
         q41.2.clps = dplyr::coalesce(q41_2.clps, q44_4.clps),
         q41.3.clps = dplyr::coalesce(q41_3.clps, q44_5.clps),
         q41.4.clps = dplyr::coalesce(q41_4.clps, q44_2.clps),
         q41.5.clps = dplyr::coalesce(q41_5.clps, q44_3.clps),
         q41.6.clps = dplyr::coalesce(q41_6.clps, q44_6.clps),
         
         q43.1.clps = dplyr::coalesce(q43_1.clps, q46_1.clps),
         q43.2.clps = dplyr::coalesce(q43_2.clps, q46_4.clps),
         q43.3.clps = dplyr::coalesce(q43_3.clps, q46_6.clps),
         q43.4.clps = dplyr::coalesce(q43_4.clps, q46_2.clps),
         q43.5.clps = dplyr::coalesce(q43_5.clps, q46_3.clps),
         q43.6.clps = dplyr::coalesce(q43_6.clps, q46_5.clps)
         ) |>
  ungroup() |> 
  
  sjlabelled::var_labels(
    q41.1 = "Election officials test machines",
    q41.2 = "Election officials conduct audits",
    q41.3 = "Partisan Poll watchers observe the election.",
    q41.4 = "Election staff includes veterans and family",
    q41.5 = "Election staff includes lawyers",
    q41.6 = "Election staff includes college students",
    
    q43.1 = "Election officials test machines",
    q43.2 = "Election officials conduct audits",
    q43.3 = "Partisan Poll watchers observe the election.",
    q43.4 = "Election staff includes veterans and family",
    q43.5 = "Election staff includes lawyers",
    q43.6 = "Election staff includes college students",
    
    q41.1.clps = "Election officials test machines",
    q41.2.clps = "Election officials conduct audits",
    q41.3.clps = "Partisan Poll watchers observe the election.",
    q41.4.clps = "Election staff includes veterans and family",
    q41.5.clps = "Election staff includes lawyers",
    q41.6.clps = "Election staff includes college students",
    
    q43.1.clps = "Election officials test machines",
    q43.2.clps = "Election officials conduct audits",
    q43.3.clps = "Partisan Poll watchers observe the election.",
    q43.4.clps = "Election staff includes veterans and family",
    q43.5.clps = "Election staff includes lawyers",
    q43.6.clps = "Election staff includes college students"
  )


# create dummy variables where 1 = 'Increase in confidence' and 0 = "No increase
# in confidence".
# add dummy variables to dataframe
df <- df |> 
  mutate(across(c(q41.1.clps:q43.6.clps), 
                ~ dplyr::case_when(
                . == "increase" ~ 1, 
                . == "no_impact" ~ 0,
                . == 'decrease' ~ 0
                ), .names = "{.col}.dum")) 