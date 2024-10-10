# plotting

# Bar plots ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# summary of factor var, easy
summary(df$q19)

# easy way to make barplot of factor var
barplot(summary(df$q19),
        ylab = "Freq",
        xlab = "Q19. How confident are you that votes in Maricopa County, AZ will be counted as voters intend in the elections this November?")

# another way, identical result
barplot(table(df$q19, useNA = "no"),
        ylab = "Freq",
        xlab = "Q19. How confident are you that votes in Maricopa County, AZ will be counted as voters intend in the elections this November?")

# can also use barplot for proportions
barplot(prop.table(table(df$q19, useNA = "no")),
        ylab = "Freq",
        xlab = "Q19. How confident are you that votes in Maricopa County, AZ will be counted as voters intend in the elections this November?",
        ylim = c(0,1) # probably a good idea to set range from 0 to 1 for proportion
)

# barplot of counts (frequency) using ggplot2
df |> 
  ggplot(aes(x = q19))+
  geom_bar()+
  labs(y = "Freq",
       x = "Q19. How confident are you that votes in Maricopa County, AZ will be counted as voters intend in the elections this November?")

# barplot of proportions using ggplot2
df |> 
  ggplot(aes(x = q19, y = ..count../sum(..count..)))+
  geom_bar()+
  labs(y = "Prop",
       x = "Q19. How confident are you that votes in Maricopa County, AZ will be counted as voters intend in the elections this November?")

# We can also use barplot() to illustrate the relationship between two
# categorical variables.
# If beside = F, then result will output a stacked bar chart
barplot(table(df$group, df$q19),
        beside = T,
        legend.text = T,
        xlab = "Q19. How confident are you that votes in Maricopa County, AZ will be counted as voters intend in the elections this November?",
        ylab = 'Freq',
        main = 'Q19 by Group')

# Proportion chart
# recall: when margin =1 for prop.table, then prop.table(table(iv, dv), margin = 1)
barplot(prop.table(table(df$group, df$q19), margin = 1),
        beside = T,
        legend.text = T,
        xlab = "Q19. How confident are you that votes in Maricopa County, AZ will be counted as voters intend in the elections this November?",
        ylab = 'Proportion',
        main = 'Q19 by Group',
        ylim = c(0, 1))

# Proportion chart, where bars sum to 100% within clusters (i.e., within groups)
# recall: when margin =2 for prop.table, then prop.table(table(dv, iv), margin = 2)
# recall: when margin =1 for prop.table, then prop.table(table(iv, dv), margin = 1)
barplot(prop.table(table(df$q19, df$group), margin = 2),
        beside = T,
        legend.text = T,
        xlab = "Q19. How confident are you that votes in Maricopa County, AZ will be counted as voters intend in the elections this November?",
        ylab = 'Proportion',
        main = 'Q19 by Group',
        ylim = c(0, 0.5))

# Base R barplot() requires, as input, a table to compute the counts or
# proportions. In ggplot(), however, you can input the variables directly.
# However, as shown below, to get a proportion chart, we still need to compute
# the proportions first.

# NOTE: "fill" tells ggplot which variable is filled with different colors
df |>  
  # By default, geom_bar() will plot a bar for NA values
  # Filter them out to prevent this
  # filter(!is.na(y) & !is.na(x))  |> 
  
  ggplot(aes(x = q19, fill = group)) +
  
  # Without position = "dodge", output will be a stacked bar chart
  geom_bar(position = "dodge") +
  
  # \n in a string tells R to break the line there
  labs(y = "Frequency",
       x = "Q19",
       fill = "Group\nExperiment\nCondition")


df |> 
  select(group, q19) |> 
  # compute frequency within each combination
  group_by(group, q19) |> 
  count() |> 
  # compute proportion within group
  # n is the default variable created by count()
  group_by(group) |>
  mutate(Proportion = round(n/sum(n), 3)) |> 
  
  # add y to inform geom_bar what to put in y-axis
  ggplot(aes(x = q19, y = Proportion, fill = group))+
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_text(aes(label = Proportion), position = position_dodge(1.0), vjust = -0.5)+
  labs(y = "Prop",
       x = "Q19",
       fill = "Group\nExperiment\nCondition")


# Q19 by experiment condition, barplot
df |> 
  # compute frequency within each combination
  group_by(group, q19) |> 
  count() |>  
  # compute proportion within group
  # n is the default variable created by count()
  group_by(group) |> 
  mutate(prop = round(n/sum(n), digits = 3),
         pct = prop*100,
         res = str_c(pct,'% (', n, ')', sep = "")) |> 
  ggplot(aes(x = q19, y = pct, fill = group))+
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_text(aes(label = res), position = position_dodge(1.0), size = 2.5, vjust = -0.5)+
  # change color to black and grey
  scale_fill_grey(start = 0.5, end = 0.1)+
  # \n in a string tells R to break the line there
  labs(y = "Percentage",
       x = "Q19. How confident are you that votes in Maricopa County, AZ \nwill be counted as voters intend in the elections this November?",
       fill = "Group\nExperiment\nCondition",
       caption = 'Treatment n = 639, Control n = 624')+
  theme_bw()


### ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####
# trying to plot likert scales

# attempting to use the ggstats::gglikert()
# ggstats::gglikert works beautifully

df |> 
  mutate(across(c(q28_1:q28_5), ~ forcats::fct_rev(.))) |> 
  labelled::set_variable_labels(
    q28_1 = "There will be voter fraud",
    q28_2 = "Many votes won't be counted",
    q28_3 = "Many people will be turned away",
    q28_4 = "Foreign country will tamper with votes",
    q28_5 = "Election officials will discourage voter turnout"
  ) |> 
  ggstats::gglikert(include = q28_1:q28_5, sort = 'ascending',facet_cols = vars(group))

df |> 
  mutate(across(c(q28_1:q28_5), ~ forcats::fct_rev(.))) |> 
  labelled::set_variable_labels(
    q28_1 = "Voter fraud",
    q28_2 = "Vote Miscount",
    q28_3 = "Voters Rejected",
    q28_4 = "Foreign Interference",
    q28_5 = "Turnout Discouraged"
  ) |> 
  ggstats::gglikert(include = q28_1:q28_5, y = 'group', facet_rows = vars(.question))


# I can likert plot vars that are measured using the same levels

# confidence
# gotta figure out what to do with those var labels
df |>
  labelled::set_variable_labels(
    q19 = "Votes will be counted as voters intend",
    q20 = "Election staff in Maricopa County, AZ will do a good job",
    q22 = "Voting process will be fair in Maricopa County, AZ",
    q23 = "Voting outcomes will be fair in Maricopa county, AZ",
    q24 = "Election systems will be secure from technological threats",
    q26 = "Voting sites will be safe places for in-person voting"
  ) |> 
  ggstats::gglikert(include = c(q19,q20,q22,q23,q24,q26), facet_cols = vars(group))+ 
  labs(
    title = "Trust and Confidence Electoral Process by Experiment Condition"
  )+
  theme_bw()+
  theme(legend.position = 'bottom')

df |>
  labelled::set_variable_labels(
    q19 = "Votes will be counted as voters intend",
    q20 = "Election staff in Maricopa County, AZ will do a good job",
    q22 = "Voting process will be fair in Maricopa County, AZ",
    q23 = "Voting outcomes will be fair in Maricopa county, AZ",
    q24 = "Election systems will be secure from technological threats",
    q26 = "Voting sites will be safe places for in-person voting"
  ) |> 
  ggstats::gglikert(include = c(q20), y = 'group', facet_rows = vars(.question))+ 
  labs(
    title = "Election staff in Maricopa County, AZ will do a good job by Experiment Condition"
  )+
  theme_bw()+
  theme(legend.position = 'bottom')



df |>
  labelled::set_variable_labels(
    q19 = "Q19. Accurate Vote counts",
    q22 = "Q22. Fair process",
    q23 = "Q23. Fair outcomes",
    q24 = "Q24. Secure Election Tech",
    q26 = "Q26. Safe In-person Voting"
  ) |> 
  ggstats::gglikert(include = c(q19,q22,q23,q24,q26), y = 'group')+
  ggplot2::facet_grid(rows = vars(.question), labeller = label_wrap_gen(15))+
  labs(
    title = "Confidence in Election Administration by Experiment Condition"
  )+
  theme_bw()+
  theme(legend.position = 'bottom')



# this is for the same questions but as concern the local community
ggstats::gglikert(data = df, include = c(q30, q31, q33, q34, q35, q37), y = 'group', facet_rows = vars(.question))


# committed
# no big whoop here
df |> 
  labelled::set_variable_labels(
    q21 = "Election staff in AZ will be committed to fairness and accuracy",
    q32 = "Election staff in local area will be committed to fairness and accuracy"
  ) |> 
  ggstats::gglikert(include = c(q21, q32), y = 'group', facet_rows = vars(.question))


# concern for violence, threats, intimidation
df |> 
  mutate(q25 = forcats::fct_rev(q25)) |> 
  labelled::set_variable_labels(
    q25 = 'Concern for voter intimidation, threats, or violence'
  ) |> 
  ggstats::gglikert(include = q25,
                    y = 'group',
                    facet_rows = vars(.question))+
  labs(
    title = "Q25. Concern for voter intimidation, threats, or violence")+
  theme_bw()+
  theme(legend.position = 'bottom')

# q26
df |> 
  ggstats::gglikert(include = q26, 
                    variable_labels = c(q26 ="Confidence in Voter Safety"),
                    y = 'group',
                    facet_rows = vars(.question))+
  labs(
    title = "Confidence for In-person voter safety in Maricopa County, AZ by Experiment Condition",
    subtitle = "Q26. How confident, if at all, are you that in person polling places in Maricopa County, AZ \nwill be safe places for voters to cast their ballots during the upcoming elections in November?",
       caption = 'Treatment n = 638, Control n = 624')+
  theme_bw()+
  theme(legend.position = 'bottom')


# Q27
df |> 
  ggstats::gglikert(include = q27, 
                    variable_labels = c(q26 ="Election Official Approval"),
                    y = 'group',
                    facet_rows = vars(.question))+
  labs(
    title = "Approval of Election Officials in Maricopa County, AZ by Experiment Condition",
    subtitle = "Q27. Do you approve or disapprove of the way \nelection officials in Maricopa County, AZ are handling their jobs?",
       caption = 'Treatment n = 636, Control n = 623')+
  theme_bw()+
  theme(legend.position = 'bottom')


# q41 and q43
# Likert plot to demonstrate difference between question sets with different
# wording
df |>
  mutate(
    qset = forcats::fct_recode(
      qset,
      "Election staff 'includes'..." = 'A',
      "Election staff is 'Majority' of... " = 'B'
    )
  ) |>
  sjlabelled::var_labels(
    q41.4 = "Veterans",
    q41.5 = "Lawyers",
    q41.6 = "College Students",
    q43.4 = "Veterans",
    q43.5 = "Lawyers",
    q43.6 = "College Students"
  ) |>
  ggstats::gglikert(
    include = c(q41.4, q41.5, q41.6, q43.4, q43.5, q43.6),
    y = 'qset',
    facet_rows = vars(.question)
  ) +
  labs(title = "Differences in Proportion Between Question Wording") +
  theme_bw() +
  theme(legend.position = 'bottom')

# Q41.4 (vets), Q41.5 (lawyers), Q41.6 (students), by treatment
df |> 
  ggstats::gglikert(include = c(q41.4:q41.6), y = 'group',  symmetric = T, add_totals = F)+ 
  facet_grid(rows = vars(.question), labeller = label_wrap_gen(15))+
  labs(
    title = "Impact on Confidence in Election Fairness and Accuracy by Experiment Condition",
    subtitle = "Q41. How would the following impact your confidence in the fairness and accuracy of elections conducted this November?"
  )+
  theme_bw()+
  theme(legend.position = 'bottom')

# save plot to images folder in project dir
ggplot2::ggsave(filename = 'q41_by_treatment_likert_plot.png', path = '~/R/GA_work/CDCE/cdce_vets/images')


# Q43.4 (vets), Q43.5 (lawyers), Q43.6 (students) by treatment 
df |> 
  ggstats::gglikert(include = c(q43.4:q43.6), y = 'group', symmetric = T, add_totals = F)+ 
  facet_grid(rows = vars(.question), labeller = label_wrap_gen(15))+
  labs(
    title = "Impact on Confidence in Voter Safety at Polling Sites by Experiment Condition",
    subtitle = "Q43. How would the following impact your confidence that voters are safe from violence, threats of violence, or intimidation while voting in-person during elections this November?"
  )+
  theme_bw()+
  theme(legend.position = 'bottom')

# save plot to images folder in project dir
ggplot2::ggsave(filename = 'q43_by_treatment_likert_plot.png', path = '~/R/GA_work/CDCE/cdce_vets/images')


# combine multiple coef plots into one plot ::::::::::::::::::::::::::::::::####
# show coef plot when q41.4 is converted to a dummy variable where 1 =
# 'Increase' and 0 = "No increase"

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

mod.q41.1 <- lm(formula = q41.1.clps.dum ~ group, data = df)
mod.q41.2 <- lm(formula = q41.2.clps.dum ~ group, data = df)
mod.q41.3 <- lm(formula = q41.3.clps.dum ~ group, data = df)
mod.q41.4 <- lm(formula = q41.4.clps.dum ~ group, data = df)
mod.q41.5 <- lm(formula = q41.5.clps.dum ~ group, data = df)
mod.q41.6 <- lm(formula = q41.6.clps.dum ~ group, data = df)

# place all the models in a list assigned as 'models' and include them in the
# ggstats::ggcoef_compare() function
models.q41 <- list(
  "Test Every Machine" = mod.q41.1,
  "Audit Ballots" = mod.q41.2,
  "Partisan Poll Watchers" = mod.q41.3,
  "Election Staff Includes Veterans" = mod.q41.4,
  "Election Staff Includes Lawyers" = mod.q41.5,
  "Election Staff Includes Students" = mod.q41.6
)

# print the coef plot and selectively color coefficients
ggstats::ggcoef_compare(
  models.q41, 
  conf.int = T, 
  add_reference_rows = F,
  categorical_terms_pattern = "{level} (ref: {reference_level})")+
  ggplot2::scale_color_manual(
    values = c("black", "black", "black", "red", "black", "black"))+
  labs(
    title = "Impact on Confidence in Election Fairness when Election Staff \nIncludes Veterans and Family by Experiment Condition",
    subtitle = "Means comparison with 95% confidence intervals\n'Increase in confidence' = 1, 'No increase in confidence = 0'",
    caption = "estimate = 0.11635, SE = 0.02799, t-statistic = 4.157, CI [-0.171, -0.0614]"
  )+
  theme_bw()+
  theme(legend.position = 'bottom')


# save plot to images folder in project dir
ggplot2::ggsave(filename = 'q41_by_treatment_coef_plot.png', path = '~/R/GA_work/CDCE/cdce_vets/images')


# Now just the data from the list of models
q41_results <- ggstats::ggcoef_compare(
  models.q41, 
  add_reference_rows = F, 
  categorical_terms_pattern = "{level} (ref: {reference_level})",
  return_data = T) |>
  select(item = model,
         group = label,
         n = n_obs,
         beta_estimate = estimate,
         std.error,
         statistic,
         p.value,
         signif_stars,
         conf.low, conf.high) |> 
  mutate(item = dplyr::case_when(
    item == "Test Every Machine" ~ "Election Officials Test Every Machine",
    item == "Audit Ballots" ~ "Election Officials Conduct Audits of Ballots",
    item == "Partisan Poll Watchers" ~ "Partisan Poll Watchers Observe Election",
    .default = item
  ))

## compare models for q43 ::::::::::::::::::::::::::::::::::::::::::::::::::####
# show coef plot when Q43 are converted to a dummy variable where 1 =
# 'Increase' and 0 = "No increase"

# I could easily use purrr::map() to iterate this part, but I don't wanna right
# now...
mod.q43.1 <- lm(formula = q43.1.clps.dum ~ group, data = df)
mod.q43.2 <- lm(formula = q43.2.clps.dum ~ group, data = df)
mod.q43.3 <- lm(formula = q43.3.clps.dum ~ group, data = df)
mod.q43.4 <- lm(formula = q43.4.clps.dum ~ group, data = df)
mod.q43.5 <- lm(formula = q43.5.clps.dum ~ group, data = df)
mod.q43.6 <- lm(formula = q43.6.clps.dum ~ group, data = df)

# place all the models in a list assigned as 'models' and include them in the
# ggstats::ggcoef_compare() function
models.q43 <- list(
  "Test Every Machine" = mod.q43.1,
  "Audit Ballots" = mod.q43.2,
  "Partisan Poll Watchers" = mod.q43.3,
  "Election Staff Includes Veterans" = mod.q43.4,
  "Election Staff Includes Lawyers" = mod.q43.5,
  "Election Staff Includes Students" = mod.q43.6
)

# print the coef plot and selectively color coefficients
ggstats::ggcoef_compare(
  models.q43, 
  conf.int = T, 
  add_reference_rows = F, 
  categorical_terms_pattern = "{level} (ref: {reference_level})")+
  ggplot2::scale_color_manual(
    values = c("black", "black", "black", "red", "black", "black"))+
  labs(
    title = "Impact on Confidence in Voter Safety when Election Staff \nIncludes Veterans and Family by Experiment Condition",
    subtitle = "Means comparison with 95% confidence intervals\n'Increase in confidence' = 1, 'No increase in confidence = 0'",
    caption = "estimate = 0.0841, SE = 0.0280, t-statistic = 3.00, CI [0.0291, 0.139]"
  )+
  theme_bw()+
  theme(legend.position = 'bottom')


# save plot to images folder in project dir
ggplot2::ggsave(filename = 'q43_by_treatment_coef_plot.png', path = '~/R/GA_work/CDCE/cdce_vets/images')


# Now just the data from the list of models
q43_results <- ggstats::ggcoef_compare(
  models.q43, 
  add_reference_rows = F, 
  categorical_terms_pattern = "{level} (ref: {reference_level})", 
  return_data = T) |>
  select(item = model,
         group = label,
         n = n_obs,
         beta_estimate = estimate,
         std.error,
         statistic,
         p.value,
         signif_stars,
         conf.low, conf.high) |> 
  mutate(item = dplyr::case_when(
    item == "Test Every Machine" ~ "Election Officials Test Every Machine",
    item == "Audit Ballots" ~ "Election Officials Conduct Audits of Ballots",
    item == "Partisan Poll Watchers" ~ "Partisan Poll Watchers Observe Election",
    .default = item
  ))

q41_results[4,]
q43_results[4,]


################################################################################


# barplot of q41.4 by group
df |> 
  select(group, q41.4, q41.5, q41.6) |> 
  drop_na() |> 
  # compute frequency within each combination
  group_by(group, q41.4) |> 
  count() |> 
  # compute proportion within group
  # n is the default variable created by count()
  group_by(group) |> 
  mutate(prop = round(n/sum(n), digits = 3),
         pct = prop*100,
         res = str_c(pct,'% (', n, ')', sep = "")) |> 
  # add y to inform geom_bar what to put in y-axis
  ggplot(aes(x = q41.4, y = prop, fill = group))+
  geom_col(position = 'dodge')+
  geom_text(aes(label = res), position = position_dodge(1.0), size = 2.5 , vjust = -0.5)+
  scale_fill_grey(start = 0.5, end = 0.1)+
  # coord_flip()+
  labs(y = "Prop",
       x = "Q41.4",
       fill = "Group\nExperiment\nCondition")

df |> 
  select(group, q41.4) |> 
  drop_na() |> 
  ggstats::gglikert(include = c(group), y = 'q41.4',
                    facet_row = vars(.question),
                    symmetric = T,
                    add_totals = T)+
  # change color to black and grey
  scale_fill_grey(start = 0.7, end = 0.1)+
  labs(
    title = "How would the following impact your confidence in the fairness and accuracy of elections conducted this November?",
    subtitle = "Election staff and volunteers include military veterans and their family members from the community.",
    caption = "Treatment = 638, Control = 623"
  )+
  theme_bw()+
  theme(legend.position = 'bottom', strip.text = element_blank())


df |> 
  select(group, q41.4.clps) |> 
  drop_na() |> 
  mutate(q41.4.clps = dplyr::case_when(
    q41.4.clps == "decrease" ~ "Decrease Confidence",
    q41.4.clps == "no_impact" ~ "No Impact",
    q41.4.clps == "increase" ~ "Increase Confidence",
    TRUE ~ NA
  )) |> 
  ggstats::gglikert(include = c(group), y = 'q41.4.clps', facet_row = vars(.question),  symmetric = T, add_totals = T)+
  # change color to black and grey
  scale_fill_grey(start = 0.7, end = 0.1)+
  labs(
    title = "How would the following impact your confidence in the fairness and accuracy of elections conducted this November?",
    subtitle = "Election staff and volunteers include military veterans and their family members from the community.",
    caption = "Treatment = 638, Control = 623"
  )+
  theme_bw()+
  theme(legend.position = 'bottom', strip.text = element_blank())


  

# Q41.4 (vets) by treatment
df |> 
  select(group, q41.4) |> 
  drop_na() |> 
  ggstats::gglikert(include = c(group), y = 'q41.4', facet_row = vars(.question),  symmetric = T, add_totals = T)+
  # change color to black and grey
  scale_fill_grey(start = 0.7, end = 0.1)+
  labs(
    title = "How would the following impact your confidence in the fairness and accuracy of elections conducted this November?",
    subtitle = "Election staff and volunteers include military veterans and their family members from the community."
  )+
  theme_bw()+
  theme(legend.position = 'bottom', strip.text = element_blank())

# vets
df |> 
  select(group, q41.4.clps) |> 
  drop_na() |> 
  mutate(q41.4.clps = dplyr::case_when(
    q41.4.clps == "decrease" ~ "Decrease Confidence",
    q41.4.clps == "no_impact" ~ "No Impact",
    q41.4.clps == "increase" ~ "Increase Confidence",
    TRUE ~ NA
  )) |> 
  ggstats::gglikert(include = c(group), y = 'q41.4.clps', facet_row = vars(.question),  symmetric = T, add_totals = T)+
  # change color to black and grey
  scale_fill_grey(start = 0.7, end = 0.1)+
  labs(
    title = "How would the following impact your confidence in the fairness and accuracy of elections conducted this November?",
    subtitle = "Election staff and volunteers include military veterans and their family members from the community.",
    caption = "Treatment = 638, Control = 623"
  )+
  theme_bw()+
  theme(legend.position = 'bottom', strip.text = element_blank())

# lawyers
df |> 
  select(group, q41.5.clps) |> 
  drop_na() |> 
  mutate(q41.5.clps = dplyr::case_when(
    q41.5.clps == "decrease" ~ "Decrease Confidence",
    q41.5.clps == "no_impact" ~ "No Impact",
    q41.5.clps == "increase" ~ "Increase Confidence",
    TRUE ~ NA
  )) |> 
  ggstats::gglikert(include = c(group), y = 'q41.5.clps', facet_row = vars(.question),  symmetric = T, add_totals = T)+
  # change color to black and grey
  scale_fill_grey(start = 0.7, end = 0.1)+
  labs(
    title = "How would the following impact your confidence in the fairness and accuracy of elections conducted this November?",
    subtitle = "Election staff and volunteers include lawyers from the community.",
    caption = "Treatment = 638, Control = 623"
  )+
  theme_bw()+
  theme(legend.position = 'bottom', strip.text = element_blank())

# students
df |> 
  select(group, q41.6.clps) |> 
  drop_na() |> 
  mutate(q41.6.clps = dplyr::case_when(
    q41.6.clps == "decrease" ~ "Decrease Confidence",
    q41.6.clps == "no_impact" ~ "No Impact",
    q41.6.clps == "increase" ~ "Increase Confidence",
    TRUE ~ NA
  )) |> 
  ggstats::gglikert(include = c(group), y = 'q41.6.clps', facet_row = vars(.question),  symmetric = T, add_totals = T)+
  # change color to black and grey
  scale_fill_grey(start = 0.7, end = 0.1)+
  labs(
    title = "How would the following impact your confidence in the fairness and accuracy of elections conducted this November?",
    subtitle = "Election staff and volunteers include college students from the community.",
    caption = "Treatment = 638, Control = 623"
  )+
  theme_bw()+
  theme(legend.position = 'bottom', strip.text = element_blank())
