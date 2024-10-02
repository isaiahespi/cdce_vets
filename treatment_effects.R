# conducting means comparisons between treatment and control


library(tidyverse)
library(RCPA3)
library(rstatix)
library(ggpubr)
library(easystats)
# library(crosstable)
# library(gmodels)
# library(likert)

# load data
load("data/df-2024-09-11.Rdata") # df set as factor with dummy vars
load("data/data_numeric_vets_survey-exp-2024-09-11.Rdata") # data2 numeric only



## 2-sample t-tests for each dummified question by group condition

# conduct t-tests, assign to dum_ttests
dum_ttests <- df |> 
  select(group, contains("_dum")) |>
  # makes df long
  reshape2::melt(id.vars = "group") |> 
  group_by(variable) |> 
  rstatix::t_test(value ~ group, detailed = T) |> 
  rstatix::add_significance()

# relocate p.signif column
dum_ttests <- dum_ttests |> dplyr::relocate(c(p, p.signif), .after = conf.high)

# print results in a 'pipe' markdown format (i.e., not HTML)
kableExtra::kbl(dum_ttests, 
                format = "pipe", 
                digits = 4,
                caption = "Two-sample t-tests results by Experiment Condition",
                booktabs = T) 

# use this to save kable output to file
kableExtra::save_kable(dum_ttests, file = "ttests.txt", self_contained = T)

# show only variables with stat.sig differences between experiment conditions
# print results in kable 
dum_ttests |> 
  select(-5, -df, alternative) |> 
  filter(p.signif != "ns") |> 
  arrange(variable) |> 
  kableExtra::kbl(
    caption = "Two-sample T-test results by Experiment Condition",
    booktabs = T
  ) |> 
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "bordered", "condensed", "responsive"),
    latex_options = "basic",
    font_size = 13,
    full_width = T,
    fixed_thead = T
  )
  
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####
# comparing t-tests and ols.

summary(lm(q19_dum~group, data = df))
rstatix::t_test(data = df,
                q19_dum ~ group,
                paired = F,
                ref.group = "Control",
                detailed = T)

plot(parameters::parameters(lm(q19_dum~group, data = df)))
lm(q22_dum~group, data = df)

plot(parameters::parameters(lm(q22_dum~group, data = df),))
plot(parameters::parameters(lm(q24_dum~group, data = df)))



# convert to numeric and conduct T-test comparison of means
confimpct_df <- df |> 
  mutate(
    q43.4.clps.dum = dplyr::case_when(
      q43.4.clps == "increase" ~ 1,
      q43.4.clps == "no_impact" ~ 0,
      q43.4.clps == 'decrease' ~ 0
      ),
    q41.4.clps.dum = dplyr::case_when(
      q41.4.clps == "increase" ~ 1,
      q41.4.clps == "no_impact" ~ 0,
      q41.4.clps == 'decrease' ~ 0
      ),
    group.dum = dplyr::case_when(
      group == "Treatment" ~ 1,
      group == "Control" ~ 0
    ))

lm(formula = q41.4.clps.dum ~ group, data = confimpct_df) |> summary()

# note that 'estimate1' is simply the (Intercept) from the linear model
# function, which reflects the mean of the control group. 'estimate2' reflects
# the b1 of the linear model, which is simply the difference between the means
# (estimate1 - estimate2)
rstatix::t_test(data = confimpct_df, 
                formula = q41.4.clps.dum ~ group,
                paired = F, 
                conf.level = 0.95, 
                detailed = T) |> 
  rstatix::add_significance()

# this is plotted using the ggstats package, ggcoef_model() function
ggstats::ggcoef_model(
  lm(formula = q41.4.clps.dum ~ group, data = confimpct_df),
  conf.int = T,
  show_p_values = T,
  signif_stars = T,
  categorical_terms_pattern = "{level} (ref: {reference_level})"
)+
  labs(
    title = "Impact on Confidence in Election Fairness when Election Staff \nIncludes Veterans and Family by Experiment Condition",
    subtitle = "Means comparison with 95% confidence intervals",
    caption = "estimate = 0.11635, SE = 0.02799, t-statistic = 4.157, CI [-0.171, -0.0614]"
  )+
  theme_bw()+
  theme(legend.position = 'bottom')

# save plot to images folder in project dir
ggplot2::ggsave(filename = 'coef_plot_q41.4.png', path = '~/R/GA_work/CDCE/cdce_vets/images')

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
models <- list(
  "Test Every Machine" = mod.q41.1,
  "Audit Ballots" = mod.q41.2,
  "Partisan Poll Watchers" = mod.q41.3,
  "Election Staff Includes Veterans" = mod.q41.4,
  "Election Staff Includes Lawyers" = mod.q41.5,
  "Election Staff Includes Students" = mod.q41.6
)

# print the coef plot and selectively color coefficients
ggstats::ggcoef_compare(models, conf.int = T, add_reference_rows = F)+
  ggplot2::scale_color_manual(
    values = c("black", "black", "black", "red", "black", "black"))+
  labs(
    title = "Impact on Confidence in Election Fairness when Election Staff \nIncludes Veterans and Family by Experiment Condition",
    subtitle = "Means comparison with 95% confidence intervals",
    caption = "estimate = 0.11635, SE = 0.02799, t-statistic = 4.157, CI [-0.171, -0.0614]"
  )+
  theme_bw()+
  theme(legend.position = 'bottom')


# save plot to images folder in project dir
ggplot2::ggsave(filename = 'q41_by_treatment_coef_plot.png', path = '~/R/GA_work/CDCE/cdce_vets/images')
