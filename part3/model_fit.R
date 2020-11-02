#### Preamble ####
# Purpose: Model Fit & Predicition to Forecast US Election 2020
# Author: Bianca Pokhrel
# Data: 30 October 2020
# Contact: bianca.pokhrel@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


# UNCOMMENT TO INSTALL PACKAGES IF YOU NEED
# install.packages("broom")
# install.packages("here")
# install.packages("skmr")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("labelled")
# install.packages("ggplot2")
# install.packages("readxl")
# install.packages("devtools")
# install.packages("digest")
# install.packages("foreign)
# install.packages("arm")
# install.packages("brms)
# install.packages("effects")
# install.packages("haven")
# install.packages("aod")
library(tidyverse)
library(labelled)
library(dplyr)
library(ggplot2)
library(readxl)
library(foreign)
library(effects)
library(haven)

load("Part1/cleaned_survey_data.Rdata")
source("Part\ 2/01-data_cleaning-post-strat.R")
size <- 6479


#### ok now I have the real data ####

#create binary var
cleaned_data$trump_biden[cleaned_data$trump_biden %in% "Trump"] <- 1
cleaned_data$trump_biden[cleaned_data$trump_biden %in% "Biden"] <- 0

cleaned_data$state_name = tolower(cleaned_data$state_name)
#set as numeric
#convert from chr to numeric
cleaned_data$trump_biden = as.numeric(cleaned_data$trump_biden)

# omit the NA's since model will remove them anyway
cleaned_data = na.omit(cleaned_data)

#### playing around with model fits ####

set.seed(123)
library(MASS)

#run an OLS regression to start
OLS_model <- lm(trump_biden ~ gender + factor(state_name) + factor(race_ethnicity) + 
                  factor(household_income) + factor(age_group), 
                data = cleaned_data)
broom::tidy(OLS_model)

#plot just to see what's up... give analysis for why this definitely isn't a good model
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(OLS_model)

#mkay so definitely not good model... QQ plot is def not linear & residuals vs. fitted aren't random/independent
#clearly there's something else going on... spoiler- looks like it's logistic regression


log_fit_full <- glm(trump_biden ~ (gender) + factor(race_ethnicity) + 
                      factor(household_income) + factor(state_name) + factor(age_group), 
               data = cleaned_data,
               family = binomial(link = "logit")
               )
summary(log_fit_full)

# ok see what's up
broom::tidy(log_fit_full)

#much better!
# now to see which variables we can drop- combo of anova + AIC

#Likelihood ratio test 1- without census region
model_test1 <- glm(trump_biden ~ (gender) + 
                  + factor(race_ethnicity) + factor(household_income) + factor(age_group), 
                  data = cleaned_data,
                  family = binomial(link = "logit"))
anova(model_test1, log_fit_full, test="Chisq")
summary(model_test1)
# need to keep state, AIC value also increases

#Likelihood ratio test 2- without gender
model_test2 <- glm(trump_biden ~ factor(race_ethnicity) + factor(household_income)+ 
                     factor(state_name) + factor(age_group), 
                    data = cleaned_data,
                    family = binomial(link = "logit")
)
anova(model_test2, log_fit_full, test="Chisq")
summary(model_test2)
# need to keep gender + AIC skyrockets


#Likelihood ratio test 3- without race
model_test4 <- glm(trump_biden ~ (gender) + factor(household_income) + 
                     factor(state_name) + factor(age_group), 
                    data = cleaned_data,
                    family = binomial(link = "logit")
)
anova(model_test4, log_fit_full, test="Chisq")
summary(model_test4)
# anova checks show massive increase- definitely significant + massive increase in AIC

#Likelihood ration test 4- without income
model_test5 <- glm(trump_biden ~ (gender) + factor(race_ethnicity) + 
                     factor(state_name) + factor(age_group), 
                   data = cleaned_data,
                   family = binomial(link = "logit")
)
#anova(model_test5, log_fit_full, test="Chisq")
summary(model_test5)

#Likelihood ratio test 5- without age
model_test6 <- glm(trump_biden ~ (gender) + factor(race_ethnicity) + 
                     factor(household_income) +  factor(state_name), 
                    data = cleaned_data,
                    family = binomial(link = "logit")
)
anova(model_test6, log_fit_full, test="Chisq")
summary(model_test6)
# need to keep age + AIC increases

# need to keep education + AIC increases
# all in all, it tells us that we should keep the full model

# calculate confidence intervals
confint.default(log_fit_full)

# look at the odds ratios and 95% CI & interpret 
# https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/

exp(cbind(OR = coef(log_fit_full), confint.default(log_fit_full)))


#### try to predict using post_strat data ####

# retrieve calculations for proportions of each cell by indep. var

gender_prop <- cleaned_data_strat_count %>%
  ungroup() %>%
  group_by(gender) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup()

race_prop <- cleaned_data_strat_count %>%
  ungroup() %>%
  group_by(race_ethnicity) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup()

region_prop <- cleaned_data_strat_count %>%
  ungroup() %>%
  group_by(state_name) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup()

income_prop <- cleaned_data_strat_count %>%
  ungroup() %>%
  group_by(household_income) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup()

age_prop <- cleaned_data_strat_count %>%
  ungroup %>%
  group_by(age_group) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup()

# predictions based on each variable

# age
age_pred <- data.frame(predict(log_fit_full, newdata = age_prop, type = "response"))
colnames(age_pred) = "vote_pred"

age_prop <- cbind(age_prop, age_pred)
age_prop %>%
  mutate (age_predict_prop = vote_pred*prop) %>%
  group_by(age_group) %>%
  summarise(vote_pred = sum(age_predict_prop)) %>%
  summarise(mean = mean(vote_pred),
            lower = quantile(vote_pred, 0.025),
            higher = quantile(vote_pred, 0.975))

# gender
gender_pred <- data.frame(predict(log_fit_full, newdata = gender_prop, type = "response"))
colnames(gender_pred) = "vote_pred"

gender_prop <- cbind(gender_prop, gender_pred)
gender_prop %>%
  mutate (gender_predict_prop = vote_pred*prop) %>%
  group_by(gender) %>%
  summarise(vote_pred = sum(gender_predict_prop)) %>%
  summarise(mean = mean(vote_pred),
            lower = quantile(vote_pred, 0.025),
            higher = quantile(vote_pred, 0.975))

# race
race_pred <- data.frame(predict(log_fit_full, newdata = race_prop, type = "response"))
colnames(race_pred) = "vote_pred"

race_prop <- cbind(race_prop, race_pred)
race_prop %>%
  mutate (race_predict_prop = vote_pred*prop) %>%
  group_by(race_ethnicity) %>%
  summarise(vote_pred = sum(race_predict_prop)) %>%
  summarise(mean = mean(vote_pred),
            lower = quantile(vote_pred, 0.025),
            higher = quantile(vote_pred, 0.975))

# region
region_pred <- data.frame(predict(log_fit_full, newdata = region_prop, type = "response"))
colnames(region_pred) = "vote_pred"

region_prop <- cbind(region_prop, region_pred)
region_prop %>%
  mutate (region_predict_prop = vote_pred*prop) %>%
  group_by(state_name) %>%
  summarise(vote_pred = sum(region_predict_prop)) %>%
  summarise(mean = mean(vote_pred),
            lower = quantile(vote_pred, 0.025),
            higher = quantile(vote_pred, 0.975))

# income
income_pred <- data.frame(predict(log_fit_full, newdata = income_prop, type = "response"))
colnames(income_pred) = "vote_pred"

income_prop <- cbind(income_prop, income_pred)
income_prop %>%
  mutate (income_predict_prop = vote_pred*prop) %>%
  group_by(household_income) %>%
  summarise(vote_pred = sum(income_predict_prop)) %>%
  summarise(mean = mean(vote_pred),
            lower = quantile(vote_pred, 0.025),
            higher = quantile(vote_pred, 0.975))

#### 'beautiful' graphs ####


### brm ###

# bayesian_model <- brm(trump_biden ~ (gender) + (is_hispanic)
#                        + (race_ethnicity) + factor(household_income) + 
#                          factor(education) + factor(state) + factor(age),
#                        family = bernoulli(),
#                        data = cleaned_data,
#                        chain = 2)
# print(bayesian_model)
# 
# # FROM THE TEXTBOOK: by using post-stratification, we are assuming that data are a random sample from some well-defined population
# # also textbook says that for a well defined population like this, in political polls, we can use default priors and assume that having a 
# # somewhat. prior doesn't really help us in this case- thus can use non-informative priors or the default weak priors that stan provides
# # additionally, due to the large sample size we are using, we are able to 'ignore' priors as they will be overtaken by the likelihood
# # function. Lastly, reference all of the other articles that find election models and use them as research into why we can use default priors
# 
# 
# summary(bayesian_model)