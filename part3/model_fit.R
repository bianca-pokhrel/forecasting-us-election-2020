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
library(arm)
library(brms)
library(effects)
library(haven)
library(aod)
#### create some simulated data ####
load("Part1/survey_data.Rdata")
size <- 6479


sim_data <- tibble(
  gender = sample(c('Male', 'Female'), size, replace = TRUE, prob = c(0.51, 0.49)),
  interest = sample(c("Most of the time", "Some of the time", "Only now and then", "Hardly at all", NA), 
                    size, replace = TRUE, prob = c(0.447, 0.354, 0.141, 0.057, 0.001)),
  registration = sample(c("Registered", "Not registered", "Don't know", NA),
                        size, replace = TRUE, prob = c(0.826, 0.149, 0.023, 0.002)),
  vote_2016 = sample(c('Donald Trump', 'Hillary Clinton', "Did not vote, but was eligible",
                       "Was not eligible to vote", "Gary Johnson", "Don't recall",
                       "Other"), size, replace = TRUE, prob = c(0.337, 0.312, 0.153, 0.114, 0.027, 0.024, 0.033)),
  vote_intention = sample(c("Yes, I will vote", "No I will not vote but I am eligible",
                            "No, I am not eligible to vote", "Not sure", NA), size,
                          replace = TRUE, prob = c(0.798, 0.072, 0.045, 0.082, 0.003)),
  vote_2020 = sample(c("Donald Trump", "Joe Biden", "Someone else", "I would not vote",
                       "I am not sure/don't know", NA), size, replace = TRUE,
                     prob = c(0.383, 0.419, 0.038, 0.057, 0.1, 0.003)),
  trump_biden = sample(c("Biden", NA), size, replace = TRUE, prob = c(0.4746, 0.525)),
  ideo5 = sample(c("Very Liberal", "Liberal", "Moderate", "Conservative",
                   "Very Conservative", "Not Sure", NA), size, replace = TRUE,
                 prob = c(0.107, 0.165, 0.339, 0.177, 0.109, 0.098, 0.005)),
  employment = sample(c("Full-time employed", "Retired", "Unemployed or temporarily on layoff",
                        "Part-time employed", "Self-employed", "Other", NA),
                      size, replace = TRUE, prob = c(0.392, 0.172, 0.106, 0.091, 0.064, 0.172, 0.003)),
  foreign_born = sample(c("The United States", "Another country"), size, replace = TRUE,
                        prob = c(0.928, 0.072)),
  census_region = sample(c("Northeast", "Midwest", "South", "West"), size,
                         replace = TRUE, prob = c(0.193, 0.208, 0.375, 0.224)),
  hispanic = sample(c("Not Hispanic", "Hispanic"), size, replace = TRUE,
                    prob = c(0.848, 0.152)),
  race_ethnicity = sample(c("White", "Black, or African American", "Some other race",
                            "Asian (Asian Indian)", "American Indian or Alaska Native",
                            "Asian (Chinese)", "Other"), size, replace = TRUE,
                          prob = c(0.743, 0.119, 0.071, 0.015, 0.014, 0.013, 0.025)),
  household_income = sample(c("Less than $14,999", "$100,000 to $14,999", 
                              "$25,000 to $29,999", "$30,000, $34,999", 
                              "$50,000 to $54,999", "Other", NA), size, replace = TRUE,
                            prob = c(0.150, 0.071, 0.052, 0.051, 0.051, 0.567, 0.058)),
  education = sample(c("College Degree (such as B.A, B.S.)", "Completed some college, but no degree",
                       "High school graduate", "Masters degree", "Completed some high school",
                       "Associate Degree", "Other"), size, replace = TRUE,
                     prob = c(0.228, 0.205, 0.166, 0.099, 0.098, 0.0879, 0.1149)))

#make the trump_biden variable binary... replace NA with 1 for Trump, replace Biden with 0
sim_data$trump_biden[sim_data$trump_biden %in% NA] <- 1
sim_data$trump_biden[sim_data$trump_biden %in% "Biden"] <- 0

#convert from chr to numeric
sim_data$trump_biden = as.numeric(sim_data$trump_biden)

# check survey
head(sim_data)

#check summary statistics
summary(sim_data)

### ok now I have the real data ###

#create binary var
cleaned_data$trump_biden[cleaned_data$trump_biden %in% "Trump"] <- 1
cleaned_data$trump_biden[cleaned_data$trump_biden %in% "Biden"] <- 0

#set as numeric
#convert from chr to numeric
cleaned_data$trump_biden = as.numeric(cleaned_data$trump_biden)

### playing around with model fits ###

set.seed(123)
library(MASS)

#run an OLS regression to start
OLS_model <- lm(trump_biden ~ gender + state + is_hispanic + 
                  race_ethnicity + household_income + education + age, 
                data = cleaned_data)
broom::tidy(OLS_model)

#plot just to see what's up... give analysis for why this definitely isn't a good model
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(OLS_model)

#mkay so definitely not good model... QQ plot is def not linear & residuals vs. fitted aren't random/independent
#clearly there's something else going on... spoiler- looks like it's logistic regression

log_fit_full <- glm(trump_biden ~ (gender) + (is_hispanic)
                   + (race_ethnicity) + factor(household_income) + 
                  factor(education) + factor(state) + age, 
               data = cleaned_data,
               family = binomial(link = "logit")
               )
summary(log_fit_full)
# ok see what's up
broom::tidy(log_fit_full)
layout(matrix(c(1,2,3,4),2,2))
plot(log_fit_full, main = "FULL")

#much better!
# now to see which variables we can drop- combo of anova + AIC

#Likelihood ratio test 1- without state
model_test1 <- glm(trump_biden ~ (gender) + (is_hispanic)
                  + (race_ethnicity) + as.factor(household_income) + 
                    as.factor(education) + age, 
                  data = cleaned_data,
                  family = binomial(link = "logit"))
anova(model_test1, log_fit_full, test="Chisq")
summary(model_test1)
# need to keep state, AIC value also increases

#Likelihood ratio test 2- without gender
model_test2 <- glm(trump_biden ~ (is_hispanic)
                    + (race_ethnicity) + as.factor(household_income) + 
                      as.factor(education) + state + age, 
                    data = cleaned_data,
                    family = binomial(link = "logit")
)
anova(model_test2, log_fit_full, test="Chisq")
summary(model_test2)
# need to keep gender + AIC skyrockets

#Likelihood ratio test 3- without is_hispanic
model_test3 <- glm(trump_biden ~ (gender)
                    + (race_ethnicity) + as.factor(household_income) + 
                      as.factor(education) + state + age, 
                    data = cleaned_data,
                    family = binomial(link = "logit")
)
anova(model_test3, log_fit_full, test="Chisq")
summary(model_test3)
# need to keep hispanic + very slight increase in AIC

#Likelihood ratio test 4- without race
model_test4 <- glm(trump_biden ~ (gender) + (is_hispanic)
                    + as.factor(household_income) + 
                      as.factor(education) + state + age, 
                    data = cleaned_data,
                    family = binomial(link = "logit")
)
anova(model_test4, log_fit_full, test="Chisq")
summary(model_test4)
# anova checks show massive increase- definitely significant + massive increase in AIC


#Likelihood ratio test 6- without age
model_test6 <- glm(trump_biden ~ (gender) + (is_hispanic)
                    + (race_ethnicity) + as.factor(household_income) + 
                      as.factor(education) + state, 
                    data = cleaned_data,
                    family = binomial(link = "logit")
)
anova(model_test6, log_fit_full, test="Chisq")
summary(model_test6)
# need to keep age + AIC increases

#Likelihood ratio test 7- without education
model_test7 <- glm(trump_biden ~ (gender) + (is_hispanic)
                    + (race_ethnicity) + as.factor(household_income) + state + age, 
                    data = cleaned_data,
                    family = binomial(link = "logit")
)
anova(model_test7, log_fit_full, test="Chisq")
summary(model_test1) 
# need to keep education + AIC increases
# all in all, it tells us that we should keep the full model

# calculate confidence intervals
confint.default(log_fit_full)

# look at the odds ratios and 95% CI & interpret 
# https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/

exp(cbind(OR = coef(log_fit_full), confint.default(log_fit_full)))


# try to predict using post_strat data

post_strat <- cbind(out2, predict(log_fit_full, newdata = out2, type = "link",
                                  se = TRUE))
post_strat <- within(post_strat, {
  pred_prob <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})







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