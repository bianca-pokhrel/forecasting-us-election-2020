#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data. 
# Rstudio:
# raw_data <- read_dta("Data/usa_00002.dta")

# Local Computer:
# raw_data <- read_dta("inputs/data/usa_00002.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
names(raw_data)

#TODO: Get dataset with citizen staus
reduced_data <- 
  raw_data %>% 
  select(region,
         stateicp,
         sex, 
         age, 
         race, 
         hispan,
         bpl,
         educd,
         empstatd,
         ftotinc)
rm(raw_data)

### race, hispanic, gender, age, state, income, education, employment


#### What's next? ####
#Sort for age >= 18
reduced_data_18 <- reduced_data %>% filter(as.numeric(age) > 18)


#Foreign born -> US or another country
unique(reduced_data_18$bpl) 
us_states <- c('alabama', 
               'alaska', 
               'arizona',
               'arkansas', 
               'california', 
               'colorado', 
               'connecticut', 
               'delaware', 
               'florida', 
               'georgia', 
               'hawaii', 
               'idaho', 
               'illinois',
               'indiana', 
               'iowa', 
               'kansas', 
               'kentucky', 
               'louisiana', 
               'maine', 
               'maryland',
               'massachusetts', 
               'michigan', 
               'minnesota', 
               'mississippi', 
               'missouri', 
               'montana', 
               'nebraska', 
               'nevada', 
               'new hampshire', 
               'new jersey', 
               'new mexico', 
               'new york', 
               'north carolina', 
               'north dakota', 
               'ohio', 
               'oklahoma', 
               'oregon', 
               'pennsylvania',
               'rhode island', 
               'south carolina', 
               'south dakota', 
               'tennessee', 
               'texas', 
               'utah', 
               'vermont', 
               'virginia', 
               'washington', 
               'west virginia', 
               'wisconsin', 
               'wyoming',
               'u.s. virgin islands')

reduced_data_18 <- reduced_data_18 %>% 
  mutate(foreign_born = ifelse(bpl %in% us_states, "US", "Foreign Country")) 

#Household income -> Brackets
unique(reduced_data_18$ftotinc) 

reduced_data_18 <- reduced_data_18 %>% mutate(household_income = case_when(
  ftotinc <= 14999 ~ "Less than $14,999",
  (14999 < ftotinc & ftotinc <= 24999) ~ "$15,000 t0 $24,999",
  (24999 < ftotinc & ftotinc <= 29999) ~ "$25,000 to $29,999",
  (29999 < ftotinc & ftotinc <= 34999) ~ "$30,000 to $34,999",
  (34999 < ftotinc & ftotinc <= 39999) ~ "$35,000 to $39,999",
  (39999 < ftotinc & ftotinc <= 44999) ~ "$40,000 to $44,999",
  (44999 < ftotinc & ftotinc <= 49999) ~ "$45,000 to $49,999",
  (49999 < ftotinc & ftotinc <= 54999) ~ "$50,000 to $54,999",
  (54999 < ftotinc & ftotinc <= 59999) ~ "$55,000 to $59,999",
  (59999 < ftotinc & ftotinc <= 64999) ~ "$60,000 to $64,999",
  (64999 < ftotinc & ftotinc <= 69999) ~ "$65,000 to $69,999",
  (69999 < ftotinc & ftotinc <= 74999) ~ "$70,000 to $74,999",
  (74999 < ftotinc & ftotinc <= 79999) ~ "$75,000 to $79,999",
  (79999 < ftotinc & ftotinc <= 84999) ~ "$80,000 to $84,999",
  (84999 < ftotinc & ftotinc <= 89999) ~ "$85,000 to $89,999",
  (89999 < ftotinc & ftotinc <= 94999) ~ "$90,000 to $94,999",
  (94999 < ftotinc & ftotinc <= 99999) ~ "$95,000 to $99,999",
  (99999 < ftotinc & ftotinc <= 124999) ~ "$100,000 to $124,999",
  124999 < ftotinc ~ "More than $125,000",
  is.na(ftotinc) ~ "Other"))

# Education -> Brackets
unique(reduced_data_18$educd) 

less_educ = c("kindergarten", "nursery school, preschool", "no schooling completed", "grade 1", "grade 2", "grade 3", "grade 4", "grade 5", "grade 6", "grade 7", "grade 8")
high_school_in_progress = c("grade 9","grade 10","grade 11","grade 12, no diploma")

reduced_data_18 <- reduced_data_18 %>% mutate(education = case_when(
  (educd == '1 or more years of college credit, no degree' |  educd == "some college, but less than 1 year") ~ "Completed some college, but no degree",
  educd == "associate's degree, type not specified" ~ "Associate Degree",
  educd == "master's degree" ~ "Masters degree",
  educd == "doctoral degree" ~ "Doctoral degree",
  educd == "bachelor's degree" ~ "College degree",
  educd == "regular high school diploma" ~ "High school graduate",
  educd == "ged or alternative credential" ~ "Ged or alternative credential",
  educd == "professional degree beyond a bachelor's degree" ~ "Professional degree beyond a bachelor's degree",
  TRUE ~ "Other"))
#(is.na(educd) | educd %in% less_educ)

# New col -> age group
reduced_data_18 <- reduced_data_18 %>% mutate(age_group = case_when(as.numeric(age) < 30 ~ "Ages 18 to 29",
                                                                    (30 <= as.numeric(age) & as.numeric(age) < 45) ~ "Ages 30 to 44",
                                                                    (45 <= as.numeric(age) & as.numeric(age) < 60) ~ "Ages 45 to 59",
                                                                    TRUE ~ "Ages 60+"))

# Hispanic->
unique(reduced_data_18$hispan)
reduced_data_18 <- reduced_data_18 %>% mutate(hispanic = case_when(hispan == "not hispanic" ~ "Not Hispanic",
                                                                   TRUE ~ "Hispanic"))

# Emploment-> armed forces grouped
unique(reduced_data_18$empstatd) 

reduced_data_18 <- reduced_data_18 %>% mutate(employment = case_when(empstatd == "at work" ~ "Full-time employed",
                                                                     empstatd == "not in labor force" ~ "Not in labor force",
                                                                     empstatd == "unemployed" ~ "Unemployed",
                                                                     empstatd == "has job, not working" ~ "Has job, not working",
                                                                     TRUE ~ "Other"))

# Race -> Group 2+ races into other
unique(reduced_data_18$race) 
reduced_data_18 <- reduced_data_18 %>% mutate(race_ethnicity = case_when(race == "white" ~ "White",
                                                                         race == "black/african american/negro" ~ "Black, or African American",
                                                                         race == "other asian or pacific islander" ~ "Other asian or pacific islander",
                                                                         race == "american indian or alaska native" ~ "American indian or alaska native",
                                                                         race == "chinese" ~ "Chinese",
                                                                         race == "japanese" ~ "Japanese",
                                                                         TRUE ~ "Other"))

### Stratified count
cleaned_data <- 
  reduced_data_18 %>% 
  select(sex, 
         age,
         age_group,
         race_ethnicity, 
         hispanic,
         foreign_born,
         education,
         household_income)

out2 <- cleaned_data %>%
  group_by(sex, 
           age_group,
           race_ethnicity, 
           hispanic,
           foreign_born,
           education,
           employment,
           household_income)%>% 
  summarise(count = n()) 

summary(cleaned_data)         
