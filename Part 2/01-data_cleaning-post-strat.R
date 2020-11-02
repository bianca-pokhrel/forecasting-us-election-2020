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
library(dplyr)
# Read in the raw data. 
# Rstudio:
# raw_data <- read_dta("Data/usa_00002.dta")

# Local Computer:
raw_data <- read_dta("Part\ 2/usa_00003.dta")
#raw_data <- read_dta("inputs/data/usa_00003.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
names(raw_data)

reduced_data <- 
  raw_data %>% 
  dplyr::select(region,
         citizen,
         stateicp,
         sex, 
         age, 
         race, 
         hispan,
         bpl,
         educd,
         empstatd,
         ftotinc)
reduced_data <- rename(reduced_data, gender = sex)
reduced_data <- rename(reduced_data, state = stateicp)
rm(raw_data)

### race, hispanic, gender, age, state, income, education, employment


#### What's next? ####
#Sort for age >= 18
citizens <- reduced_data %>% filter(citizen == "naturalized citizen" | citizen == "born abroad of american parents") %>% filter(as.numeric(ftotinc) < 9999999)
reduced_data_18 <- citizens %>% filter(as.numeric(age) > 18)

#Consitent gender labels
reduced_data_18 <- reduced_data_18 %>%  mutate_at(vars(gender), .funs = 
                                                    funs(case_when(.== "male" ~ "Male",
                                                                   .== "female" ~ "Female")))

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
  ftotinc <= 24999 ~ "Less than $24,999",
  (24999 < ftotinc & ftotinc <= 49999) ~ "$25,000 to $49,999",
  (49999 < ftotinc & ftotinc <= 74999) ~ "$50,000 to $74,999",
  (74999 < ftotinc & ftotinc <= 99999) ~ "$75,000 to $99,999",
  (99999 < ftotinc & ftotinc <= 124999) ~ "$100,000 to $124,999",
  (124999 < ftotinc & ftotinc <= 149999) ~ "$125,000 to $149,999",
  (149999 < ftotinc & ftotinc <= 174999) ~ "$150,000 to $174,999",
  (174999 < ftotinc & ftotinc <= 199999) ~ "$175,000 to $199,999",
  199999 < ftotinc ~ "More than $200,000",
  is.na(ftotinc) ~ "NA"))

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
reduced_data_18 <- reduced_data_18 %>% mutate(ages = case_when(as.numeric(age) < 30 ~ "18 to 29",
                                                                    (30 <= as.numeric(age) & as.numeric(age) < 45) ~ "30 to 44",
                                                                    (45 <= as.numeric(age) & as.numeric(age) < 60) ~ "45 to 59",
                                                                    TRUE ~ "60+"))

# Hispanic->
unique(reduced_data_18$hispan)
reduced_data_18 <- reduced_data_18 %>% mutate(is_hispanic = case_when(hispan == "not hispanic" ~ "Not Hispanic",
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
                                                                         race == "black/african american/negro" ~ "African American",
                                                                         race == "other asian or pacific islander" ~ "Other Asian or Pacific Islander",
                                                                         race == "american indian or alaska native" ~ "Native American",
                                                                         race == "chinese" ~ "Asian(Chinese or Japanese)",
                                                                         race == "japanese" ~ "Asian(Chinese or Japanese)",
                                                                         TRUE ~ "Others"))
# Region
northeast <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island","Vermont", "New Jersey", "New York", "Pennsylvania")
midwest <- c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota")
south <- c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "District of Columbia", "West Virginia", "Alabama", "Kentucky", "Mississippi", "Tennessee","Arkansas", "Louisiana", "Oklahoma", "Texas")
west <- c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming", "Alaska", "California", "Hawaii", "Oregon", "Washington")

northeast <- lapply(northeast, tolower)
midwest <- lapply(midwest, tolower)
south <- lapply(south, tolower)
west <- lapply(west, tolower)

reduced_data_18 <- reduced_data_18 %>% mutate(region = ifelse(state %in% northeast, "Northeast",
                                                              ifelse(state %in% midwest, "Midwest",
                                                                     ifelse(state %in% south, "South",
                                                                            ifelse(state %in% west, "West", "NA")))))


### Stratified count
### Region not included bc not working rn
cleaned_data_strat <- 
  reduced_data_18 %>% 
  dplyr::select(gender, 
         region,
         ages,
         race_ethnicity, 
         is_hispanic,
         foreign_born,
         education,
         employment,
         household_income)

cleaned_data_strat <- rename(cleaned_data_strat, age_group = ages)
cleaned_data_strat <- rename(cleaned_data_strat, census_region = region)
cleaned_data_strat_count <- cleaned_data_strat %>%
  group_by(gender, 
           age_group,
           household_income,
           race_ethnicity,
           census_region
           )%>% 
  summarise(n = n()) 

