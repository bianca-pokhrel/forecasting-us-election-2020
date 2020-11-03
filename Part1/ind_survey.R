#### Workspace setup ####
library(haven)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(usmap)#allow us to graph cool maps
library(knitr)
library(kableExtra)
library(gridExtra)
library(waffle)#beauticul wat to show proportion

#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from []
# Author: Huining Qu
# Data: 26 October 2020
# Contact: mackenzie.qu@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!

# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(registration,
         vote_2016,
         vote_intention,
         trump_biden,
         ideo5,
         gender,
         race_ethnicity,
         household_income,
         state,
         census_region,
         age)


#### Clean Up ####
# make age group "18-29", "30-44", "45-59", "60+"
cleaned_data <- reduced_data %>%
  mutate(age_group = case_when(
    age >= 60 ~ "60+",
    age >= 45 & age <= 59 ~ "45 to 59",
    age >= 30 & age <= 44 ~ "30 to 44",
    age >= 18 & age <= 29 ~ "18 to 29",
  )
  )

# removes observation who will not vote or not eligible to vote
cleaned_data<-cleaned_data[!(cleaned_data$vote_intention
                             =="No, I will not vote but I am eligible" &
                               cleaned_data$vote_intention
                             =="No, I am not eligible to vote"&
                               cleaned_data$vote_intention
                             =="Not sure" &
                               cleaned_data$vote_intention
                             =="NA's"),]

# clean up trump_biden variable by grouping NA and simplifying name
cleaned_data<- cleaned_data%>%
  mutate_at(vars(trump_biden), .funs = 
              funs(case_when(
                .== "Joe Biden" ~ "Biden",
                .== "Donald Trump" ~ "Trump",
                .== "Dont Know" ~ "NA",
                .== "NA's" ~ "NA"
              )))

# name matcher unifies the state name in 2 datasets. 
names_matcher <- tibble(state_name = state.name, state = state.abb)
cleaned_data<-
  cleaned_data%>%
  left_join(names_matcher)

# remove all na contained in Biden trump for later modeling 
cleaned_data<-cleaned_data%>%
  drop_na(trump_biden)

# clean vote_2016
cleaned_data<- cleaned_data%>%
  mutate_at(vars(vote_2016), .funs = 
              funs(case_when(
                .== "Hillary Clinton" ~ "Hillary Clinton",
                .== "Donald Trump" ~ "Donald Trump",
                .== "Gary Johnson" ~ "Others",
                .== "Jill Stein" ~ "Others",
                .== "Someone else:" ~ "Others",
                .== "Did not vote, but was eligible" ~ "Others",
                .== "Was not eligible to vote" ~ "Others",
                .== "Don't recall" ~ "Others"
              )))

# clean race_ethnicity
cleaned_data<-cleaned_data%>%
  mutate_at(vars(race_ethnicity), .funs = 
              funs(case_when(
                .== "White" ~ "White",
                .== "Black, or African American" ~ "African American",
                .== "American Indian or Alaska Native" ~ "Native American",
                .== "Asian (Asian Indian)" ~ "Other Asian or Pacific Islander",
                .== "Asian (Chinese)" ~ "Asian(Chinese or Japanese)",
                .== "Asian (Filipino)" ~ "Other Asian or Pacific Islander",
                .== "Asian (Japanese)" ~ "Asian(Chinese or Japanese)",
                .== "Asian (Korean)" ~ "Other Asian or Pacific Islander",
                .== "Asian (Vietnamese)" ~ "Other Asian or Pacific Islander",
                .== "Asian (Other)" ~ "Other Asian or Pacific Islander",
                .== "Pacific Islander (Native Hawaiian)" ~ "Other Asian or Pacific Islander",
                .== "Pacific Islander (Guamanian)" ~ "Other Asian or Pacific Islander",
                .== "Pacific Islander (Samoan)" ~ "Other Asian or Pacific Islander",
                .== "Pacific Islander (Other)" ~ "Other Asian or Pacific Islander",
                .== "Some other race" ~ "Others"
              )))


#clean income
cleaned_data<- cleaned_data%>%
  mutate_at(vars(household_income), .funs = 
              funs(case_when(
                .=="Less than $14,999"~"Less than $24,999",
                .=="$15,000 to $19,999"~"Less than $24,999",
                .=="$20,000 to $24,999"~"Less than $24,999",
                .=="$25,000 to $29,999"~"$25,000 to $49,999",
                .=="$30,000 to $34,999"~"$25,000 to $49,999",
                .=="$35,000 to $39,999"~"$25,000 to $49,999",
                .=="$40,000 to $44,999"~"$25,000 to $49,999",
                .=="$45,000 to $49,999"~"$25,000 to $49,999",
                .=="$50,000 to $54,999"~"$50,000 to $74,999",
                .=="$55,000 to $59,999"~"$50,000 to $74,999",
                .=="$60,000 to $64,999"~"$50,000 to $74,999",
                .=="$65,000 to $69,999"~"$50,000 to $74,999",
                .=="$70,000 to $74,999"~"$50,000 to $74,999",
                .=="$75,000 to $79,999"~"$75,000 to $99,999",
                .=="$80,000 to $84,999"~"$75,000 to $99,999",
                .=="$85,000 to $89,999"~"$75,000 to $99,999",
                .=="$90,000 to $94,999"~"$75,000 to $99,999",
                .=="$95,000 to $99,999"~"$75,000 to $99,999",
                .=="$100,000 to $124,999"~"$100,000 to $124,999",
                .=="$125,000 to $149,999"~"$125,000 to $149,999",
                .=="$150,000 to $174,999"~"$150,000 to $174,999",
                .=="$175,000 to $199,999"~"$175,000 to $199,999",
                .=="$200,000 to $249,999"~"More than $200,000",
                .=="$250,000 and above"~"More than $200,000",
                .=="NA's"~"NA",
              )))

# save cleaned_data
save(cleaned_data, file = "cleaned_survey_data.Rdata")


# Data Preview
kable(head(cleaned_data[1:6]), format="latex", booktabs=TRUE) %>% 
  kable_styling(latex_options="scale_down")

kable(head(cleaned_data[7:13]), format="latex", booktabs=TRUE) %>% 
  kable_styling(latex_options="scale_down")


### Plot Raw Data ###
#Plot Biden Trump expected votes
cleaned_data%>%
  ggplot(aes(y=trump_biden, fill = trump_biden))+
  geom_bar(stat="count", width = 0.6)+
  scale_x_continuous(name = "Vote count", 
                     breaks = seq(0,3500, 500))+
  ylab("Vote Intention")+
  scale_fill_manual(values=c("Biden"= "steelblue3","Trump"= "lightcoral"), 
                    name = "Vote")+
  theme_minimal()

# Plot election forecast by States
#create data frame containing the frequency of biden/trump's votes of each state
state_vote<-as.data.frame(table(cleaned_data$trump_biden, cleaned_data$state_name))
#create new data frame
election_forecast<-data.frame(stringsAsFactors = FALSE)
#compare trump/biden's vote for each state, bind the person with majority vote 
#in the new data frame created. 
for (i in seq(1,dim(state_vote)[1],2)){
  if (state_vote$Freq[i] > state_vote$Freq[i+1]){
    election_forecast<-rbind(election_forecast,
                             c(as.character(state_vote$Var1[i]), 
                               as.character(state_vote$Var2[i])),
                             stringsAsFactors = FALSE)
  }
  else {
    election_forecast<-rbind(election_forecast,
                             c(as.character(state_vote$Var1[i+1]), 
                               as.character(state_vote$Var2[i+1])),
                             stringsAsFactors = FALSE)
  }
}
#change data frame variable names
names(election_forecast)[1]<-"vote"
names(election_forecast)[2]<-"state"
#use usmap to graph the new data, visualize the person with majority vote each state. 
plot_usmap(data=election_forecast, values = "vote")+
  scale_fill_manual(values = c("steelblue3", "lightcoral"))+
  theme(legend.position = "right")+
  labs(title = "Election Forecast by States", fill = "Vote")

# plot vote intention by political stance
ggplot(cleaned_data, aes(x=ideo5, fill=trump_biden))+
  geom_bar(alpha = 0.8)+
  facet_wrap(~trump_biden, ncol=1)+
  coord_flip()+
  theme_minimal()+
  ylab("Vote Count")+
  xlab("Political Stance")+
  theme(axis.text.y = element_text(size=6))+
  scale_fill_manual(values = c("steelblue3", "lightcoral"), guide = FALSE)


# plot vote intention by 2016 vote
ggplot(cleaned_data, aes(x=vote_2016, fill=trump_biden))+
  geom_bar(alpha = 0.8)+
  facet_wrap(~trump_biden, ncol=1)+
  coord_flip()+
  theme_minimal()+
  ylab("Vote Count")+
  xlab("2016 Vote")+
  scale_fill_manual(values = c("steelblue3", "lightcoral"), guide = FALSE)


#Plot vote intention by gender
ggplot(cleaned_data, aes(x=gender, fill=trump_biden))+
  geom_bar(alpha = 0.8)+
  facet_wrap(~trump_biden, ncol=1)+
  coord_flip()+
  theme_minimal()+
  ylab("Vote Count")+
  xlab("Gender")+
  scale_fill_manual(values = c("steelblue3", "lightcoral"), guide = FALSE)


#plot vote intention by race
ggplot(cleaned_data, aes(x=race_ethnicity, fill=trump_biden))+
  geom_bar(alpha = 0.8)+
  facet_wrap(~trump_biden, ncol=1)+
  coord_flip()+
  theme_minimal()+
  ylab("Vote Count")+
  xlab("Race")+
  theme(axis.text.y = element_text(size=6))+
  scale_fill_manual(values = c("steelblue3", "lightcoral"), guide = FALSE)



#plot vote intention by age
ggplot(cleaned_data, aes(x=as.factor(age_group), fill=trump_biden))+
  geom_bar(alpha = 0.8)+
  facet_wrap(~trump_biden, ncol=1)+
  coord_flip()+
  theme_minimal()+
  ylab("Vote Count")+
  xlab("Age Group")+
  scale_x_discrete(labels = c("18~29","30~44", "45~59","60+"))+
  scale_fill_manual(values = c("steelblue3", "lightcoral"), guide = FALSE)



# Graph household income tears for trump/biden votes. 
# remove na, and levels ensures the order presented on graph

ggplot(data = subset(cleaned_data, !is.na(household_income)), 
       aes(x=factor(household_income, 
                    levels = c("Less than $24,999",
                               "$25,000 t0 $49,999",
                               "$50,000 to $74,999",
                               "$75,000 to $99,999",
                               "$100,000 to $124,999",  
                               "$125,000 to $149,999",
                               "$150,000 to $174,999",
                               "$175,000 to $199,999",
                               "$200,000 and above"),
                    exclude = "NA"), 
           fill=trump_biden))+
  geom_bar(alpha = 1)+
  facet_wrap(~trump_biden, ncol=1)+
  coord_flip()+
  theme_minimal()+
  ylab("Vote Count")+
  xlab("Household Income")+
  theme(axis.text.y = element_text(size=6))+
  scale_fill_manual(values = c("steelblue3", "lightcoral"), guide = FALSE)
