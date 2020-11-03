#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://www.voterstudygroup.org/publication/nationscape-data-set**
# Author: Ananya Jha
# Data: 27 October 2020
# Contact: ananya.jha@mail.utoronto.ca 
# License: MIT



#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("/Users/ananyajha/forecasting-US-election-using-MRP")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("inputs/ns20200625/ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keeping some variables
reduced_data <- 
  raw_data %>% 
  select(#interest,
         #registration,
         #vote_2016,
         #vote_intention,
         vote_2020,
         news_sources_new_york_times,
         employment,
         abortion_any_time,
         #foreign_born,
         ban_guns,
         #language,
         #religion,
         #medicare_for_all,
         gender,
         #gov_insurance,
         extra_trump_corona,
         green_new_deal,
         race_ethnicity,
         #household_income,
         #education,
         state,
         age)

#making the vote(response variable) binary for the first model
reduced_data<-
  reduced_data %>%
  mutate(vote_biden = 
           ifelse(vote_2020=="Joe Biden", 1, 0))

#making the response variable for the second model
reduced_data<-
  reduced_data %>%
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0))

#cleaning the race variable and reducing options
reduced_data<-
  reduced_data %>%
  mutate(race_ethnicity = 
           ifelse(race_ethnicity=="White", "White", ifelse(race_ethnicity=="Black, or African American","Black, or African American", "Other" ))
)

#cleaning the employment columns (to match to the census data)
reduced_data<-
  reduced_data %>%
  mutate(employment = 
           ifelse(employment=="Full-time employed" | employment=="Part-time employed"|employment=="Self-employed" , "employed", ifelse(employment=="Homemaker" |employment=="Unemployed or temporarily on layoff" , "unemployed", "not in labor force"))
  )


# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "outputs/survey_data.csv")

#References
#Rohan Alexander and Sam Caetano,"01-data_cleaning-survey1.R", 22 October 2020
