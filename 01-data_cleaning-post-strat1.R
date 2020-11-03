#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://usa.ipums.org/usa/
# Author: Ananya Jha
# Data: 2 November 2020
# Contact: ananya.jha@mail.utoronto.ca
# License: MIT




#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
raw_data <- read_dta("/Users/ananyajha/forecasting-US-election-using-MRP/usa_00002.dta.gz")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keeping variables of interest
reduced_data <- 
  raw_data %>% 
  select(#region,
         stateicp,
         sex, 
         age, 
         race, 
         empstat
         #bpl,
         #citizen,
         #educd,
         #labforce,
         #labforce
         )
         
#renaming variable columns to match survey data
reduced_data<-
  reduced_data %>%
  rename(gender = sex, race_ethnicity = race, employment = empstat)


#removing unimportant columns
reduced_data <- 
 reduced_data %>% 
 filter(age != "less than 1 year old") %>%
   filter(age != "90 (90+ in 1980 and 1990)")

#changing values to match survey data 
reduced_data<-reduced_data %>%
  mutate(race_ethnicity = ifelse(race_ethnicity=="white", "White", ifelse(race_ethnicity=="black/african american/negro","Black, or African American", "Other" ))
  )
#changing values to match survey data 
reduced_data<-reduced_data %>%
  mutate(gender = ifelse(gender=="female", "Female", ifelse(gender=="male","Male", "Respondent Skipped" ))
  )
#changin state names to codes
reduced_data <- reduced_data %>% 
 mutate(stateicp = str_to_title(stateicp))

reduced_data <- reduced_data %>% 
 mutate(state = state.abb[match(reduced_data$stateicp, state.name)])

#calculating n for each category 
reduced_data <- 
  reduced_data %>%
  count(age,gender,race_ethnicity,employment, state) %>%
  group_by(age,gender,race_ethnicity,employment, state) 

#removing NA values and only keeping male and female values to match survey data
reduced_data <- 
  reduced_data %>%
  filter(gender=="Male" | gender == "Female" ) %>%
  filter(employment != "n/a")

#typecasting age to integer
reduced_data$age <- as.integer(reduced_data$age)

# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data, "outputs/census_data2.csv")

#References
#Rohan Alexander and Sam Caetano, "01-data_cleaning-post-strat1.R" 22 October 2020

         