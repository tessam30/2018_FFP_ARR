# Clean up Food For Peace data and plot results
# Date: 2017_12_19
# Author: Tim Essam, Ph.D. - GeoCenter


# libraries and data ------------------------------------------------------
library(tidyverse)
library(readxl)
library(llamar)
library(lubridate)
library(data.table)
library(knitr)
#library(splitstackshape)

data_dir <- "~/Documents/USAID/2018_FFP_TechTracking/"
df <- read_excel(str_c(data_dir, 'ARR_Attachment_Technical_Sectors_Tracking.xlsx'))

# source the crosswalks
source(str_c(data_dir, "text_cw.R"))


# Remove columns with all missing data
data <- Filter(function(x)!all(is.na(x)), df) %>% 
  filter(., `Prime awardee` != 'KG Test') # drop Kyla's test case

varnames <- data_frame(names(data))

# Check the proposed names with FFP -- hand edited in google drive
write_csv(varnames, file.path(data_dir, "ARR_TechTrack_Names.csv"))
newnames <- read_csv(str_c(data_dir, 'ARR_TechTrack_Names_cw.csv')) 

# use rlang to set new names based on the crosswalk
#  https://stackoverflow.com/questions/43742369/rename-variables-via-lookup-table-in-r
data = data %>% 
  rename(!!!setNames(rlang::syms(as.character(newnames$`names(data)`)), newnames$var_name))

# Reordering for the documentation so things are in alpha order
data = data %>% select(order(colnames(.)))
names(data)

write_csv(data, file.path(data_dir, 'ARR_TechTrack_wide.csv'))


# Data cleaning tasks -----------------------------------------------------
# Flag all awards that are Emergency or contain FFP-G in award_num
# Split columns wide that contain mulitple responses -- generally separated by a comma
# Need to determine all columns that are activities, then determine universe of possible responses

# String replace will find the patterns we qre looking for and replace them with desired text
# if we make these replacements a list, we can map this call across the entire dataframe
# To make things simple, we will parse on on commas
data[cw_text$lookupVar[1]]

# Used a combination of mapping into a dataframe and the mapValues function from BBmisc
# This step is simply replacing the troublesome chunks of text with comma-less values so we can parse below
subset_wide = data %>% 
  map_df(., function(x) BBmisc::mapValues(x, 
                                          cw_text$lookupText, 
                                          cw_text$newText, 
                                          regex = TRUE)) %>% 
  
  # Keep only rows that have multiple response answers + the award number as the unique identifier
  select((contains("activities")), 
         contains("approach"), 
         contains("subsector"), 
         award_num) %>% 
  
  # cSplit allows us to split on the comma, create new column variables, while spreading wide
  splitstackshape::cSplit(., 
                          dput(as.character(names(.))), 
                          sep = ",", 
                          direction = "wide") %>%
  
  #this filter function will go through the resulting dataframe and remove all blank columns
  Filter(function(x)!all(is.na(x)), .) %>% 
  rename(award_num = award_num_01)

# Merge the wide separated data with the original dataframe -- need to decide whether or not you want original names
data_wide = data %>% 
  select((-contains("activities")), 
         -contains("approach"), 
         -contains("subsector")) %>% 
  left_join(., 
            subset_wide, 
            by = c("award_num")) %>% 
  
  # add in a flag for emergency awards
  mutate(emerg_award = ifelse(award_num %like% "FFP-G", 1, 0))


# Tidy dataframes for each dimension of data ------------------------------

tidy_activity = data_wide %>% 
  gather(contains("activities"), key = activity_categ, value = activity) %>% 
  filter(!is.na(activity)) %>% 
  mutate(activity_categ = substr(activity_categ, 1, nchar(activity_categ) - 3))
  
tidy_approach = data_wide %>% 
  gather(contains("approach"), key = approach_categ, value = approach) %>% 
  filter(!is.na(approach)) %>% 
  mutate(approach_categ = substr(approach_categ, 1, nchar(approach_categ) - 3))

tidy_subsector = data_wide %>% 
  gather(contains("subsector"), key = subsector_categ, value = subsector) %>% 
  filter(!is.na(subsector)) %>% 
  mutate(subsector_categ = substr(subsector_categ, 1, nchar(subsector_categ) - 3))
  
tidy_budget = data_wide %>% 
  select((-contains("activities")), -contains("approach"), -contains("subsector")) %>% 
  gather(contains("budget"), key = budget_categ, value = budget) %>% 
  select(award_num, project, contains("date"), everything())
  


# split columns -----------------------------------------------------------


# What country has most reported programs?
data %>% 
  group_by(prime) %>% 
  tally() %>% 
  arrange(desc(n))




new <- data
new[] <- lapply(data, function(x) text_cw$[match])

