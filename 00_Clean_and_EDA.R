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


# Remove columns missing all data (empty cols) as these are unnecessary
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
# Data notes as 2018_02_20: DRR does not have an activities; HIV does not have activities so we should be able
# to simply rename the drr_subsectors to drr_activities and hiv_subsectors to hiv_activities and then use a crosswalk
# with the subsector and activity to re-map in the sector labels

data = data %>% 
  select(order(colnames(.))) %>% 
  mutate(hiv_activities_hiv = hiv_subsectors, 
         drr_activities_drr = drr_subsectors, 
         youth_activities_youth = youth_subsectors,
         program_activities_pa = program_approaches)
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
# Use the map function to iterate over each column of the dataframe
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
  # dplyr separate was not quite appropriate for this task -- but cSplit does make everything a factor 
  # so use the type.convert = FALSE option to keep them as characters
  splitstackshape::cSplit(., 
                          dput(as.character(names(.))), 
                          sep = ",", 
                          direction = "wide", 
                          type.convert = FALSE) %>%
  
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
  mutate(emerg_award = ifelse(award_num %like% "FFP-G", 1, 0),
         
         #Clean up award numbers for now, may require additional reformatting later
         award_num_clean = str_replace_all(award_num, 
                                           c("Grant No.: |Cooperative Agreement |Malawi CRS | "), 
                                           ""),
         enddate_days = enddate_revised - award_end,
         award_extended = ifelse(enddate_days > 0, 1, 0),
         award_num_clean = ifelse(award_num_clean == "FFP-A-13-00003-00", "AID-FFP-A-13-00003-00",
                                  ifelse(award_num_clean == "FFP-A-13-00004", "AID-FFP-A-13-00004", award_num_clean))) 


# Tidy dataframes for each dimension of data ------------------------------

activity_arr = data_wide %>% 
  mutate(total_mech_budget = (aglivhood_budget + assets_infra_budget + cond_uncondfood_budget + drr_budget + hiv_budget + mchn_budget + mkts_budget + wash_budget)) %>% 
  
  # Create a unique ID for each mechanism
  arrange(award_num_clean) %>% 
  mutate(award_id = seq_along(award_num_clean)) %>% 

  # Reshape by swinging the activities long as requested by customer
  gather(contains("activities"), key = activity_categ, value = activity) %>% 
  filter(!is.na(activity)) %>% 
  mutate(activity_categ = str_sub(activity_categ, 1, str_length(activity_categ) - 3)) %>% 
  group_by(award_num_clean) %>% 
  mutate(tot_mechanism_activities = n()) %>% 
  
  # Rearrange data and drop the subsector and approaches data -- this is contained in crosswalk
  select(award_num, project, contains("date"), everything()) %>% 
  select(-contains("subsectors"), -contains("approaches")) %>% 
  
  # Join in the crosswalk to populate the hierarchy requested
  left_join(., sector_labels, by = c("activity_categ")) %>% 
  arrange(award_num_clean)

  
# approach_arr = data_wide %>% 
#   gather(contains("approach"), key = approach_categ, value = approach) %>% 
#   filter(!is.na(approach)) %>% 
#   mutate(approach_categ = str_sub(approach_categ, 1, str_length(approach_categ) - 3)) %>% 
#   group_by(award_num_clean) %>% 
#   mutate(tot_approaches = n()) %>% 
#   select(contains("date"), contains("award"), everything())

# subsector_arr = data_wide %>% 
#   gather(contains("subsector"), key = subsector_categ, value = subsector) %>% 
#   filter(!is.na(subsector)) %>% 
#   mutate(subsector_categ = str_sub(subsector_categ, 1, str_length(subsector_categ) - 3)) %>% 
#   group_by(award_num_clean) %>% 
#   mutate(tot_sectors = n()) %>% 
#   select(award_num, project, contains("date"), everything()) 

budget_arr = data_wide %>% 
  select(-contains("activities"), -contains("approach"), -contains("subsector")) %>% 
  gather(contains("budget"), key = budget_categ, value = budget) %>% 
  select(award_num, project, contains("date"), everything()) %>% 
  group_by(award_num_clean) %>%  
  # Create some budget categories for flexibility
  mutate(award_budget = sum(budget)) %>% 
  group_by(country) %>% 
  mutate(country_budget = sum(budget)) %>% 
  group_by(budget_categ) %>% 
  mutate(categ_budget = sum(budget)) %>% 
  ungroup() %>% 
  mutate(award_categ_budget_sh = (budget / award_budget)) 

# check that budget shares sum to 1
budget_arr %>% group_by(award_num_clean) %>% summarise(sum(award_categ_budget_sh)) %>% kable()

base_arr = data_wide %>% 
  select(-contains("activities"), -contains("approach"), - contains("subsector")) %>% 
  select(award_num, project, contains("date"), everything())


# Write results to csv ----------------------------------------------------

# Create a named list of dataframes that will be saved as .csv for FFP analysts
datalist =list(activity_arr = activity_arr, 
     budget_arr = budget_arr,
     base_arr = base_arr) 

datalist %>%  
  names() %>% 
  map(., ~ write_csv(datalist[[.]], str_c(data_dir, ., ".csv")))
