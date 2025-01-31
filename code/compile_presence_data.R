#--------------------------------------

# Script name: compile_presence_data.R

# Purpose: compile beaked whale & baleen whale presence results for FCBB area PAM sites

# Author: Joy Stanistreet

# Date created: 2025-01-23

#--------------------------------------

# load packages
library(tidyverse)
library(lubridate)
library(readxl)
library(here)

#--------------------------------------

# PART 1: COMPILE BEAKED WHALE PRESENCE DATA

# path to input data folder
input_folder <- here('data', 'beaked')

# create list of presence table files and extract deployment names
data_list = list.files(path = input_folder, pattern = "*.xlsx", full.names = T) 
depl_list = list.files(path = input_folder, pattern = "*.xlsx", full.names = F)
names(data_list)<-str_extract(depl_list, "^(?:[^_]+_){2}([^_]+)")

# read in all presence tables
beaked_data <- data_list %>% 
  map_df(~read_excel(.),.id = 'deployment')

# combine Me (in older tables) with MmMe (in newer tables)
beaked_data <- beaked_data %>% 
  mutate(Presence_MmMe = coalesce(Presence_MmMe, Presence_Me)) %>% 
  select(!Presence_Me)

# adjust column names
names(beaked_data) = sub(".*_","",names(beaked_data))

# species list
sp_list_beaked <- c('MmMe','Zc','Mb', 'Ha')

# organize data
daily_presence_beaked <- beaked_data  %>%
  
  # format deployment names
  mutate(deployment = str_replace_all(deployment, "_", "-")) %>% 
  
  # format species and presence columns
  pivot_longer(any_of(sp_list_beaked), names_to = 'species', values_to = 'presence') %>% 
  mutate(species = factor(species)) %>% 
  mutate(presence = replace(presence, is.na(presence), 0)) %>% 
  
  # parse dates 
  transmute(deployment, 
            rec_date = as_date(as.character(StartTime), format = "%Y%m%d_%H%M%S"),
            species,
            presence)

#--------------------------------------

# PART 2: COMPILE BALEEN WHALE PRESENCE DATA

# path to input data

input_folder_baleen <- here('data', 'baleen')

# create list of presence table files and extract deployment names
data_list_baleen = list.files(path = input_folder_baleen, pattern = "*.csv", full.names=T) 
depl_list_baleen = list.files(path = input_folder_baleen, pattern = "*.csv", full.names=F)
names(data_list_baleen)<-str_extract(depl_list_baleen, "^(?:[^_]+_){2}([^_]+)")

# read in all presence tables
baleen_data <- data_list_baleen %>% 
  map_df(~read_csv(.),.id = 'deployment')

# organize data
daily_presence_baleen <- baleen_data  %>%
  
  # format deployment names
  mutate(deployment = str_replace_all(deployment, "_", "-")) %>%
  
  # parse dates, drop callcat and calltype, format presence column
  transmute(deployment, 
            rec_date = as_date(detecdate),
            species = as_factor(species),
            presence) %>% 
  filter(presence == 'D') %>% 
  group_by(deployment, rec_date, species) %>% 
  summarize(presence = 1) %>% 
  ungroup()
  
#--------------------------------------

# PART 3: COMPILE EFFORT DATA

# path to input data
metadata_folder <- here('data', 'metadata')

# load stations table and filter
stations <- read_csv(here(metadata_folder, 'station_summary.csv')) %>% 
  filter(Revision == 1) %>% 
  mutate(station = Code)

# load missing data and expand missing dates for each deployment
missing_data <- read_csv(here(metadata_folder, 'missing_dates.csv')) %>% 
  group_by(deployment) %>% 
  mutate(start_missing = as_date(as.character(start_missing), format = '%Y%m%d'),
         end_missing = as_date(as.character(end_missing), format = '%Y%m%d')) %>% 
  rowwise() %>% 
  mutate(rec_date = list(seq(start_missing, 
                             end_missing, 
                             by = "1 day"))) %>% 
  unnest(cols = rec_date) %>% 
  ungroup() %>% 
  mutate(rec_effort = 0)

# load metadata and compile full table
depl_summary <- read_csv(here(metadata_folder,'fcbb_deployment_summary.csv'))

# create full species table
all_species <- tibble(species = as_factor(c(levels(daily_presence_baleen$species), sp_list_beaked)),
                      group = as_factor(c(rep("baleen", 6), rep("beaked", 4))))

# compile recording effort
effort <- depl_summary %>% 
  
  # remove deployment with no dataset
  drop_na('In-water_start') %>% 
  
  # parse dates
  transmute(deployment = Deployment,
            firstday = as_date(`In-water_start`, format = "%m/%d/%Y %H:%M")+1,
            lastday = as_date(`In-water_end`, format = "%m/%d/%Y %H:%M")-1) %>% 
  
  # add species & groups
  merge(all_species, all = T) %>% 
  
  # expand to all recording dates
  group_by(deployment, species, group) %>% 
  rowwise() %>% 
  mutate(rec_date = list(seq(firstday, lastday, by = '1 day'))) %>% 
  unnest(cols = rec_date) %>% 
  ungroup %>% 
  
  # add column to indicate missing data (recording effort = 0)
  left_join(missing_data, by = c('deployment', 'rec_date')) %>% 
  mutate(rec_effort = coalesce(rec_effort, 1)) %>% 
  
  # drop unneeded columns
  select(deployment, species, group, rec_date, rec_effort)

#--------------------------------------

# PART 4: COMBINE EFFORT & PRESENCE RESULTS

# combine baleen and beaked whale results
all_presence <- bind_rows(daily_presence_baleen, daily_presence_beaked)

# combine effort and presence data to create full dataset
all_data <- effort %>% 
  full_join(all_presence, by = c('deployment', 'species', 'rec_date')) %>% 
  mutate(presence = case_when(rec_effort == 0 ~ NA,
                              rec_effort == 1 & is.na(presence) ~ 0,
                              .default = presence)) %>% 
  
  # add station column and re-organize
  transmute(station = as_factor(str_extract(deployment, '[^-]+')),
            deployment = as_factor(deployment),
            group,
            species,
            rec_date,
            rec_effort,
            presence)

# save as RDS for use in other scripts
saveRDS(all_data, here('data', 'processed', 'presence_results.RDS'))


