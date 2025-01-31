
# Convert hourly BWD presence table to daily

library(tidyverse)
library(here)
library(readxl)
library(openxlsx2)

input_folder <- 'R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_4/PAM_analysis/DFO_MAR/FCH_2018_09'

#hourly <- read_excel(here(input_folder, 'GBK_2019_10_Beaked_Presence_hourly_final.xlsx'))
hourly <- read_excel(here(input_folder, 'FCH_2018_09_Beaked_Presence_perHour.xlsx'))

hourly <- hourly %>% 
  mutate(rec_date = as_date(as.character(StartTime), 
                     format = "%Y%m%d_%H%M%S")) %>% 
  
  # replace -1 with 0.5 for unknowns
  mutate(across(c(Presence_Me, Presence_Zc, Presence_Mb),
                ~ ifelse(.x == -1, 0.5, .x))) %>%
  
  # get daily presence
  group_by(rec_date) %>% 
  summarize(across(c(Presence_Me, Presence_Zc, Presence_Mb), max)) %>% 
  
  # replace 0.5 with -1 for unknowns
  mutate(across(c(Presence_Me, Presence_Zc, Presence_Mb),
                ~ ifelse(.x == 0.5, -1, .x))) %>%

  # reformat as presence table
  mutate(StartTime = paste0(format(rec_date, '%Y%m%d'), '_000000')) %>% 
  mutate(EndTime = paste0(format(rec_date + 1, '%Y%m%d'), '_000000')) %>% 
  
  transmute(StartTime, EndTime, Presence_Me, Presence_Zc, Presence_Mb)
            #Presence_Ha = 0)

# write excel file

write_xlsx(hourly, here(input_folder, 'FCH_2018_09_Beaked_Presence.xlsx'))


###

# combine two presence tables

pt1 <- read_excel(here(input_folder, 'GBK_2019_10_Beaked_Presence_daily1.xlsx'))
pt2 <- read_excel(here(input_folder, 'GBK_2019_10_Beaked_Presence_daily_final.xlsx'))

final <- pt1 %>% 
  bind_rows(pt2)

write_xlsx(final, here(input_folder, 'GBK_2019_10_Beaked_Presence.xlsx'))

