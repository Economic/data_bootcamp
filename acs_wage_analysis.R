library(tidyverse)
library(haven)

# Load the ACS data from IPUMS
acs <- read_dta("/home/benzipperer/Downloads/acs_2019.dta")

# Clean up the data
acs_clean <- acs %>% 
  # keep only workers
  filter(incwage > 0 & incwage < 999998) %>% 
  filter(uhrswork > 0) %>% 
  # keep only full-time workers
  filter(wkswork2 == 6) %>% 
  # restrict analysis to VA
  filter(statefip == 51) %>% 
  # define wages and low-wage workers
  mutate(wage = incwage / (uhrswork * 51)) %>% 
  mutate(low_wage = wage <= 15)

# Do the analysis
acs_clean %>% 
  summarize(weighted.mean(low_wage, perwt))

# by gender
acs_clean %>%
  group_by(sex) %>% 
  summarize(weighted.mean(low_wage, perwt))

acs_clean %>%
  group_by(race) %>% 
  summarize(weighted.mean(low_wage, perwt))

