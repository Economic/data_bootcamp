library(tidyverse)
library(haven)

# load data from IPUMS
acs <- read_dta("/home/benzipperer/Downloads/acs_2018.dta")

acs_clean <- acs %>%
  # restrict to VA
  filter(statefip == 51) %>% 
  # assume average weeks worked within bin
  mutate(weeks_worked = case_when(
    wkswork2 == 0 ~ 0,
    wkswork2 == 1 ~ (1 + 13) / 2,
    wkswork2 == 2 ~ (14 + 26) / 2,
    wkswork2 == 3 ~ (27 + 39) / 2, 
    wkswork2 == 4 ~ (40 + 47) / 2,
    wkswork2 == 5 ~ (48 + 49) / 2,
    wkswork2 == 6 ~ (50 + 52) / 2
  )) %>% 
  # keep positive income
  filter(incwage > 0 & incwage < 999998) %>% 
  # keep workers
  filter(weeks_worked > 0 & uhrswork > 0) %>% 
  # calculate hourly wage
  mutate(wage = incwage / (weeks_worked * uhrswork)) %>% 
  # define low wage as under $15
  mutate(low_wage = wage <= 15)

# results
results_overall <- acs_clean %>% 
  summarize(weighted.mean(low_wage, w = perwt))

results_gender <- acs_clean %>% 
  group_by(sex) %>% 
  summarize(weighted.mean(low_wage, w = perwt))

results_race <- acs_clean %>% 
  group_by(race) %>% 
  summarize(weighted.mean(low_wage, w = perwt))


