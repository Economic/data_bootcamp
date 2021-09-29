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
  mutate(weeks_worked = case_when(
    wkswork2 == 0 ~ 0,
    wkswork2 == 1 ~ (1 + 13) / 2,
    wkswork2 == 2 ~ (14 + 26) / 2,
    wkswork2 == 3 ~ (27 + 39) / 2,
    wkswork2 == 4 ~ (40 + 47) / 2,
    wkswork2 == 5 ~ (48 + 49) / 2,
    wkswork2 == 6 ~ (50 + 52) / 2
  )) %>% 
  # define wages and low-wage workers
  mutate(wage = incwage / (uhrswork * weeks_worked)) %>% 
  mutate(low_wage = wage <= 15) %>% 
  mutate(new_hispanic = ifelse(hispan >= 1 & hispan <= 4, 1, 0)) %>% 
  mutate(new_race = case_when(
    new_hispanic == 1 ~ "Hispanic",
    race == 1 ~ "White",
    race == 2 ~ "Black",
    race == 3 ~ "Other",
    race >= 4 & race <= 6 ~ "Asian",
    race >= 7 ~ "Other"
  ))

### STACK OVERALL AND RACE-SPECIFIC ANALYSIS
results_overall <- acs_clean %>% 
  summarize(
    low_wage_share = weighted.mean(low_wage, perwt),
    total_pop = sum(perwt),
    low_wage_pop = sum(low_wage * perwt),
    sample_size = n()
  )
results_race <- acs_clean %>% 
  group_by(new_race) %>% 
  summarize(
    low_wage_share = weighted.mean(low_wage, perwt),
    total_pop = sum(perwt),
    low_wage_pop = sum(low_wage * perwt),
    sample_size = n()
  )

stacked_results <- bind_rows(results_race, results_overall) %>% 
  mutate(new_race = ifelse(!is.na(new_race), new_race, "Overall"))
stacked_results


results_overall_states <- acs_clean %>%
  mutate(state_abb = case_when(
    statefip == 21 ~ "KY",
    statefip == 24 ~ "MD",
    statefip == 37 ~ "NC",
    statefip == 51 ~ "VA",
    statefip == 54 ~ "WV",
    statefip == 47 ~ "TN"
  )) %>% 
  filter(!is.na(state_abb)) %>% 
  group_by(state_abb, new_race) %>% 
  summarize(
    low_wage_share = weighted.mean(low_wage, perwt)
  ) %>% 
  pivot_wider(id_cols = new_race, names_from = state_abb, values_from = low_wage_share)






