library(tidyverse)
library(haven)

# Load the ACS data from IPUMS
acs <- read_dta("acs_2018.dta")

#################################################
### RECODE ACS DATA FROM IPUMS, PARTICULARLY
### WEEKS WORKED, RACE/ETHNICITY VARIABLES
#################################################
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



############################################
### VIRGINIA SPECIFIC ANALYSIS:
### CREATE MULTIPLE SUMMARY STATISTICS AND
### STACK OVERALL AND RACE-SPECIFIC ANALYSIS
############################################
results_overall <- acs_clean %>% 
  filter(statefip == 51) %>% 
  summarize(
    low_wage_share = weighted.mean(low_wage, perwt),
    total_pop = sum(perwt),
    low_wage_pop = sum(low_wage * perwt),
    sample_size = n()
  )
results_race <- acs_clean %>% 
  filter(statefip == 51) %>% 
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



############################################
### COMPARE VA TO US
### JOINING DATA FROM THE LEFT AND RIGHT
############################################

# Overall US
results_race_us <- acs_clean %>%
  group_by(new_race) %>%
  summarize(low_wage_share = weighted.mean(low_wage, w = perwt)) 
results_overall_us <- acs_clean %>%
  summarize(low_wage_share = weighted.mean(low_wage, w = perwt))
results_us <- bind_rows(results_race_us, results_overall_us) %>% 
  mutate(new_race = ifelse(is.na(new_race), "Overall", new_race)) %>% 
  rename(US = low_wage_share)

# VA Only
results_race_va <- acs_clean %>%
  filter(statefip == 51) %>% 
  group_by(new_race) %>%
  summarize(low_wage_share = weighted.mean(low_wage, w = perwt))
results_overall_va <- acs_clean %>%
  filter(statefip == 51) %>% 
  summarize(low_wage_share = weighted.mean(low_wage, w = perwt))
results_va <- bind_rows(results_race_va, results_overall_va) %>% 
  mutate(new_race = ifelse(is.na(new_race), "Overall", new_race)) %>% 
  rename(VA = low_wage_share)

joined_results <- full_join(results_va, results_us, by = "new_race") %>% 
  rename(Group = new_race)

joined_results



############################################
### COMPARE VA AND NEARBY STATES
### RESHAPING THE DATA USING PIVOT_WIDER
############################################

results_nearby_states <- acs_clean %>%
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
  pivot_wider(id_cols = new_race, names_from = state_abb, values_from = low_wage_share) %>% 
  rename(Group = new_race)

results_nearby_states


############################################
### CREATE COUNTY-LEVEL ANALYSIS FOR VA
### USING PUMA IDENTIFIERS
############################################

# note this does not look good: read_csv("~/Downloads/geocorr2018.csv")
# use names() function to grab column names, then read in CSV, skip 2 lines
# also need puma to be numeric to match IPUMS data
geocorr_column_names <- names(read_csv("geocorr2018.csv"))
puma_county_mapping <- 
  read_csv("geocorr2018.csv", skip = 2, col_names = geocorr_column_names) %>%
  mutate(puma = as.numeric(puma12)) %>% 
  select(puma, 
         afact,
         county_fips = county14, 
         puma_name = PUMAname,
         county_name = cntyname2)

# The following dataset will duplicate observations 
# due to imperfect PUMA-County mapping
acs_county <- acs_clean %>% 
  filter(statefip == 51) %>% 
  full_join(puma_county_mapping, by = "puma")

# But if we scale weights proportional to allocation factors,
# we will recover the original estimates of low-wage workers

# estimate using original data
acs_clean %>% 
  filter(statefip == 51) %>%
  summarize(weighted.mean(low_wage, perwt))

# estimate using "duplicated" data
acs_county %>% 
  summarize(weighted.mean(low_wage, perwt*afact))

# Report results by county
results_county <- acs_county %>% 
  group_by(county_name) %>% 
  summarize(
    low_wage_share = weighted.mean(low_wage, w = afact * perwt), 
    total_pop = sum(perwt * afact),
    low_wage_pop = sum(low_wage * perwt * afact),
    sample_size_afact = sum(afact),
    sample_size_rows = n()
  ) 
results_county 

# note: could increase sample size by using 
# 5-year 2014-2018 ACS data rather than 1-year 2018 ACS
