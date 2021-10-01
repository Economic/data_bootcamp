library(tidyverse)
library(haven)

# this is my analysis to calculate shares of low wage workers
# this is a comment 

acs <- read_dta("/home/benzipperer/Downloads/acs_2019.dta")

acs_clean <- acs %>% 
  # keep only workers
  filter(incwage > 0 & incwage < 999998) %>% 
  filter(uhrswork > 0) %>% 
  filter(wkswork2 == 6) %>% 
  # restrict to Virginia
  filter(statefip == 51) %>% 
  # calculate wages
  mutate(wage = incwage / (uhrswork * 51)) %>% 
  mutate(low_wage = wage <= 15)

# summarize the data
acs_va_mean <- acs_clean %>% 
  summarize(weighted.mean(low_wage, perwt))
acs_va_mean

# how to calculate standard errors
# first download the "replication person weights" from IPUMS
# these will have names like repwtp*

# then use the survey package to define the survey structure
# svy <- as_survey(
#   acs,
#   weight = perwt,
#   repweights = matches("repwtp[0-9]+"),
#   type = "JK1",
#   scale = 4/80 ,
#   rscales = rep(1, 80),
#   mse = TRUE
# )
#
# then run the analysis, like
# svy %>%
#   summarize(med = survey_mean(low_wage))