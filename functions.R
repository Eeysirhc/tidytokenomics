# Author: Eeysirhc
# Date written: 2022-02-15
# Last updated: 2022-04-17

# load libraries
library(tidyverse)
library(lubridate)
library(scales)

options(scipen = 100)


# create base data layer
tokenomics_layer <- function(category, token_allocation, percent_total_supply, price_per_token, percent_tge_issuance, emission_frequency, first_emission_day, last_emission_day){
  
  df <- tribble(
    ~category, ~token_allocation, ~percent_total_supply, ~price_per_token, ~percent_tge_issuance, ~emission_frequency, ~first_emission_day, ~last_emission_day,
    category, token_allocation, percent_total_supply, price_per_token, percent_tge_issuance, emission_frequency, first_emission_day, last_emission_day) %>% 
    unnest(col = c(category, token_allocation, percent_total_supply, price_per_token, percent_tge_issuance, emission_frequency, first_emission_day, last_emission_day)) %>% 
    mutate(number_of_tokens = total_supply * percent_total_supply) %>% 
    mutate(total_value = number_of_tokens * price_per_token) %>% 
    mutate(tge_issuance_tokens = number_of_tokens * percent_tge_issuance) %>% 
    mutate(standardized_emission = (number_of_tokens - tge_issuance_tokens) / last_emission_day)
  
  df <- df %>% 
    select(category, token_allocation, number_of_tokens, percent_total_supply, price_per_token, total_value, percent_tge_issuance, tge_issuance_tokens, emission_frequency, standardized_emission, first_emission_day, last_emission_day)
  
  return(df)
  
}


# simulate tokenomics data
tokenomics_simulate <- function(category, token_allocation, tge_issuance_tokens, standardized_emission, emission_frequency, first_emission_day, last_emission_day){
  
  df <- seq(1, outlook_years * 365, 1) %>% 
    as_tibble() %>% 
    rename(day = value) %>% 
    
    mutate(category = category, 
           token_allocation = token_allocation) %>% 
    
    mutate(tokens = case_when(
      
      day == 1 & tge_issuance_tokens > 0 ~ tge_issuance_tokens,
      emission_frequency == "-" & day == first_emission_day ~ tge_issuance_tokens,
      emission_frequency == "Daily" & day%%1 == 0 & day >= first_emission_day & day <= last_emission_day ~ standardized_emission,
      emission_frequency == "Monthly" & day%%30 == 0 & day >= first_emission_day & day <= last_emission_day ~ (standardized_emission * 30),
      emission_frequency == "Quarterly" & day%%90 == 0 & day >= first_emission_day & day <= last_emission_day ~ (standardized_emission * 90),
      day == 0 ~ tge_issuance_tokens,
      TRUE ~ 0 )) %>% 
    
    mutate(tokens_supply = cumsum(tokens))
  
  return(df)
  
}


