library(tidyverse)

# keep vars that are always available in the census
vars_always_available <- c(
  "year",
  "household_size",
  
  "sex",
  "age",
  "educ_hrp",
  "educ_partner",
  "race",
  
  "married_full",
  
  "occ_hrp", "occ_partner",
  "ind_hrp", "ind_partner",
  
  "work_status", "work_status_partner",
  "classwkr", "classwkr_partner"
)

census_vars <- tribble(
  ~ year, ~ house_value,
  2020,  T,
  2010,  T,
  2000,  T,
  1990,  T,
  1980,  T,
  1970,  T,
  1960,  T,
  1950,  F,
  1940,  T,
  1930,  T
)

census_vars <- census_vars %>% 
  mutate(
    house_tenure = ifelse(year == 1950, F, T), 
    
    mortamt1 = ifelse(year < 1990, F, T),
    mortamt2 = ifelse(year < 1990, F, T),
    
    taxincl = ifelse(year < 1980, F, T),
    insincl = ifelse(year < 1980, F, T),
    proptx = ifelse(year < 1990, F, T),
    rent = ifelse(year == 1950 | year <= 1930, F, T),
    
    incwage = ifelse(year < 1940, F, T),
    incbus = ifelse(year < 1950, F, T),
    incss = ifelse(year < 1970, F, T),
    incwelfr = ifelse(year < 1970, F, T),
    incinvst = ifelse(year < 1980, F, T),
    incretir = ifelse(year < 1990, F, T),
    incother = ifelse(year < 1950, F, T),
    
    nvehic = ifelse(year < 1990, F, T),
    
    hcovany = ifelse(year < 2010, F, T),
    
    uhrswork = ifelse(year < 1980, F, T),
    uhrswork_partner = ifelse(year < 1980, F, T),
    wkswork = ifelse(year < 1940, F, T),
    wkswork_partner = ifelse(year < 1940, F, T)
    
  )

save(census_vars, vars_always_available, file = "data/census_var_availability.Rdata")
