library(haven)
library(tidyverse)

scf <- read_dta("data/SCF_plus.dta")

inc_vars <- c("tinc","incws","incwsse", "inctrans","inccap")
debt_vars <- c("tdebt","pdebt","hdebt","ccdebt","othdebt","oestdebt")

other_asset_vars <- c(
  "ffaequ", # equity
  "ffabus", # business wealth
  "prepaid", # prepaid cards
  "liq", # checking, savings, money market, etc.  
  "moneymarketacc", # money market account
  "savbnd", #saving bond
  "cerde", # certificates of deposit
  "mfun", # mutual funds
  "oest", # other real estate
  "onfin", # other non-financial assets
  "ofin", # other financial assets
  "pen", # quasi-liquid retirement accounts
  "bnd", # bonds
  "life" # life insurance
  )

# flag for which households have imputed wealth values
impute_summary <- scf %>% 
  # aggregate to the household level
  group_by(id) %>%
  mutate_at(
    vars(
      ffanw, ffafin, ffanfin, 
      tdebt, pdebt, hdebt, 
      house, vehi,
      raceh
      ),
    list(
     impute_flag = ~ ifelse(. - dplyr::lead(., 1) != 0, 1, 0)
    )
  ) %>% 
  summarise_at(
    vars(matches("impute_flag$")),
    ~ mean(., na.rm = T)
  ) %>%
  ungroup
# ^ More than 50% of households have some wealth component which is imputed!  
  
  
# tidy data for modelling
scf_tidy <- scf %>% 
  # aggregate to the household level
  group_by(id) %>% 
  summarise_at(
    vars(
      ffanw, ffaass, ffafin, ffanfin, 
      vehi, # vehicles
      house, # real value of house
      all_of(inc_vars),
      all_of(debt_vars),
      all_of(other_asset_vars),
      wgt,
      wgtI95W95
    ),
    ~ mean(., na.rm = T)
  ) %>% 
  rename(
    net_wealth = ffanw,
    wealth_gross = ffaass,
    wealth_financial = ffafin,
    wealth_non_financial = ffanfin,
    debt_total = tdebt,
    
    wgt_original = wgt,
    wgt = wgtI95W95
  ) %>%
  ungroup


# keep hhld head values (as they are all repeated)
tmp <- scf %>%
  filter(impnum == 1) %>% 
  select(
    id,
    ageh, 
    year, 
    yearmerge,
    collegeh, 
    hhequiv,
    raceh
  ) %>% 
  mutate(
    collegeh = ifelse(collegeh == 1, "college", "no college"),
    raceh = ifelse(raceh == 0, "other", raceh),
    raceh = ifelse(raceh == "1", "black", raceh),
    raceh = ifelse(raceh == "2", "white", raceh)
  ) 

scf_tidy <- scf_tidy %>% 
  left_join(tmp) %>% 
  select(
    id,
    year,
    yearmerge, 
    everything()
  ) 

# add states
# scf_states <- haven::read_dta("data/SCF_States.dta")
# 
# scf_tidy <- scf_tidy %>% 
#   left_join(
#     scf_states %>% 
#       filter(impnum == 1) %>% 
#       select(
#         state,id
#       )
#   ) 

save(scf_tidy, file = "data/scf_tidy.Rdata")

rm(tmp,scf)
