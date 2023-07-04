
# script to define and save train and test samples 

library(tidyverse)
library(tictoc)
load("data/scf_tidy_full.Rdata")
source("scripts/census_var_availability.R")

years <- c(
  2020,  
  2000,
  rev(seq(1940, 1980, 10))
  )

for (y in seq_along(years)) {
  
  # keep post 1989 data only
  df <- scf_tidy %>% 
    filter(year >= 1989) %>% 
    rename(
      mortamt1 = paymort1,
      mortamt2 = paymort2
    )
  
  vars_to_keep <- census_vars %>% 
    filter(year == years[[y]]) %>% 
    select_if(. == T) %>% 
    names
  
  # category changes pre 1990
  if (years[[y]] < 1990) {
    df <- df %>% 
      mutate_at(
        vars(matches("^educ")),
        ~ case_when(
          str_detect(., "high-school") ~ "no college",
          str_detect(., "college|master") ~ "college",
          TRUE ~ "inappropriate"
          )
      )
  }
  
  # 1980 mortgage payments are amalgamated into 1 entry
  if (years[[y]] == 1980) {
    
    df <- df %>%
      rowwise() %>% 
      mutate(
        mortotal = sum(c(mortamt1, mortamt2), na.rm = T),
        incother = sum(c(incother,incretir), na.rm = T)
      ) %>% 
      ungroup
  
  # amalgamate income 
  } else if (years[[y]] == 1970) {
    
    df <- df %>%
      rowwise() %>% 
      mutate(
        incother = sum(c(incother,incretir,incinvst), na.rm = T),
        house_tenure = ifelse(str_detect(house_tenure,"^own"),"own","rent")
      ) %>% 
      ungroup
    
  } else if (years[[y]] == 1960 | years[[y]] == 1950) {
    
    df <- df %>%
      rowwise() %>% 
      mutate(
        incother = sum(c(incother,incretir,incinvst,incss,incwelfr), na.rm = T),
        house_tenure = ifelse(str_detect(house_tenure,"^own"),"own","rent")
      ) %>% 
      ungroup
    
  } else if (years[[y]] == 1940) {
    
    df <- df %>%
      rowwise() %>% 
      mutate(
        incnonwg = sum(c(incbus:incother), na.rm = T),
        incnonwg = ifelse(incnonwg >= 50, "50+", "less_than_50"),
        house_tenure = ifelse(str_detect(house_tenure,"^own"),"own","rent")
      ) %>% 
      ungroup
    
  }
  
  
  df <- df %>%
    select(
      net_wealth,
      have_wealth,
      
      wealth_gross,
      debt_total,
      
      all_of(vars_to_keep),
      all_of(vars_always_available),
      any_of(c("mortotal","incnonwg"))
    ) %>% 
    mutate(
      #have_wealth = ifelse(net_wealth <= 10000, "no", "yes")
    ) 
  
  # specify train,val, test  
  set.seed(180*25)
  ind <- sample(seq_len(nrow(df)), size = floor(0.8 * nrow(df)))
  train_df <- df[ind,] %>% na.omit
  test_df <- df[-ind,] %>% na.omit
  
  #set.seed(18240)
  val_ind <- sample(seq_len(nrow(test_df)), size = floor(0.5 * nrow(test_df)))
  val_df <- test_df[val_ind,]
  test_df <- test_df[-val_ind,]
  
  rm(ind,val_ind)
  
  save(train_df, val_df, test_df, file = paste0("data/train_test_",years[[y]],".Rdata"))
  
}

