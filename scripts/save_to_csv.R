library(tidyverse)

load( "wealth_inequality.Rdata")

ineq <- list(
  puma = ineq_puma %>% 
    select(year, puma, everything()) %>% 
    arrange(year, puma),
  cz = ineq_cz %>% 
    select(year, czone, everything()) %>% 
    arrange(year, czone),
  metarea = ineq_metarea %>% 
    select(year, metarea, everything()) %>% 
    arrange(year, metarea),
  state = ineq_state %>% 
    mutate(
      state = ifelse(year >= 2010, str_to_title(state), state)
    ) %>%
    select(year, state, everything()) %>% 
    arrange(year, state),
  division = ineq_region %>% 
    select(year, division = region_label, everything()) %>% 
    arrange(year, division)
  
)

# tidy / select only relevant columns
ineq <- ineq %>% 
  map(
    select,
    1, 2, 
    households,
    wealth_median = median,
    wealth_sd = sd,
    
    wealth_mean = mean,
    wealth_mean_lower = q2.5_Mean,
    wealth_mean_upper = q97.5_Mean,
    
    wealth_gini = gini,
    wealth_gini_lower = q2.5_gini,
    wealth_gini_upper = q97.5_gini,
    
    wealth_top1 = top1,
    wealth_top1_lower = q2.5_Top1,
    wealth_top1_upper = q97.5_Top1,
    
    wealth_top10 = top10,
    wealth_top10_lower = q2.5_Top10,
    wealth_top10_upper = q97.5_Top10,
    
    wealth_bottom50 = bottom50,
    wealth_bottom50_lower = q2.5_Bottom50,
    wealth_bottom50_upper = q97.5_Bottom50,
    
    nine_ten,
    nine_five,
    five_ten,
    
    incwage_mean,
    income_mean,
    own:own_mortgage,
    college:age
  ) %>% 
  map(
    mutate_if,
    is.numeric,
    ~ round(., digits = 3)
  )

# save to CSV
levels <- c("puma", "cz", "metarea", "state", "division")
for (i in seq_along(levels)) {
  write_csv(ineq[[i]], file = paste0(levels[[i]],"_wealth_inequality.csv"))
}
