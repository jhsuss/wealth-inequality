library(tidyverse)
library(ineq)
library(reldist)
library(boot)

# inequality and bootstrap functions
source("functions/inequality_measures.R")
source("functions/compute_inequality.R")

years <- seq(1960,2020,10)

for (y in seq_along(years)) {
  compute_inequality(years[[y]], short = F)
}

ineq_cz <- ineq_country <- ineq_region <- ineq_state <- ineq_metarea <- ineq_puma <- vector("list",length(years))
for (i in seq_along(years)) {
  load(
    paste0(
      "data/inequality_",years[[i]],"_full.Rdata"
      )
    )
  
  ineq_cz[[i]] <- ineq$czone
  ineq_state[[i]] <- ineq$state
  ineq_region[[i]] <- ineq$region_label
  ineq_country[[i]] <- ineq$country
  
  if (years[[i]] == 1960 | years[[i]] >= 1990) {
    ineq_puma[[i]] <- ineq$puma
  }
  
  if (years[[i]] != 1960) {
    ineq_metarea[[i]] <- ineq$metarea
  }
  
}

ineq_puma <- reduce(ineq_puma, bind_rows)
ineq_cz <- reduce(ineq_cz, bind_rows)
ineq_state <- reduce(ineq_state,bind_rows)
ineq_country <- reduce(ineq_country,bind_rows)
ineq_region <- reduce(ineq_region,bind_rows)
ineq_metarea <- reduce(ineq_metarea,bind_rows)


save(
  ineq_cz, ineq_country, ineq_state, 
  ineq_puma, ineq_region, ineq_metarea,
  file = "wealth_inequality.Rdata"
  )
