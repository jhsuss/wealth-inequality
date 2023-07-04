years <- seq(1960,2020,10)
source("functions/impute_census.R")

for (y in seq_along(years)) {
  
  impute_census(years[[y]])
  
}

