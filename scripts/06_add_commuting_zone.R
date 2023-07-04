library(tidyverse)

# load function which will do the crosswalk
source("functions/cz_crosswalk.R")

years <- seq(1960,2020,10)

for (r in seq_along(years)) {
  
  cz_crosswalk(year = years[[r]]) 
  
}
