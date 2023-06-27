library(tidyverse)

# load function which will do the computation
source("functions/cz_crosswalk.R")

# puma codes are different in each census file
years <- seq(1960,2020,10)

for (r in seq_along(years)) {
  year <- years[[r]]
  cz_crosswalk(year = year) # saves within function
}
