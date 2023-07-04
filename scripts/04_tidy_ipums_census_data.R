library(tidyverse)
library(ipumsr)

years <- seq(1960,2020,10)
source("functions/clean_census.R")

for (y in seq_along(years)) {
  
  # load ipums data
  # https://usa.ipums.org/usa/
  # (2010 uses 3 year ACS centered on 2010)
  # (2020 uses 3 separate ACS years centered on 2020)
  
  # read relevant file from local driver
  file_path <- paste0("data/census_", year, ".xml")
  ddi <- read_ipums_ddi(file_path)
  data <- read_ipums_micro(ddi)
  
  clean_census(years[[y]])
  
}

