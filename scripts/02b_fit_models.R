library(tictoc)
library(caret)
library(tidyverse)

years <- c(
  2020,  
  2000,
  rev(seq(1960, 1980, 10))
)

for (y in seq_along(years)) {
  
  load(paste0("data/train_test_",years[[y]],".Rdata"))
  source("functions/fit_ensembles.R")
  
}


