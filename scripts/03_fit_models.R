####' Loop through years (varying sets of variables available) to fit predictive models  
####' Caution: this can take a considerable amount of time (especially for some models, e.g. SVM)
####' Will save models in 'models/' sub-directory

years <- c(
  seq(1960, 1980, 10),
  2000,
  2020
)

for (y in seq_along(years)) {
  
  load(paste0("data/train_test_",years[[y]],".Rdata"))
  source("functions/fit_ensembles.R")
  
}


