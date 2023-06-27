#####################################
###### SCRIPT TO TRAIN MODELS #######
#####################################

library(tictoc)
library(caret)
library(tidyverse)

path <- "ADD_PATH"

# method for choosing hyperparameters
my_control <- trainControl(
  method = "cv",
  number = 5,
  allowParallel = T,
  search = "random",
  classProbs = T,
  savePredictions = "final",
  sampling = "up", # upsample minority class
  summaryFunction = twoClassSummary # metric = AUC
)

ctrl <- trainControl(
  method = "cv",
  number = 5,
  allowParallel = T,
  search = "random",
  savePredictions = "final"
)

# sample for positive and negative models
train_pos <- train_df %>%
  filter(
    have_wealth == "yes"
  ) %>%
  select(
    -have_wealth
  ) %>%
  mutate(net_wealth = log(net_wealth)) %>%
  na.omit 

train_0 <- train_df %>% 
  filter(net_wealth <= 0) %>% 
  mutate(have_wealth = ifelse(net_wealth == 0, "zero","negative") )

train_neg <- train_df %>%
  filter(
    net_wealth < 0
    ) %>%
  mutate(net_wealth = asinh(net_wealth)) %>%
  na.omit 


wealth_debt <- c("wealth_gross","debt_total")


# set seed to reproduce models
set.seed(2828*years[[y]])

##### BINARY #######

## RF
tic("rf done")
mod_rf_bin <- train(
  y = train_df$have_wealth,
  x = train_df %>% select(-have_wealth, -net_wealth, -any_of(wealth_debt)), 
  trControl = my_control,
  method = "ranger", 
  tuneLength = 10, 
  keep.inbag = TRUE,
  importance = "impurity"
)
save(mod_rf_bin, file =  paste0("models/mod_rf_bin_",years[[y]],".Rdata"))
toc()


## SVM
tic("svm done")
mod_svm_bin <- train(
  have_wealth ~ ., 
  data = train_df %>% 
    select(-net_wealth,-any_of(wealth_debt)),
  trControl = my_control,
  method = "svmRadial",
  tuneLength = 10,
  preProcess = c("scale","center")
)
save(mod_svm_bin, file =  paste0("models/mod_svm_bin_",years[[y]],".Rdata"))
toc()


## KNN
tic("knn done")
mod_knn_bin <- train(
  have_wealth ~ .,
  data = train_df %>%
    select(-net_wealth,-any_of(wealth_debt)),
  trControl = trainControl(
    method = "cv",
    number = 5,
    allowParallel = T,
    classProbs = T,
    savePredictions = "final",
    sampling = "up", # upsample minority class
    summaryFunction = twoClassSummary # metric = AUC
  ),
  method = "knn", 
  tuneGrid = expand.grid(
    k = c(5,25,100,250)
    ),
  preProcess = c("scale","center")
)
save(mod_knn_bin, file =  paste0(path,"models/mod_knn_bin_",years[[y]],".Rdata"))
toc()

## Regression models
mod_glm_bin <- train(
  have_wealth ~ ., 
  data = train_df %>% select(-net_wealth,-any_of(wealth_debt)),
  trControl = my_control,
  method = "glm"
)

mod_lasso_bin <- train(
  have_wealth ~ .,
  data = train_df %>% select(-net_wealth,-any_of(wealth_debt)),
  trControl = my_control,
  method = "glmnet",
  tuneLength = 10
)

save(mod_glm_bin, mod_lasso_bin, file = paste0("models/mod_glm_bin_",years[[y]],".Rdata"))

## Boosted trees
tic("xgb done")
mod_xgb_bin <- train(
  have_wealth ~ ., 
  data = train_df %>% 
    select(-net_wealth,-any_of(wealth_debt)),
  trControl = my_control,
  method="xgbDART",
  tuneLength = 10
)
save(mod_xgb_bin, file = paste0("models/mod_xgb_bin_",years[[y]],".Rdata"))
toc()

## NEURAL NET
tic("net done")
mod_net_bin <- train(
  have_wealth ~ .,
  data = train_df %>% 
    select(-net_wealth, -any_of(wealth_debt)),
  method = "nnet",
  trControl = my_control,
  tuneLength = 10,
  preProcess = c("scale","center"),
  MaxNWts = 2000,
  maxit = 200, 
  trace = F
)

save(mod_net_bin, file = paste0("models/mod_net_bin_",years[[y]],".Rdata"))
toc()


## FIT ensembles
tic("binary ensemble done")
preds_svm_bin <- predict(mod_svm_bin, val_df, type = "prob")
preds_knn_bin <- predict(mod_knn_bin, val_df, type = "prob")
preds_rf_bin <- predict(mod_rf_bin, val_df, type = "prob")
preds_xgb_bin <- predict(mod_xgb_bin, val_df, type = "prob")
preds_net_bin <- predict(mod_net_bin, val_df, type = "prob")
preds_glm_bin <- predict(mod_glm_bin, val_df, type = "prob")
preds_lasso_bin <- predict(mod_lasso_bin, val_df, type = "prob")


preds_val <- tibble(
  y = val_df$have_wealth,
  preds_svm_bin = preds_svm_bin$yes,
  preds_knn_bin = preds_knn_bin$yes,
  preds_rf_bin = preds_rf_bin$yes,
  preds_xgb_bin = preds_xgb_bin$yes,
  preds_net_bin = preds_net_bin$yes,
  preds_glm_bin = preds_glm_bin$yes,
  preds_lasso_bin = preds_lasso_bin$yes
)

mod_ens_lm_bin <- train(
  y ~ ., 
  data = preds_val,
  method = "glm",
  metric = "Kappa"
)

save(
  mod_ens_lm_bin, 
  file = paste0("models/mod_ensemble_bin_",years[[y]],".Rdata")
  
)
toc()


####################
##### POSITIVE #####
####################

tic("rf pos done")
mod_rf_pos <- train(
  y = train_pos$net_wealth,
  x = train_pos %>% select(-net_wealth,-any_of(wealth_debt)),
  method = "ranger",
  trControl = ctrl,
  tuneLength = 10,
  keep.inbag = TRUE,
  importance = "impurity"
)
save(mod_rf_pos, file = paste0("models/mod_rf_pos_",years[[y]],".Rdata"))
toc()


## NEURAL NET
tic("net pos done")
mod_net_pos <- train(
  net_wealth ~ .,
  data = train_pos %>% 
    select(-any_of(wealth_debt)),
  method = "nnet",
  trControl = ctrl,
  tuneLength = 10,
  preProcess = c("scale","center"),
  maxit = 200, # bump up max iterations from 100 to 200
  MaxNWts = 2000,
  linout = T,
  trace = F
)

save(mod_net_pos, file = paste0("models/mod_net_pos_",years[[y]],".Rdata"))
toc()

## Boosted trees
tic("xgb pos done")
mod_xgb_pos <- train(
  net_wealth ~ ., 
  data = train_pos %>% select(-any_of(wealth_debt)),
  method="xgbDART",
  trControl = ctrl,
  tuneLength = 10
)
save(mod_xgb_pos, file = paste0("models/mod_xgb_pos_",years[[y]],".Rdata"))
toc()

## SVM
tic("svm pos done")
mod_svm_pos <- train(
  net_wealth ~ ., 
  data = train_pos %>% 
    select(-any_of(wealth_debt)),
  trControl = ctrl,
  method = "svmRadial",
  tuneLength = 10,
  preProcess = c("scale","center")
)
save(mod_svm_pos, file =  paste0("models/mod_svm_pos_",years[[y]],".Rdata"))
toc()

tic("knn pos done")
mod_knn_pos <- train(
  net_wealth ~ .,
  data = train_pos %>%
    select(-any_of(wealth_debt)),
  trControl = trainControl(
    method = "cv",
    number = 5,
    allowParallel = T,
    savePredictions = "final"
  ),
  method = "knn",
  tuneGrid = expand.grid(
    k = c(5,25,100,250)
  ),
  preProcess = c("scale","center")
)
save(mod_knn_pos, file =  paste0(path,"models/mod_knn_pos_",years[[y]],".Rdata"))
toc()

## Regression
mod_glm_pos <- train(
  net_wealth ~ ., 
  data = train_pos %>% select(-any_of(wealth_debt)),
  trControl = ctrl,
  method = "glm"
)

mod_lasso_pos <- train(
  net_wealth ~ .,
  data = train_pos %>% select(-any_of(wealth_debt)),
  trControl = ctrl,
  method = "glmnet",
  tuneLength = 10
)

save(mod_glm_pos, mod_lasso_pos, file = paste0("models/mod_glm_pos_",years[[y]],".Rdata"))

val_pos <- val_df %>% filter(net_wealth > 0)
preds_svm_pos <- predict(mod_svm_pos, val_pos)
preds_knn_pos <- predict(mod_knn_pos, val_pos)
preds_rf_pos <- predict(mod_rf_pos, val_pos)
preds_xgb_pos <- predict(mod_xgb_pos, val_pos)
preds_net_pos <- predict(mod_net_pos, val_pos)
preds_glm_pos <- predict(mod_glm_pos, val_pos)
preds_lasso_pos <- predict(mod_lasso_pos, val_pos)


preds_val <- tibble(
  y = log(val_pos$net_wealth),
  preds_svm_pos,
  preds_knn_pos,
  preds_rf_pos,
  preds_xgb_pos,
  preds_net_pos,
  preds_glm_pos,
  preds_lasso_pos
)

mod_ens_lm_pos <- train(
  y ~ ., 
  data = preds_val,
  method = "lm",
  trControl = ctrl
  )

save(
  mod_ens_lm_pos, 
  file = paste0("models/mod_ensemble_pos_",years[[y]],".Rdata")
)


##### ZERO/NEG binary
tic("rf zero done")
mod_rf_zero <- train(
  y = train_0$have_wealth,
  x = train_0 %>% select(-net_wealth, -have_wealth, -any_of(wealth_debt)),
  trControl = my_control,
  method = "ranger", 
  tuneLength = 10, 
  keep.inbag = TRUE,
  importance = "impurity"
)
save(mod_rf_zero, file = paste0("models/mod_rf_zero_",years[[y]],".Rdata"))
toc()


## Boosted trees
tic("xgb done")
mod_xgb_zero <- train(
  have_wealth ~ ., 
  data = train_0 %>% select(-net_wealth,-any_of(wealth_debt)),
  trControl = my_control,
  method="xgbDART",
  tuneLength = 10
  
)
save(mod_xgb_zero, file = paste0("models/mod_xgb_zero_",years[[y]],".Rdata"))
toc()

## NEURAL NET
tic("net done")
mod_net_zero <- train(
  have_wealth ~ .,
  data = train_0 %>% 
    select(-net_wealth, -any_of(wealth_debt)),
  method = "nnet",
  trControl = my_control,
  tuneLength = 10,
  preProcess = c("scale","center"),
  MaxNWts = 2000,
  maxit = 200, 
  trace = F
)

save(mod_net_zero, file = paste0("models/mod_net_zero_",years[[y]],".Rdata"))
toc()

tic("svm zero done")
mod_svm_zero <- train(
  have_wealth ~ ., 
  data = train_0 %>% 
    select(-net_wealth,-any_of(wealth_debt)),
  trControl = my_control,
  method = "svmRadial",
  tuneLength = 10,
  preProcess = c("scale","center")
)
save(mod_svm_zero, file =  paste0("models/mod_svm_zero_",years[[y]],".Rdata"))
toc()


## KNN
tic("knn done")
mod_knn_zero <- train(
  have_wealth ~ .,
  data = train_0 %>%
    select(-net_wealth,-any_of(wealth_debt)),
  trControl = trainControl(
    method = "cv",
    number = 5,
    allowParallel = T,
    classProbs = T,
    savePredictions = "final",
    sampling = "up", # upsample minority class
    summaryFunction = twoClassSummary # metric = AUC
  ),
  method = "knn",
  tuneGrid = expand.grid(
    k = c(5,25,100,250)
  ),
  preProcess = c("scale","center")
)
save(mod_knn_zero, file =  paste0(path,"models/mod_knn_zero_",years[[y]],".Rdata"))
toc()

## Regression models
mod_glm_zero <- train(
  have_wealth ~ ., 
  data = train_0 %>% select(-net_wealth,-any_of(wealth_debt)),
  trControl = my_control,
  method = "glm"
)

mod_lasso_zero <- train(
  have_wealth ~ .,
  data = train_0 %>% select(-net_wealth,-any_of(wealth_debt)),
  trControl = my_control,
  method = "glmnet",
  tuneLength = 10
)

save(mod_glm_zero, mod_lasso_zero, file = paste0("models/mod_glm_zero_",years[[y]],".Rdata"))


## FIT ensembles
val_0 <- val_df %>% 
  filter(net_wealth <= 0) %>% 
  mutate(have_wealth = ifelse(net_wealth == 0, "zero","negative") )

tic("binary ensemble done")
preds_svm_zero <- predict(mod_svm_zero, val_0, type = "prob")
preds_knn_zero <- predict(mod_knn_zero, val_0, type = "prob")
preds_rf_zero <- predict(mod_rf_zero, val_0, type = "prob")
preds_xgb_zero <- predict(mod_xgb_zero, val_0, type = "prob")
preds_net_zero <- predict(mod_net_zero, val_0, type = "prob")
preds_glm_zero <- predict(mod_glm_zero, val_0, type = "prob")
preds_lasso_zero <- predict(mod_lasso_zero, val_0, type = "prob")


preds_val <- tibble(
  y = val_0$have_wealth,
  preds_svm_zero = preds_svm_zero$zero,
  preds_knn_zero = preds_knn_zero$zero,
  preds_rf_zero = preds_rf_zero$zero,
  preds_xgb_zero = preds_xgb_zero$zero,
  preds_net_zero = preds_net_zero$zero,
  preds_glm_zero = preds_glm_zero$zero,
  preds_lasso_zero = preds_lasso_zero$zero
)

mod_ens_lm_zero <- train(
  y ~ ., 
  data = preds_val,
  method = "glm",
  metric = "Kappa"
)

save(
  mod_ens_lm_zero, 
  file = paste0("models/mod_ensemble_zero_",years[[y]],".Rdata")
  
)

#####################
##### NEGATIVE WEALTH
#####################


tic("rf neg done")
mod_rf_neg <- train(
  y = train_neg$net_wealth,
  x = train_neg %>% select(-net_wealth, -have_wealth, -any_of(wealth_debt)),
  method = "ranger",
  trControl = ctrl,
  tuneLength = 10,
  keep.inbag = TRUE,
  importance = "impurity"
)

save(mod_rf_neg, file = paste0("models/mod_rf_neg_",years[[y]],".Rdata"))
toc()


## NEURAL NET
tic("net neg done")
mod_net_neg <- train(
  net_wealth ~ .,
  data = train_neg %>% 
    select(-any_of(wealth_debt),-have_wealth),
  method = "nnet",
  trControl = ctrl,
  tuneLength = 10,
  preProcess = c("scale","center"),
  maxit = 200, 
  MaxNWts = 2000,
  linout = T,
  trace = F
)

save(mod_net_neg, file = paste0("models/mod_net_neg_",years[[y]],".Rdata"))
toc()

## Boosted trees
tic("xgb neg done")
mod_xgb_neg <- train(
  net_wealth ~ ., 
  data = train_neg %>% select(-any_of(wealth_debt),-have_wealth),
  trControl =  ctrl, 
  method="xgbDART",
  tuneLength = 10
)
save(mod_xgb_neg, file = paste0("models/mod_xgb_neg_",years[[y]],".Rdata"))
toc()

tic("svm neg done")
mod_svm_neg <- train(
  net_wealth ~ ., 
  data = train_neg %>% 
    select(-any_of(wealth_debt),-have_wealth),
  trControl = ctrl,
  method = "svmRadial",
  tuneLength = 10,
  preProcess = c("scale","center")
)
save(mod_svm_neg, file =  paste0("models/mod_svm_neg_",years[[y]],".Rdata"))
toc()



tic("knn neg done")
mod_knn_neg <- train(
  net_wealth ~ .,
  data = train_neg %>%
    select(-any_of(wealth_debt),-have_wealth),
  trControl = trainControl(
    method = "cv",
    number = 5,
    allowParallel = T,
    savePredictions = "final"
  ),
  method = "knn", 
  tuneGrid = expand.grid(
    k = c(5,25,100,250)
    ),
  preProcess = c("scale","center")
)
save(mod_knn_neg, file =  paste0(path,"models/mod_knn_neg_",years[[y]],".Rdata"))
toc()

## Regression
mod_glm_neg <- train(
  net_wealth ~ ., 
  data = train_neg %>% select(-any_of(wealth_debt),-have_wealth),
  trControl = ctrl,
  method = "glm"
)

mod_lasso_neg <- train(
  net_wealth ~ .,
  data = train_neg %>% select(-any_of(wealth_debt),-have_wealth),
  trControl = ctrl,
  method = "glmnet",
  tuneLength = 10
)

save(mod_glm_neg, mod_lasso_neg, file = paste0("models/mod_glm_neg_",years[[y]],".Rdata"))


val_neg <- val_df %>% filter(net_wealth < 0)
preds_svm_neg <- predict(mod_svm_neg, val_neg)
preds_knn_neg <- predict(mod_knn_neg, val_neg)
preds_rf_neg <- predict(mod_rf_neg, val_neg)
preds_xgb_neg <- predict(mod_xgb_neg, val_neg)
preds_net_neg <- predict(mod_net_neg, val_neg)
preds_glm_neg <- predict(mod_glm_neg, val_neg)
preds_lasso_neg <- predict(mod_lasso_neg, val_neg)


preds_val <- tibble(
  y = asinh(val_neg$net_wealth),
  preds_svm_neg,
  preds_knn_neg,
  preds_rf_neg,
  preds_xgb_neg,
  preds_net_neg,
  preds_glm_neg,
  preds_lasso_neg
)

mod_ens_lm_neg <- train(
  y ~ ., 
  data = preds_val,
  method = "lm",
  trControl = ctrl
)

save(
  mod_ens_lm_neg, 
  file = paste0("models/mod_ensemble_neg_",years[[y]],".Rdata")
)

##### INVERSE HYPERBOLIC SINE
tic("rf ihs done")
mod_rf_ihs <- train(
  y = asinh(train_df$net_wealth),
  x = train_df %>% select(-net_wealth,-have_wealth, -any_of(wealth_debt)),
  method = "ranger",
  trControl = ctrl,
  tuneLength = 10,
  keep.inbag = TRUE,
  importance = "impurity"
)

save(mod_rf_ihs, file = paste0("models/mod_rf_ihs_",years[[y]],".Rdata"))
toc()

## NEURAL NET
tic("net ihs done")
mod_net_ihs <- train(
  asinh(net_wealth) ~ .,
  data = train_df %>% 
    select(-any_of(wealth_debt),-have_wealth),
  method = "nnet",
  trControl = ctrl,
  tuneLength = 10,
  preProcess = c("scale","center"),
  maxit = 200, 
  MaxNWts = 2000,
  linout = T,
  trace = F
)

save(mod_net_ihs, file = paste0("models/mod_net_ihs_",years[[y]],".Rdata"))
toc()

## Boosted trees
tic("xgb ihs done")
mod_xgb_ihs <- train(
  asinh(net_wealth) ~ ., 
  data = train_df %>% select(-any_of(wealth_debt),-have_wealth),
  method="xgbDART",
  trControl = ctrl,
  tuneLength = 10
  )
save(mod_xgb_ihs, file = paste0("models/mod_xgb_ihs_",years[[y]],".Rdata"))
toc()

mod_glm_ihs <- train(
  asinh(net_wealth) ~ ., 
  data = train_df %>% select(-any_of(wealth_debt), -have_wealth),
  trControl = ctrl,
  method = "glm"
)

mod_lasso_ihs <- train(
  asinh(net_wealth) ~ .,
  data = train_df %>% select(-any_of(wealth_debt), -have_wealth),
  trControl = ctrl,
  method = "glmnet",
  tuneLength = 10
)

save(mod_glm_ihs, mod_lasso_ihs, file = paste0("models/mod_glm_ihs_",years[[y]],".Rdata"))

tic("svm ihs done")
mod_svm_ihs <- train(
  asinh(net_wealth) ~ ., 
  data = train_df %>% 
    select(-any_of(wealth_debt),-have_wealth),
  trControl = ctrl,
  method = "svmRadial",
  tuneLength = 10,
  preProcess = c("scale","center")
)
save(mod_svm_ihs, file =  paste0("models/mod_svm_ihs_",years[[y]],".Rdata"))
toc()



tic("knn ihs done")
mod_knn_ihs <- train(
  asinh(net_wealth) ~ .,
  data = train_df %>%
    select(-any_of(wealth_debt),-have_wealth),
  trControl = trainControl(
    method = "cv",
    number = 5,
    allowParallel = T,
    savePredictions = "final"
  ),
  method = "knn", 
  tuneGrid = expand.grid(
    k = c(5,25,100,250)
  ),
  preProcess = c("scale","center")
)
save(mod_knn_ihs, file =  paste0(path,"models/mod_knn_ihs_",years[[y]],".Rdata"))
toc()


preds_rf_ihs <- predict(mod_rf_ihs, val_df)
preds_xgb_ihs <- predict(mod_xgb_ihs, val_df)
preds_net_ihs <- predict(mod_net_ihs, val_df)
preds_knn_ihs <- predict(mod_knn_ihs, val_df)
preds_svm_ihs <- predict(mod_svm_ihs, val_df)
preds_glm_ihs <- predict(mod_glm_ihs, val_df)
preds_lasso_ihs <- predict(mod_lasso_ihs, val_df)

preds_ihs <- tibble(
  y = asinh(val_df$net_wealth),
  preds_rf_ihs,
  preds_xgb_ihs,
  preds_net_ihs,
  preds_svm_ihs,
  preds_knn_ihs,
  preds_glm_ihs,
  preds_lasso_ihs
)

mod_ens_lm_ihs <- train(
  y ~ ., 
  data = preds_ihs,
  method = "lm",
  trControl = ctrl
)

save(
  mod_ens_lm_ihs, 
  file = paste0("models/mod_ensemble_ihs_",years[[y]],".Rdata")
)


##### wealth and debt

train_wealth_debt <- train_df %>% 
  mutate_at(
    vars(wealth_gross, debt_total),
    ~ asinh(.)
  )

tic("rf wealth done")
mod_rf_wealth <- train(
  y = train_wealth_debt$wealth_gross,
  x = train_wealth_debt %>% select(-net_wealth,-have_wealth, -any_of(wealth_debt)),
  method = "ranger",
  trControl = ctrl,
  tuneLength = 10,
  keep.inbag = TRUE,
  importance = "impurity"
)

save(mod_rf_wealth, file = paste0("models/mod_rf_wealth_",years[[y]],".Rdata"))
toc()

## NEURAL NET
tic("net wealth done")
mod_net_wealth <- train(
  wealth_gross ~ .,
  data = train_wealth_debt %>% select(-net_wealth,-have_wealth, -debt_total),
  method = "nnet",
  trControl = ctrl,
  tuneLength = 10,
  preProcess = c("scale","center"),
  maxit = 200, 
  MaxNWts = 2000,
  linout = T,
  trace = F
)

save(mod_net_wealth, file = paste0("models/mod_net_wealth_",years[[y]],".Rdata"))
toc()

## Boosted trees
tic("xgb wealth done")
mod_xgb_wealth <- train(
  wealth_gross ~ .,
  data = train_wealth_debt %>% select(-net_wealth,-have_wealth, -debt_total),
  method="xgbDART",
  trControl = trainControl(method="none"), 
  tuneGrid = mod_xgb_wealth$bestTune
)
save(mod_xgb_wealth, file = paste0("models/mod_xgb_wealth_",years[[y]],".Rdata"))
toc()

mod_glm_wealth <- train(
  wealth_gross ~ ., 
  data = train_wealth_debt %>% select(-net_wealth,-have_wealth, -debt_total),
  trControl = ctrl,
  method = "glm"
)

mod_lasso_wealth <- train(
  wealth_gross ~ .,
  data = train_wealth_debt %>% select(-net_wealth,-have_wealth, -debt_total),
  trControl = ctrl,
  method = "glmnet",
  tuneLength = 10
)

save(mod_glm_wealth, mod_lasso_wealth, file = paste0("models/mod_glm_wealth_",years[[y]],".Rdata"))

tic("svm wealth done")
mod_svm_wealth <- train(
  wealth_gross ~ ., 
  data = train_wealth_debt %>% 
    select(-net_wealth,-have_wealth, -debt_total) %>%
    sample_frac(0.3),
  trControl = ctrl,
  method = "svmRadial",
  tuneLength = 10,
  preProcess = c("scale","center")
)
save(mod_svm_wealth, file =  paste0("models/mod_svm_wealth_",years[[y]],".Rdata"))
toc()

tic("knn wealth done")
mod_knn_wealth <- train(
  wealth_gross ~ .,
  data = train_wealth_debt %>% select(-net_wealth,-have_wealth, -debt_total),
  trControl = trainControl(
    method = "cv",
    number = 5,
    allowParallel = T,
    savePredictions = "final"
  ),
  method = "knn", 
  tuneGrid = expand.grid(
    k = c(5,25,100,250)
    ),
  preProcess = c("scale","center")
)
save(mod_knn_wealth, file =  paste0(path,"models/mod_knn_wealth_",years[[y]],".Rdata"))
toc()

preds_rf_wealth <- predict(mod_rf_wealth, val_df)
preds_xgb_wealth <- predict(mod_xgb_wealth, val_df)
preds_net_wealth <- predict(mod_net_wealth, val_df)
preds_glm_wealth <- predict(mod_glm_wealth, val_df)
preds_lasso_wealth <- predict(mod_lasso_wealth, val_df)
preds_svm_wealth <- predict(mod_svm_wealth, val_df)
preds_knn_wealth <- predict(mod_knn_wealth, val_df)

preds_wealth <- tibble(
  y = asinh(val_df$wealth_gross),
  preds_svm_wealth,
  preds_knn_wealth,
  preds_rf_wealth,
  preds_xgb_wealth,
  preds_net_wealth,
  preds_glm_wealth,
  preds_lasso_wealth
)

mod_ens_lm_wealth <- train(
  y ~ ., 
  data = preds_wealth,
  method = "lm",
  trControl = ctrl
)

save(
  mod_ens_lm_wealth, 
  file = paste0("models/mod_ensemble_wealth_",years[[y]],".Rdata")
)



tic("rf debt done")
mod_rf_debt <- train(
  y = train_wealth_debt$debt_total,
  x = train_wealth_debt %>% select(-net_wealth,-have_wealth, -any_of(wealth_debt)),
  method = "ranger",
  trControl = ctrl,
  tuneLength = 10,
  keep.inbag = TRUE,
  importance = "impurity"
)

save(mod_rf_debt, file = paste0("models/mod_rf_debt_",years[[y]],".Rdata"))
toc()


## NEURAL NET
tic("net debt done")
mod_net_debt <- train(
  debt_total ~ .,
  data = train_wealth_debt %>% select(-net_wealth,-have_wealth, -wealth_gross),
  method = "nnet",
  trControl = ctrl,
  tuneLength = 10,
  preProcess = c("scale","center"),
  maxit = 200, 
  MaxNWts = 2000,
  linout = T,
  trace = F
)

save(mod_net_debt, file = paste0("models/mod_net_debt_",years[[y]],".Rdata"))
toc()

## Boosted trees
tic("xgb debt done")
mod_xgb_debt <- train(
  debt_total ~ .,
  data = train_wealth_debt %>% select(-net_wealth,-have_wealth, -wealth_gross),
  method="xgbDART",
  trControl = ctrl,
  tuneLength = 10
)
save(mod_xgb_debt, file = paste0("models/mod_xgb_debt_",years[[y]],".Rdata"))
toc()

mod_glm_debt <- train(
  debt_total ~ ., 
  data = train_wealth_debt %>% select(-net_wealth,-have_wealth, -wealth_gross),
  trControl = ctrl,
  method = "glm"
)

mod_lasso_debt <- train(
  debt_total ~ .,
  data = train_wealth_debt %>% select(-net_wealth,-have_wealth, -wealth_gross),
  trControl = ctrl,
  method = "glmnet",
  tuneLength = 10
)

save(mod_glm_debt, mod_lasso_debt, file = paste0("models/mod_glm_debt_",years[[y]],".Rdata"))

tic("svm debt done")
mod_svm_debt <- train(
  debt_total ~ ., 
  data = train_wealth_debt %>% 
    select(-net_wealth,-have_wealth, -wealth_gross),
  trControl = ctrl,
  method = "svmRadial",
  tuneLength = 10,
  preProcess = c("scale","center")
)
save(mod_svm_debt, file =  paste0("models/mod_svm_debt_",years[[y]],".Rdata"))
toc()

tic("knn debt done")
mod_knn_debt <- train(
  debt_total ~ .,
  data = train_wealth_debt %>% select(-net_wealth,-have_wealth, -wealth_gross),
  trControl = trainControl(
    method = "cv",
    number = 5,
    allowParallel = T,
    savePredictions = "final"
  ),
  method = "knn",
  tuneGrid = expand.grid(
    k = c(5,25,100,250)
  ),
  preProcess = c("scale","center")
)
save(mod_knn_debt, file =  paste0(path,"models/mod_knn_debt_",years[[y]],".Rdata"))
toc()


preds_rf_debt <- predict(mod_rf_debt, val_df)
preds_xgb_debt <- predict(mod_xgb_debt, val_df)
preds_net_debt <- predict(mod_net_debt, val_df)
preds_glm_debt <- predict(mod_glm_debt, val_df)
preds_lasso_debt <- predict(mod_lasso_debt, val_df)
preds_knn_debt <- predict(mod_knn_debt, val_df)
preds_svm_debt <- predict(mod_svm_debt, val_df)

preds_debt <- tibble(
  y = asinh(val_df$debt_total),
  preds_rf_debt,
  preds_xgb_debt,
  preds_net_debt,
  preds_svm_debt,
  preds_knn_debt,
  preds_glm_debt,
  preds_lasso_debt
)

mod_ens_lm_debt <- train(
  y ~ ., 
  data = preds_debt,
  method = "lm",
  trControl = ctrl
)

save(
  mod_ens_lm_debt, 
  file = paste0("models/mod_ensemble_debt_",years[[y]],".Rdata")
)
