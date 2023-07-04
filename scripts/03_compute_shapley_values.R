library(tidyverse)
library(iml)
library(tictoc)
library(ranger)
library(caret)

year <- 2020

load("data/train_test_",year,".Rdata")

mods <- c(
  "svm",
  "knn",
  "rf",
  "glm",
  "net",
  "xgb",
  "ensemble"
)
types <- c("bin","pos","zero","neg"
           #,"ihs","wealth","debt"
)

for (m in mods) {
  for (t in types) {
    
    x <- paste0("mod_",m,"_",t,"_",year,".Rdata")
    if (x %in% list.files("models/")) {
      load(paste0("models/",x))
    }
    
  }
  
}  # end load models

# prediction function
pred_fct <- function(model, newdata) {
  predict(model, newdata)
}


##### positive model
train_pos <- train_df %>%
  select(
    -have_wealth, -wealth_gross, -debt_total
  ) %>% 
  mutate_if(
    is.logical,
    ~ as.numeric(.)
  ) %>% 
  mutate_if(
    is.factor,
    ~ as.character(.)
  ) %>% 
  filter(net_wealth > 0) %>% 
  mutate(net_wealth = log(net_wealth))

test <- test_df %>%
  #sample_n(1000) %>% 
  select(
    -have_wealth, -net_wealth, -wealth_gross, -debt_total
  ) 

# Shapley objects for regression
shaps <- map(
  mget(
    ls(pattern="^mod_.*(rf|svm|knn|net|glm|lasso|xgb)_pos")
    ),
  ~ Predictor$new(
    model = .,
    data = train_pos %>% select(-net_wealth),
    y = train_pos$net_wealth,
    predict.function = pred_fct
  )
)

mods <- c("glm", "knn", "svm", "lasso", "net", "rf", "xgb")

phi <- matrix(ncol = ncol(test), nrow = nrow(test))

# takes around nrow(test)*10s
for (sh in 1:nrow(test)) {
  tic(paste0(sh,"/",nrow(test), " complete."))
  
  phi[sh,] <- Shapley$new(
    shap,
    x.interest = test[sh,]
  )$results$phi
  
  toc()
}

phi <- phi %>%
  as_tibble
names(phi) <- names(test)

save(phi, file = "data/shapley_values_pos.Rdata")



# phis <- vector("list", length(mods))
# 
# for (m in seq_along(mods)) {
#   
#   phis[[m]] <- matrix(ncol = ncol(test), nrow = nrow(test))
#   
#   # takes around nrow(test)*10s 
#   for (sh in 1:nrow(test)) {
#     tic(paste0(sh,"/",nrow(test), " complete."))
#     
#     phis [[m]] [sh,] <- Shapley$new(
#       shaps[[m]],
#       x.interest = test[sh,]
#     )$results$phi
#     
#     toc()
#   }
#   
#   message(mods[[m]], " complete")
#   
# }
# 
# phi <- phi %>%
#   as_tibble
# names(phi) <- names(test)
# 
# save(phis, file = "data/shapley_values_pos_full.Rdata")



############# NEGATIVE MODEL -----------
train_neg <- train_df %>%
  select(
    -have_wealth, -wealth_gross, -debt_total
  ) %>% 
  mutate_if(
    is.logical,
    ~ as.numeric(.)
  ) %>% 
  mutate_if(
    is.factor,
    ~ as.character(.)
  ) %>% 
  filter(net_wealth<0) %>% 
  mutate(net_wealth = asinh(net_wealth))

# Shapley object for regression
shap <- Predictor$new(
  model = mod_rf_neg,
  data = train_neg %>% select(-net_wealth),
  y = train_neg$net_wealth,
  predict.function = pred_fct
)

test <- test_df %>%
  select(
    -have_wealth, -net_wealth, -wealth_gross, -debt_total
  ) 
phi <- matrix(ncol = ncol(test), nrow = nrow(test))

# takes around nrow(test)*10s 
for (sh in 1:nrow(test)) {
  tic(paste0(sh,"/",nrow(test), " complete."))
  
  phi[sh,] <- Shapley$new(
    shap,
    x.interest = test[sh,]
  )$results$phi
  
  toc()
}

phi <- phi %>% 
  as_tibble 
names(phi) <- names(test)  

save(phi, file = "data/shapley_values_neg.Rdata")



########## BINARY model ---------------

# prediction function
pred_fct <- function(model, newdata) {
  predict(model, newdata, type = "prob")$yes
}


shap <- Predictor$new(
  model = mod_rf_bin,
  data = train_df %>%
    select(
      -net_wealth, -wealth_gross, -debt_total
    ) %>% 
    mutate_if(
      is.logical,
      ~ as.numeric(.)
    ) %>% 
    mutate_if(
      is.factor,
      ~ as.character(.)
    ),
  y = "have_wealth",
  #class = "have_wealth",
  predict.function = pred_fct,
  type = "prob"
)

test <- test_df %>%
  select(
    -net_wealth, -have_wealth, -wealth_gross, -debt_total
  ) 

phi <- matrix(ncol = ncol(test), nrow = nrow(test))

for (sh in 1:nrow(test)) {
  tic(paste0(sh,"/",nrow(test), " complete."))
  
  x <- Shapley$new(
    shap,
    x.interest = test[sh,]
  )$results
  
  phi[sh,] <- x %>% 
    filter(class == "yes") %>% 
    .$phi
  
  toc()
}

phi <- phi %>% 
  as_tibble 
names(phi) <- names(test)  

save(phi, file = "data/shapley_values_binary.Rdata")


###### ZERO -----
# prediction function
pred_fct <- function(model, newdata) {
  predict(model, newdata, type = "prob")$zero
}

train_0 <- train_df %>% 
  filter(net_wealth <= 0) %>% 
  mutate(have_wealth = ifelse(net_wealth == 0, "zero","negative") )


shap <- Predictor$new(
  model = mod_rf_zero,
  data = train_0 %>%
    select(
      -net_wealth, -wealth_gross, -debt_total
    ) %>% 
    mutate_if(
      is.logical,
      ~ as.numeric(.)
    ) %>% 
    mutate_if(
      is.factor,
      ~ as.character(.)
    ),
  y = "have_wealth",
  #class = "have_wealth",
  predict.function = pred_fct,
  type = "prob"
)


test <- test_df %>%
  filter(net_wealth <= 0) %>% 
  select(
    -net_wealth, -have_wealth, -wealth_gross, -debt_total
  ) 

phi <- matrix(ncol = ncol(test), nrow = nrow(test))

for (sh in 1:nrow(test)) {
  tic(paste0(sh,"/",nrow(test), " complete."))
  
  x <- Shapley$new(
    shap,
    x.interest = test[sh,]
  )$results
  
  phi[sh,] <- x %>% 
    filter(class == "zero") %>% 
    .$phi
  
  toc()
}

phi <- phi %>% 
  as_tibble 
names(phi) <- names(test)  

save(phi, file = "data/shapley_values_zero.Rdata")
