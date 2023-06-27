library(tidyverse)

# variables available in census by year
##### 
census_vars <- tribble(
  ~ year, ~ valueh,
    2020,  T,
    2010,  T,
    2000,  T,
    1990,  T,
    1980,  T,
    1970,  T,
    1960,  T,
    1950,  F,
    1940,  T#,
    #1930,  T
  )

census_vars <- census_vars %>% 
  mutate(
    ownershp = ifelse(year == 1950, F, T), 
    
    mortamt1 = ifelse(year < 1990, F, T),
    mortamt2 = ifelse(year < 1990, F, T),
    #mortotal = ifelse(year == 1980, T, F),
    
    taxincl = ifelse(year < 1980, F, T),
    insincl = ifelse(year < 1980, F, T),
    proptx99 = ifelse(year < 1990, F, T),
    rent = ifelse(year == 1950 | year <= 1930, F, T),
    
    incwage = ifelse(year < 1940, F, T),
    incbus = ifelse(year < 1950, F, T),
    incss = ifelse(year < 1970, F, T),
    incwelfr = ifelse(year < 1970, F, T),
    incinvst = ifelse(year < 1980, F, T),
    incretir = ifelse(year < 1990, F, T),
    incother = ifelse(year < 1950, F, T),
    
    #hhincome = ifelse(year < 1980, F, T),
    #inctot = ifelse(year < 1950, F, T),
    
    age = T,
    race = T,
    educ = T,
    sex = T,
    marst = T,
    #hispan = T,
    
    
    occ = T, 
    ind = T,
    empstat = T,
    classwkr = T,
    uhrswork = ifelse(year < 1980, F, T),
    wkswork2 = ifelse(year < 1940, F, T),
    
    vehicle = ifelse(year < 1990, F, T),
    hcovany = ifelse(year < 2010, F, T)
    
  ) %>% 
  rename_all(~str_to_upper(.))
  
save(census_vars, file = "tables/census_vars_available.Rdata")
#####


##### performance for test sample

library(caret)

years <- c(2020,2000,rev(seq(1940,1980,10)))
# load models
Mods <- c(
  "svm",
  "knn",
  "rf",
  "glm",
  "net",
  "xgb",
  "ensemble"
)
types <- c("bin","pos","zero","neg","ihs","wealth","debt")

# performance metrics containers

metrics <- preds_test_sample <- vector("list", length(years))
  
# across years / models 
for (y in seq_along(years)) {
  
  for (M in Mods) {
    for (t in types) {
      
      x <- paste0("mod_",M,"_",t,"_",years[[y]],".Rdata")
      if (x %in% list.files(paste0(path,"models/"))) {
        load(paste0(path,"models/",x))
      }
      
    }
    
  }  # end load models
  
  # load test df
  load(paste0(path,"data/train_test_",years[[y]],".Rdata"))
  
  if (years[[y]] %in% seq(1940,1980,10)) {
    test_df <- test_df %>% 
      filter(educ_hrp != "inappropriate")
  }
  
  ps_bin <- map(
    mget(
      ls(pattern="^mod_.*(rf|svm|knn|net|glm|lasso|xgb)_bin")
    ),
    ~ predict(
      .,
      test_df,
      type = "prob"
    )$yes
  )
  ps_bin <- ps_bin %>% 
    bind_rows() %>%
    rename_all(~ str_replace(.,"^mod","preds"))
  
  ps_zero <- map(
    mget(
      ls(pattern="^mod_.*(rf|svm|knn|net|glm|lasso|xgb)_zero")
    ),
    ~ predict(
      .,
      test_df,
      type = "prob"
    )$zero
  )
  ps_zero <- ps_zero %>% 
    bind_rows() %>%
    rename_all(~ str_replace(.,"^mod","preds")) 
  
  ps_quant <- map(
    mget(
      ls(pattern="^mod_.*(rf|svm|knn|net|glm|lasso|xgb)_(pos|neg|ihs|debt|wealth)")
    ),
    ~ predict(
      .,
      test_df
    )
  )
  ps_quant <- ps_quant %>%  
    bind_rows() %>%
    rename_all(~ str_replace(.,"^mod","preds")) 
  
  
  preds_test <- cbind(
    test_df,
    ps_bin,
    ps_zero,
    ps_quant
  )
  
  preds_test$preds_ens_bin <- predict(mod_ens_lm_bin, ps_bin, type = "prob")$yes
  preds_test$preds_ens_zero <- predict(mod_ens_lm_zero, ps_zero, type = "prob")$zero
  preds_test$preds_ens_pos <- predict(mod_ens_lm_pos, ps_quant)
  preds_test$preds_ens_neg <- predict(mod_ens_lm_neg, ps_quant)
  
  preds_test$preds_ens_ihs <- predict(mod_ens_lm_ihs, ps_quant)
  preds_test$preds_ens_wealth <- predict(mod_ens_lm_wealth, ps_quant)
  preds_test$preds_ens_debt <- predict(mod_ens_lm_debt, ps_quant)
  preds_test$preds_wealth_debt <- sinh(preds_test$preds_ens_wealth) - sinh(preds_test$preds_ens_debt)
  
  # calculate optimal threshold based on Kappa metric
  Ps <- seq(0,1,.025)
  optim_thresh <- vector("numeric",length(Ps))
  for (p in seq_along(Ps)) {
    X <- ifelse(preds_test$preds_ens_bin >= Ps[[p]], "yes", "no") %>% factor()
    Y <- preds_test$have_wealth
    
    Z <- confusionMatrix(X, Y, mode = "prec_recall", positive="yes")
    optim_thresh[[p]] <- Z$overall[["Kappa"]] # Z$byClass[["Balanced Accuracy"]]
    
  }
  optim_thresh <- Ps[[which.max(optim_thresh)]]
  
  # for zero
  optim_thresh_0 <- vector("numeric",length(Ps))
  for (p in seq_along(Ps)) {
    tmp0 <- preds_test %>% 
      filter(have_wealth == "no") %>% 
      mutate(
        have_wealth = ifelse(net_wealth == 0, "zero", "negative") %>% factor()
      )
    
    X <- ifelse(tmp0$preds_ens_zero >= Ps[[p]], "zero", "negative") %>% factor()
    Y <- tmp0$have_wealth
    
    Z <- confusionMatrix(X, Y, mode = "prec_recall", positive="zero")
    optim_thresh_0[[p]] <- Z$overall[["Kappa"]]
    
  }
  optim_thresh_0 <- Ps[[which.max(optim_thresh_0)]]
  
  preds_test <- preds_test %>%
    mutate(
      preds_binary_50 = ifelse(preds_ens_bin >= 0.5, "yes", "no"),
      preds_binary_optim = ifelse(preds_ens_bin >= optim_thresh, "yes", "no"),
      preds_binary_ihs = ifelse(preds_ens_ihs > 0, "yes", "no"),
      preds_binary_wealth_debt = ifelse(preds_wealth_debt > 0, "yes", "no"),
      
      # preds_combo = ifelse(preds_ens_bin >= 0.5, exp(preds_ens_pos), sinh(preds_ens_neg)),
      # preds_combo_optim = ifelse(preds_ens_bin >= optim_thresh, exp(preds_ens_pos), sinh(preds_ens_neg)),
      
      preds_combo = case_when(
        preds_ens_bin >= 0.5 ~ exp(preds_ens_pos),
        preds_ens_bin < 0.5 & preds_ens_zero >= optim_thresh_0 ~ 0,
        preds_ens_bin < 0.5 & preds_ens_zero < optim_thresh_0 ~ sinh(preds_ens_neg),
      ),
      
      preds_combo_optim = case_when(
        preds_ens_bin >= optim_thresh ~ exp(preds_ens_pos),
        preds_ens_bin < optim_thresh & preds_ens_zero >= optim_thresh_0 ~ 0,
        preds_ens_bin < optim_thresh & preds_ens_zero < optim_thresh_0 ~ sinh(preds_ens_neg),
      ),
      
      preds_zero_optim = ifelse(preds_ens_zero >= optim_thresh_0, "zero", "negative"),
      
      # groups for visualising in figure
      group_combo = case_when(
        preds_combo > 0 & net_wealth > 0 ~ "TP",
        preds_combo <= 0 & net_wealth <= 0 ~ "TN",
        preds_combo > 0 & net_wealth <= 0 ~ "FP",
        preds_combo <= 0 & net_wealth > 0 ~ "FN"
      ),
      group_combo_optim = case_when(
        preds_combo_optim > 0 & net_wealth > 0 ~ "TP",
        preds_combo_optim <= 0 & net_wealth <= 0 ~ "TN",
        preds_combo_optim > 0 & net_wealth <= 0 ~ "FP",
        preds_combo_optim <= 0 & net_wealth > 0 ~ "FN"
      ),
      group_ihs = case_when(
        preds_ens_ihs > 0 & net_wealth > 0 ~ "TP",
        preds_ens_ihs <= 0 & net_wealth <= 0 ~ "TN",
        preds_ens_ihs > 0 & net_wealth <= 0 ~ "FP",
        preds_ens_ihs <= 0 & net_wealth > 0 ~ "FN"
      ),
      group_wealth_debt = case_when(
        preds_wealth_debt > 0 & net_wealth > 0 ~ "TP",
        preds_wealth_debt <= 0 & net_wealth <= 0 ~ "TN",
        preds_wealth_debt > 0 & net_wealth <= 0 ~ "FP",
        preds_wealth_debt <= 0 & net_wealth > 0 ~ "FN"
      )
      
    ) 
  
  # KAPPA score
  mods <- c(
    names(mod_ens_lm_bin$trainingData)[-1],
    #"preds_binary_50", 
    "preds_ens_bin",
    "preds_binary_ihs", 
    "preds_binary_wealth_debt"
  )
  # if (years[[y]] == 1980) {
  #   iter <- c(1:6,8)
  # } else {
    iter <- c(1:7,9)
  # }
  kappas <- accuracies <- TPR <- TNR <- optim_threshs <-  vector("numeric",length(mods))
  for (m in seq_along(mods)) {
    
    # if probability can calculate Kappa
    #if (m %in% iter) {
      # calculate optimal threshold based on Kappa metric
      Ps <- seq(0,1,.025)
      vecs <- vector("numeric",length(Ps))
      for (p in seq_along(Ps)) {
        x_ <- ifelse(preds_test[[mods[[m]]]] >= Ps[[p]], "yes", "no") %>% factor()
        y_ <- preds_test$have_wealth
        
        z_ <- confusionMatrix(x_, y_, mode = "prec_recall", positive="yes")
        vecs[[p]] <- z_$overall[["Kappa"]]
        
      }
      #Ps[[which.max(vecs)]]
      
    X <- ifelse(preds_test[[ mods[[m]] ]] >= Ps[[which.max(vecs)]], "yes", "no") %>% factor()
      
    # } else {
    #   X <- preds_test[[ mods[[m]] ]]  %>% factor()
    #}
    
    Y <- preds_test$have_wealth
    
    Z <- confusionMatrix(X, Y, mode = "prec_recall", positive="yes")
    optim_threshs[[m]] <- ifelse(m %in% c(1:8), Ps[[which.max(vecs)]], NA)
    kappas[[m]] <- Z$overall[["Kappa"]]
    accuracies[[m]] <- Z$overall[["Accuracy"]]
    TPR[[m]] <- Z$byClass[c("Sensitivity")] 
    TNR[[m]] <- Z$byClass[c("Specificity")]
  }
  names(kappas) <- names(accuracies) <- names(TPR) <- names(TNR) <- mods
  
  kappas <- kappas %>% 
    as.data.frame %>% 
    rename(Kappa = 1) %>% 
    mutate(
      model = row.names(.)
    ) %>% 
    as_tibble %>% 
    cbind(
      Accuracy = accuracies, TPR, TNR,
      Thresholds = optim_threshs
    )
  
  
  
  # Brier score 
  brier <- preds_test %>%
    mutate_at(
      vars(have_wealth, preds_binary_ihs, preds_binary_wealth_debt),
      ~ ifelse(. == "yes", 1, 0)
    ) %>% 
    select(have_wealth, 
           any_of(names(mod_ens_lm_bin$trainingData)),
           preds_ens_bin, 
           preds_binary_ihs, 
           preds_binary_wealth_debt
           ) %>% 
    mutate_at(
      vars(starts_with("preds")),
      ~ (. - have_wealth)^2
    ) %>% 
    summarise_at(
      vars(starts_with("preds")),
      ~ mean(.)
    )
  
  brier <- brier %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate(
      model = row.names(.)
    ) %>% 
    rename(Brier = V1) %>% 
    as_tibble 
  
  
  # zero stats
  ##### 
  brier0 <- preds_test %>%
    filter(net_wealth <= 0) %>% 
    mutate(
      zero_wealth = ifelse(net_wealth == 0, 1, 0)
      ) %>% 
    select(zero_wealth, 
           any_of(names(mod_ens_lm_zero$trainingData)),
           preds_ens_zero
    ) %>% 
    mutate_at(
      vars(starts_with("preds")),
      ~ (. - zero_wealth)^2
    ) %>% 
    summarise_at(
      vars(starts_with("preds")),
      ~ mean(.)
    ) %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate(
      model = row.names(.)
    ) %>% 
    rename(Brier = V1) %>% 
    as_tibble 
  
  mods_0 <- c(
    names(mod_ens_lm_zero$trainingData)[-1],
    "preds_ens_zero"
  )
  kappas_0 <- accuracies_0 <- TPR_0 <- TNR_0 <- optim_threshs_0 <-  vector("numeric",length(mods_0))
  
  for (m in seq_along(mods_0)) {
    
    # if probability can calculate Kappa
    #if (m %in% iter) {
    # calculate optimal threshold based on Kappa metric
    tmp0 <- preds_test %>% 
      filter(have_wealth == "no") %>% 
      mutate(
        have_wealth = ifelse(net_wealth == 0, "zero", "negative") %>% factor()
      )
    
    Ps <- seq(0,1,.025)
    vecs <- vector("numeric",length(Ps))
    for (p in seq_along(Ps)) {
      
      
      x_ <- ifelse(tmp0[[mods_0[[m]]]] >= Ps[[p]], "zero", "negative") %>% factor()
      y_ <- tmp0$have_wealth
      
      z_ <- confusionMatrix(x_, y_, mode = "prec_recall", positive="zero")
      vecs[[p]] <- z_$overall[["Kappa"]]
      
    }
    
    X <- ifelse(tmp0[[ mods_0[[m]] ]] >= Ps[[which.max(vecs)]], "zero", "negative") %>% factor()
    
    Y <- tmp0$have_wealth 
    
    Z <- confusionMatrix(X, Y, mode = "prec_recall", positive="zero")
    optim_threshs_0[[m]] <- ifelse(m %in% c(1:8), Ps[[which.max(vecs)]], NA)
    kappas_0[[m]] <- Z$overall[["Kappa"]]
    accuracies_0[[m]] <- Z$overall[["Accuracy"]]
    TPR_0[[m]] <- Z$byClass[c("Sensitivity")] 
    TNR_0[[m]] <- Z$byClass[c("Specificity")]
  }
  names(kappas_0) <- names(accuracies_0) <- names(TPR_0) <- names(TNR_0) <- mods_0
  
  kappas_0 <- kappas_0 %>% 
    as.data.frame %>% 
    rename(Kappa = 1) %>% 
    mutate(
      model = row.names(.)
    ) %>% 
    as_tibble %>% 
    cbind(
      Accuracy = accuracies_0, TPR = TPR_0, TNR = TNR_0,
      Thresholds = optim_threshs_0
    )
  
  zero_perf <- kappas_0 %>% left_join(brier0, by = "model")
  #####
  
  
  library(pROC)
  # AUCs
  # ROC and AUCs
  rocs <- list(
    RF = roc(response = preds_test$have_wealth, preds_test$preds_rf_bin), 
    XGB = roc(response = preds_test$have_wealth, preds_test$preds_xgb_bin), 
    ENS = roc(response = preds_test$have_wealth, preds_test$preds_ens_bin),
    SVM = roc(response = preds_test$have_wealth, preds_test$preds_svm_bin),
    KNN = roc(response = preds_test$have_wealth, preds_test$preds_knn_bin),
    NET = roc(response = preds_test$have_wealth, preds_test$preds_net_bin),
    GLM = roc(response = preds_test$have_wealth, preds_test$preds_glm_bin),
    EN = roc(response = preds_test$have_wealth, preds_test$preds_lasso_bin),
    IHS = roc(response = preds_test$have_wealth, ifelse(preds_test$preds_binary_ihs == "yes", 1, 0)),
    WD = roc(response = preds_test$have_wealth, ifelse(preds_test$preds_binary_wealth_debt == "yes", 1, 0))
  )
  
  # ggroc(
  #   rocs
  # )
  
  aucs <- aucs_upper <- aucs_lower <- vector("numeric", length(names(rocs)))
  
  for (i in seq_along(aucs)) {
    aucs[[i]] <- rocs[[i]]$auc
    aucs_upper[[i]] <- ci.auc(rocs[[i]]) %>% as.numeric %>% .[[3]]
    aucs_lower[[i]] <- ci.auc(rocs[[i]]) %>% as.numeric %>% .[[1]]
  }
  
  aucs <- tibble(
    model = names(rocs),
    aucs,
    aucs_lower,
    aucs_upper
  )
  
  # RMSE
  rmse <- preds_test %>% 
    summarise(
      RMSE_combo_50 = (asinh(preds_combo) - asinh(net_wealth))^2 %>% mean %>% sqrt,
      RMSE_combo_optim = (asinh(preds_combo_optim) - asinh(net_wealth))^2 %>% mean %>% sqrt,
      RMSE_ihs = (asinh(preds_ens_ihs) - asinh(net_wealth))^2 %>% mean %>% sqrt,
      
      RMSE_wealth = (preds_rf_wealth - asinh(wealth_gross))^2 %>% mean %>% sqrt,
      RMSE_debt = (preds_rf_debt - asinh(debt_total))^2 %>% mean %>% sqrt,
      RMSE_wealth_debt = (asinh(preds_wealth_debt) - asinh(net_wealth))^2 %>% mean %>% sqrt
    ) 
  
  # for actual positives
  rmse_pos <- preds_test %>% 
    filter(net_wealth > 0) %>% 
    summarise(
      rmse_pos = (preds_ens_pos - log(net_wealth))^2 %>% 
        mean %>% 
        sqrt,
      rmse_ihs = (asinh(preds_ens_ihs) - asinh(net_wealth))^2 %>% 
        mean %>% 
        sqrt,
      rmse_wealth = (asinh(preds_wealth_debt) - asinh(net_wealth))^2 %>% mean %>% sqrt
      )
  # for actual negatives
  rmse_neg <- preds_test %>% 
    filter(net_wealth < 0) %>% 
    summarise(
      rmse_neg = (preds_ens_neg - asinh(net_wealth))^2 %>% 
        mean %>% 
        sqrt,
      rmse_ihs = (asinh(preds_ens_ihs) - asinh(net_wealth))^2 %>% 
        mean %>% 
        sqrt,
      rmse_wealth_debt = (asinh(preds_wealth_debt) - asinh(net_wealth))^2 %>% 
        mean %>% 
        sqrt
    )
  
  # ROW for results table for each year
  clean_row <- tibble(
    Year = years[[y]],
    Threshold = optim_thresh,
    Brier = brier %>% filter(model == "preds_ens_bin") %>% .$Brier,
    Kappa = kappas %>% filter(model == "preds_ens_bin") %>% .$Kappa,
    Accuracy = kappas %>% filter(model == "preds_ens_bin") %>% .$Accuracy,
    TPR = kappas %>% filter(model == "preds_ens_bin") %>% .$TPR,
    TNR = kappas %>% filter(model == "preds_ens_bin") %>% .$TNR,
    AUC = aucs %>% 
      filter(model == "ENS") %>% 
      mutate(CI = paste0(round(aucs_lower,digits=3),"-",round(aucs_upper,digits=3))) %>% 
      .$CI,
    `RMSE positive wealth` = rmse_pos[["rmse_pos"]],
    `RMSE negative wealth` = rmse_neg[["rmse_neg"]],
    `RMSE overall` = rmse[["RMSE_combo_optim"]]
  ) %>% 
    mutate_if(
      is.numeric,
      ~ round(.,digits=3)
    )
  
  key <- tibble(
    model = c(
      "preds_rf_bin",
      "preds_xgb_bin",
      "preds_ens_bin",
      "preds_svm_bin",
      "preds_knn_bin",
      "preds_net_bin",
      "preds_glm_bin",
      "preds_lasso_bin",
      "preds_binary_ihs",
      "preds_binary_optim",
      "preds_binary_ihs",
      "preds_binary_wealth_debt",
      "preds_binary_50"
      ),
    label = c("RF", "XGB", "ENS", "SVM", "KNN", "NET", "GLM", "EN", "IHS", "ENS","IHS","WD","ENS_50")
  )
  # LONG
  if (years[[y]] <= 2020) {
    
    
    rmse_level1_pos <- preds_test %>% 
      filter(net_wealth > 0) %>% 
      summarise_at(
        vars(preds_glm_pos, preds_lasso_pos, preds_rf_pos, preds_xgb_pos, preds_net_pos, 
             preds_svm_pos, preds_knn_pos
             ),
        ~ (. - log(net_wealth))^2 %>% mean %>% sqrt
      )
    rmse_pos <- cbind(rmse_level1_pos, rmse_pos)
    
    rmse_level1_neg <- preds_test %>% 
      filter(net_wealth < 0) %>% 
      summarise_at(
        vars(preds_glm_neg, preds_lasso_neg, preds_rf_neg, preds_xgb_neg, preds_net_neg, 
             preds_svm_neg, preds_knn_neg
        ),
        ~ (. - asinh(net_wealth))^2 %>% mean %>% sqrt
      )
    rmse_neg <- cbind(rmse_level1_neg, rmse_neg)
    
    rmses <- cbind(
       rmse_pos %>% t(), 
       rmse_neg %>% t()
      ) %>% 
      as.data.frame %>% 
      mutate(
        model = row.names(.)
      ) %>% 
      rename(
        `RMSE positive wealth` = V1,
        `RMSE negative wealth` = V2
      ) %>% 
      mutate(
        label = c("GLM","EN","RF","XGB","NET","SVM","KNN","ENS","IHS","WD")
      ) %>% 
      left_join(
        rmse[2:4] %>%
          t() %>% 
          as.data.frame() %>%
          rename(`RMSE total` = V1) %>% 
          mutate(
            label = c("ENS","IHS","WD")
          ),
        by = "label"
      )
    
    
       
    
    
    
    # TODO allow this to run for all years when ensembles all fit
  } else {
    rmses <- cbind(
      rmse_pos %>% t(), 
      rmse_neg %>% t()
    ) %>% 
      as.data.frame %>% 
      mutate(
        model = row.names(.)
      ) %>% 
      rename(
        `RMSE positive wealth` = V1,
        `RMSE negative wealth` = V2
      ) %>% 
      mutate(
        label = c("ENS","IHS","WD")
      ) %>% 
      left_join(
        rmse[2:4] %>%
          t() %>% 
          as.data.frame() %>%
          rename(`RMSE total` = V1) %>% 
          mutate(
            label = c("ENS","IHS","WD")
          ),
        by = "label"
      )
    
  }
  
  clean_long <- aucs %>%
    rename(label = model) %>% 
    left_join(key, by = "label") %>%
    distinct(label, .keep_all = T) %>% 
    mutate(
      label = factor(label, levels = c("ENS","IHS","WD","GLM","EN","RF","XGB","NET","SVM","KNN"))
    ) %>% 
    arrange(label) %>% 
    mutate(
      CI = paste0(round(aucs_lower,digits=3),"-",round(aucs_upper,digits=3))
    ) %>% 
    left_join(
      brier, 
      by = "model"
    ) %>% 
    left_join(
      kappas %>% 
        left_join(
          key, by = "model"
        ) %>% 
        select(
          label, Kappa, Accuracy, TPR, TNR, Thresholds
        ) %>%
        distinct(label, .keep_all = T),
      by = "label"
    ) %>%
    # TODO add other RMSEs
    left_join(
      rmses,
      by = "label"
    ) %>% 
    select(
      label, Brier, Kappa, Accuracy, TPR, TNR, 
      AUC = CI, 
      `RMSE total`, `RMSE positive wealth`, `RMSE negative wealth`
    ) %>% 
    mutate_if(
      is.numeric,
      ~ round(.,digits=3)
    ) %>% 
    mutate(
      Year = years[[y]]
    )
  
  # bring it all together
  metrics[[y]] <- list(
    raw = list(
      kappas = kappas,
      brier = brier,
      aucs = aucs,
      rmse = rmse,
      rmse_pos = rmse_pos,
      rmse_neg = rmse_neg,
      optim_thresh_0 = optim_thresh_0
    ),
    zero_perf = zero_perf,
    clean = clean_row,
    clean_long = clean_long
  )
  
  preds_test_sample[[y]] <- preds_test
  
} # end year perf measures

names(metrics) <- years

save(metrics, preds_test_sample, file = "data/performance_metrics.Rdata")

metrics_table1 <- metrics %>% 
  map(
    `[[`, "clean"
  ) %>%
  reduce(bind_rows)

metrics_table2 <- metrics$`2020`$clean_long

metrics_table2_long <- metrics %>%
  map(
    `[[`, "clean_long"
  ) %>%
  reduce(bind_rows)

save(
  metrics_table1,
  metrics_table2,
  metrics_table2_long,
  file = "tables/metrics_table_ensemble.Rdata"
  )

