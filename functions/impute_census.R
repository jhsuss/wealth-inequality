
impute_census <- function(
  year = NULL
) {
  
  library(tidyverse)
  library(caret)
  
  year_census <- year
  
  if (year_census >= 1990 & year_census < 2008) {
    year_model <- 2000 
  } else if (year_census >= 2008) {
    year_model <- 2020 
  } else {
    year_model <- year_census
  }
  
  # load census
  file_path <- paste0("data/census_clean_",year_census,".Rdata") 
  load(file_path)
  
  # remove some strange obs which affect model fitting (additional categories)
  # 1 obs of 'other'
  if (year_census <= 1980 & year_census != 1950) {
    census <- census %>% 
      filter(house_tenure != "other")
  } 
  
  if (year_census == 1960) {
    census <- census %>% 
      filter(educ_hrp != "inappropriate")
  } 
  

  # load models
  mods <- c(
    "svm",
    "knn",
    "rf",
    "glm",
    "net",
    "xgb",
    "ensemble"
  )
  # choose subset of models (omit IHS and wealth and debt models for time reasons)
  types <- c(
    "bin","pos","zero","neg"
    )
  
  for (m in mods) {
    for (t in types) {
      
      x <- paste0("mod_",m,"_",t,"_",year_model,".Rdata")
      if (x %in% list.files("models/")) {
        load(paste0("models/",x))
      }
  
    }
    
  }  # end load models
  
  
  # keep identifiers, geography and preds
  ids <- c("sample", "serial", "year")
  geogs <- c("state", "statefip", "region_label", "puma", "czone", "metarea","sea","cntygp97","cntygp98")
  demogs <- c( "house_tenure","house_value","house_value_new","educ_hrp","race","age", "ind_hrp", "occ_hrp", "sex", "household_size", "work_status")
  wgts <- c("hhwt","hwx")
  
  add <- census %>% 
    select(
      any_of(ids),
      any_of(geogs),
      matches("^inc"),
      any_of(demogs),
      any_of(wgts),
      matches("_flag$"), adjustments
    )
  
  # split into more manageable chunks for fitting models
  tmp <- census %>% 
    group_split(sample, region_label) %>% 
    map(
      select,
      sample, serial,
      any_of(mod_rf_bin$trainingData %>% names), 
      matches("_new")
    ) 

  rm(census)

  # loop through census to make predictions
  census_list <- vector("list", length(tmp))
  
  for (i in 1:length(census_list)) {
    
    # for preds with top and bottom-code adjusted values
    tmp_new <- tmp[[i]] %>% 
      # remove adjusted 
      select(
        -any_of(vars_flag)
      ) %>% 
      # rename for models
      rename_at(
        vars(matches("_new")),
        ~ str_replace(., "_new", "")
      ) %>% 
      mutate_if(
        is.numeric,
        ~ ifelse(is.nan(.) | . == Inf, NA, .)
      )
    
    # loop through list of models
    ps_bin <- map(
      mget(
        ls(pattern="^mod_(rf|svm|knn|net|glm|lasso|xgb)_bin$")
        ),
      ~ predict(
        .,
        tmp[[i]],
        type = "prob"
        )$yes
      )
    ps_bin <- ps_bin %>% 
      bind_rows() %>%
      rename_all(~ str_replace(.,"^mod","preds"))
    
    ps_zero <- map(
      mget(
        ls(pattern="^mod_(rf|svm|knn|net|glm|lasso|xgb)_zero$")
      ),
      ~ predict(
        .,
        tmp[[i]],
        type = "prob"
      )$zero
    )
    ps_zero <- ps_zero %>% 
      bind_rows() %>%
      rename_all(~ str_replace(.,"^mod","preds")) 
   
    ps_quant <- map(
      mget(
        ls(pattern="^mod_(rf|svm|knn|net|glm|lasso|xgb)_(pos|neg)$")
      ),
      ~ predict(
        .,
        tmp[[i]]
      )
    )
    ps_quant <- ps_quant %>%
      bind_rows() %>%
      rename_all(~ str_replace(.,"^mod","preds"))
    
    ps_quant_new <- map(
      mget(
        ls(pattern="^mod_(rf|svm|knn|net|glm|lasso|xgb)_(pos|neg)$")
      ),
      ~ predict(
        .,
        tmp_new
      )
    )
    ps_quant_new <- ps_quant_new %>% 
      bind_rows() %>%
      rename_all(~ str_replace(.,"^mod","preds")) 
    
    # fit the ensemble models
    preds_ens_bin <- predict(mod_ens_lm_bin, ps_bin, type = "prob")$yes
    preds_ens_zero <- predict(mod_ens_lm_zero, ps_zero, type = "prob")$zero
    
    preds_ens_pos <- predict(mod_ens_lm_pos, ps_quant)
    preds_ens_pos_new <- predict(mod_ens_lm_pos, ps_quant_new)
    
    preds_ens_neg <- predict(mod_ens_lm_neg, ps_quant)
    preds_ens_neg_new <- predict(mod_ens_lm_neg, ps_quant_new)
    
    ps_quant_new <- ps_quant_new %>% 
      rename_all( ~str_c(.,"_new"))
    
    # save subset of predictions
    census_list[[i]] <- tibble(
      sample = tmp_new$sample,
      serial = tmp_new$serial,
      
      preds_ens_bin,
      preds_ens_pos,
      preds_ens_pos_new,
      preds_ens_zero,
      preds_ens_neg,
      preds_ens_neg_new
      ) %>% 
      cbind(
        # add subset of level 1 predictions
        ps_bin,
        ps_quant,
        ps_quant_new
      )
    
    
    message(i,"/",length(tmp), " complete")
  }
  
  rm(tmp)
  
  # bind rows and add back in additional columns
  census <- census_list %>% 
    reduce(bind_rows) 
  
  # add LASSO ensemble (experimental)
  preds_ens_pos_lasso <- census %>%
    select(
      matches("preds.*_pos_new")
    ) %>%
    rename_all(~str_replace_all(.,"_new","")) %>%
    mutate(
      preds_ens_pos_lasso = predict(mod_ens_lasso_pos,.)
    ) %>%
    .$preds_ens_pos_lasso
  
  
  census <- census %>%
    cbind(preds_ens_pos_lasso)
  
  census <- census %>% 
    left_join(add, by = c("sample","serial")) %>% 
    select(sample, serial, hhwt, state, region_label, 
           matches("preds_ens"),
           everything()
           )
  
  rm(add)
  
  
  ########## confidence intervals hhld level ------------
  ## for binary
  se_bin <- predict(
    mod_ens_lm_bin$finalModel, 
    census, 
    interval = "prediction", 
    type = "response",
    se.fit = T
  )$se.fit
  
  se_pos <- predict(
    mod_ens_lm_pos$finalModel, 
    census %>% select(matches("_pos_new")) %>% rename_all(~ str_remove(.,"_new")), 
    interval = "prediction",
    se.fit = T
  )$se.fit
  
  se_neg <- predict(
    mod_ens_lm_neg$finalModel, 
    census %>% select(matches("_neg_new")) %>% rename_all(~ str_remove(.,"_new")), 
    interval = "prediction",
    se.fit = T
  )$se.fit
  
  census <- census %>% 
    cbind(
      se_bin,
      se_pos,
      se_neg
    )

  
  # load binary model threshold values
  load("data/binary_thresholds.Rdata")
  
  ### calculate final household wealth prediction
  ### 'pred_wealth_new' uses top-code adjusted variables
  census <- census %>% 
    mutate(
      pred_wealth_new = case_when(
        preds_ens_bin >= thresh ~ exp(preds_ens_pos_new),
        preds_ens_bin < thresh & preds_ens_zero >= optim_thresh_0 ~ 0,
        preds_ens_bin < thresh & preds_ens_zero < optim_thresh_0 ~ sinh(preds_ens_neg_new),
      ),
      
      pred_wealth = case_when(
        preds_ens_bin >= thresh ~ exp(preds_ens_pos),
        preds_ens_bin < thresh & preds_ens_zero >= optim_thresh_0 ~ 0,
        preds_ens_bin < thresh & preds_ens_zero < optim_thresh_0 ~ sinh(preds_ens_neg),
      )
    ) 
  
  # select set of variables
  census <- census %>%
    select(
      any_of(ids),
      any_of(geogs),
      matches("^inc"),
      any_of(demogs),
      matches("^pred(s|)_|^se_"),
      any_of(wgts),
      matches("_flag$"), 
      adjustments,
      
      
    )
  
  ## save final 
  save_path <- paste0("data/census_imputed_wealth_",year_census,".Rdata")
  save(census, file = save_path)
  source("functions/save_final_imputed_wealth.R") # final bits to do for final imputed file  
  
  message("imputation for census year ",year_census," complete")
  
  
  
}

