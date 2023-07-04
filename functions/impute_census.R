
impute_census <- function(
  year = 2000
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
  types <- c("bin","pos","zero","neg"
             #,"ihs","wealth","debt"
             )
  
  for (m in mods) {
    for (t in types) {
      
      x <- paste0("mod_",m,"_",t,"_",year_model,".Rdata")
      if (x %in% list.files("models/")) {
        load(paste0("models/",x))
      }
  
    }
    
  }  # end load models
  
  # figure out which vars are topcode adjusted
  load("data/topcodes.Rdata")
  
  vars_flag <- topcodes %>% 
    filter(year == year_census) %>% 
    select_if(~ !any(is.na(.))) %>% 
    select(
      matches("_topcode")
    ) %>% 
    rename_all(
      ~ str_remove(., "_topcode")
    ) %>% 
    select(!matches("incsupp")) %>% 
    names
  
  if (year_census == 1940) {
    vars_flag <- "incwage"
  } else if (year_census == 1980) (
    vars_flag <- vars_flag[-4]
  ) else if (year_census == 1970) (
    vars_flag <- vars_flag[-3]
  ) else if (year_census == 1990) {
    vars_flag <- vars_flag[-4]
  }
  
  for (i in seq_along(vars_flag)) {
    
    census[[ paste0(vars_flag[[i]], "_adjust_flag") ]] <- ifelse(
      census[[ vars_flag[[i]] ]] != census[[ paste0(vars_flag[[i]], "_new") ]],
      1, 
      0
    )
    
  }
  
  census <- census %>%
    mutate(
      adjustments = rowSums(select(., contains("_adjust_flag")))
    )
  ##### end flag adding
  
  # keep only identifiers, geography and preds (can always merge back with census)
  ids <- c("sample", "serial", "year")
  geogs <- c("state", "statefip", "region_label", "puma", "czone", "metarea","sea","cntygp97","cntygp98")
  demogs <- c( "house_tenure","house_value","house_value_new","educ_hrp","race","age")
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
  
  # split into more manageable chunks, only if nrow > 2e6?
  if (year_census != 1950) {
    
    tmp <- census %>% 
      group_split(sample, house_tenure, region_label) %>% 
      map(
        select,
        sample, serial,
        any_of(mod_rf_bin$trainingData %>% names), 
        matches("_new")
      ) 
    
    
  } else {
    
    tmp <- census %>% 
      group_split(
        region_label
      ) %>% 
      map(
        select,
        sample, serial,
        any_of(mod_rf_bin$trainingData %>% names), 
        matches("_new")
      ) 
  }
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
        ls(pattern="^mod_.*(rf|svm|knn|net|glm|lasso|xgb)_bin")
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
        ls(pattern="^mod_.*(rf|svm|knn|net|glm|lasso|xgb)_zero")
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
        ls(pattern="^mod_.*(rf|svm|knn|net|glm|lasso|xgb)_(pos|neg)")
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
        ls(pattern="^mod_.*(rf|svm|knn|net|glm|lasso|xgb)_(pos|neg)")
      ),
      ~ predict(
        .,
        tmp_new
      )
    )
    ps_quant_new <- ps_quant_new %>% 
      bind_rows() %>%
      rename_all(~ str_replace(.,"^mod","preds")) 
    
    preds_ens_bin <- predict(mod_ens_lm_bin, ps_bin, type = "prob")$yes
    preds_ens_zero <- predict(mod_ens_lm_zero, ps_zero, type = "prob")$zero
    
    preds_ens_pos <- predict(mod_ens_lm_pos, ps_quant)
    preds_ens_pos_new <- predict(mod_ens_lm_pos, ps_quant_new)
    
    preds_ens_neg <- predict(mod_ens_lm_neg, ps_quant)
    preds_ens_neg_new <- predict(mod_ens_lm_neg, ps_quant_new)
    
    ps_quant_new <- ps_quant_new %>% 
      rename_all( ~str_c(.,"_new"))
    
    
    ### other transformation predictions (just new census)
    # ps_alt <- map(
    #   mget(
    #     ls(pattern="^mod_.*(rf|svm|knn|net|glm|lasso|xgb)_(ihs|wealth|debt)")
    #   ),
    #   ~ predict(
    #     .,
    #     tmp_new
    #   )
    # )
    # ps_alt <- ps_alt %>% 
    #   bind_rows() %>%
    #   rename_all(~ str_replace(.,"^mod","preds"))
    # 
    #preds_ens_ihs <- predict(mod_ens_lm_ihs, ps_alt)
    #preds_ens_wealth <- predict(mod_ens_lm_wealth, ps_alt)
    #preds_ens_debt <- predict(mod_ens_lm_debt, ps_alt)
    
    census_list[[i]] <- tibble(
      sample = tmp_new$sample,
      serial = tmp_new$serial,
      
      preds_ens_bin,
      preds_ens_pos,
      preds_ens_pos_new,
      preds_ens_zero,
      preds_ens_neg,
      preds_ens_neg_new#,
      
      #preds_ens_ihs,
      #preds_ens_wealth,
      #preds_ens_debt
      ) %>% 
      cbind(
        # select level 1 predictions
        ps_bin,
        preds_rf_zero = ps_zero$preds_rf_zero,
        ps_quant,
        ps_quant_new
      )
    
    
    message(i,"/",length(tmp), " complete")
  }
  
  rm(tmp)
  
  # bind rows and add back in additional columns
  census <- census_list %>% 
    reduce(bind_rows) 
  
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

  census <- census %>%
    select(
      any_of(ids),
      any_of(geogs),
      matches("^inc"),
      any_of(demogs),
      matches("^preds_|^se_"),
      any_of(wgts),
      matches("_flag$"), adjustments
    )
  
  save_path <- paste0("data/census_imputed_wealth_",year_census,".Rdata")
  save(census, file = save_path)
  message("imputation for census year ",year_census," complete")
}

