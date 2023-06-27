
compute_inequality <- function(year, short = T) {
  
  load(paste0(
    path,"data/census_imputed_wealth_",year,"_cw.Rdata"
  ))
  
  # add metareas where missing
  if (year == 2010) {
    metareas <- haven::read_dta(paste0(path,"/data/2010_2020_met_areas.gz"))
    metareas <- metareas %>% 
      select(year, sample, serial, metarea) %>% 
      distinct
    
    census <- census %>% 
      left_join(
        metareas,
        by = c("year", "sample", "serial")
      )
    rm(metareas)
    gc()
  }
  
  if (year == 2020) {
    metareas <- haven::read_dta(paste0(path,"/data/2010_2020_met_areas.gz"))
    metareas <- metareas %>% 
      select(year, sample, serial, metarea = met2013) %>% 
      distinct %>% 
      filter(year == 2020)
    
    census <- census %>% 
      left_join(
        metareas %>% select(serial, metarea),
        by = "serial"
      )
    rm(metareas)
    gc()
  }
  
  if (year == 1940) {
    census <- census %>% rename(in_cnonwg = incnonwg)
  }
  census <- census %>% 
    mutate_at(
      vars(matches("^inc"), matches("house_value")),
      ~ sinh(.)
    ) 
  
  # pre-1990 hhld income variable
  if (year %in% c(1950,1960,1970)) {
    census$income <- census$incwage_new + census$incbus_new + census$incother_new
  } else if (year == 1940) {
    census$income <- census$incwage_new
  } else if (year == 1990 | year == 2000 | year == 2020) {
    census$income <- census$incwage_new + census$incbus_new + census$incss_new + census$incwelfr_new + census$incinvst_new + census$incretir_new + census$incother_new
  } else if (year == 2010) {
    #census$income <- census$incwage_new + census$incbus_new + census$incinvst_new + census$incretir_new + census$incother_new
  } 
  
  # calculate inequality at each level
  if (year == 1960 | year >= 1990) {
    levels <- c("puma", "czone", "metarea", "state", "region_label", "country") 
  } else if (year == 1940) {
    levels <- c("czone", "sea", "metarea", "state", "region_label","country")
  } else if (year == 1970) {
    levels <- c("czone", "metarea", "state", "region_label","country")
  } else {
    levels <- c("czone", "state", "region_label","country")
  }
  
  ineq <- vector("list", length(levels))
  
  load(paste0(path,"data/performance_metrics.Rdata"))
  
  if (year == 2010) {
    year_ <- 2020
  } else if (year == 1990) {
    year_ <- 2000
  } else {
    year_ <- year
  }
  thresh <- metrics %>% 
    map(
      `[[`, "clean"
    ) %>%
    reduce(bind_rows) %>% 
    filter(Year == year_) %>% 
    .$Threshold
  
  
  if (year == 1980) {
    thresh <- 0.975 #0.95  
  } else if (year == 2010) {
    thresh <- 0.9
  } else if (year == 1960) {
    thresh <- 0.60
  } else if (year == 1970) {
    thresh <- 0.825
  } else if (year == 1990) {
    thresh <- 0.85
  } else if (year == 2020) {
    thresh <- 0.965 #0.975
  } else if (year == 2000) {
    thresh <- 0.85
  }
  
  
  optim_thresh_0 <- metrics[[as.character(year_)]]$raw$optim_thresh_0 
  
  
  census <- census %>% 
    mutate(
      
      # pred_wealth = case_when(
      #   preds_ens_bin >= thresh ~ exp(preds_ens_pos),
      #   preds_ens_bin < thresh & preds_ens_zero >= optim_thresh_0 ~ 0,
      #   preds_ens_bin < thresh & preds_ens_zero < optim_thresh_0 ~ sinh(preds_ens_neg),
      # ),
      pred_wealth_new = case_when(
        preds_ens_bin >= thresh ~ exp(preds_ens_pos_new),
        preds_ens_bin < thresh & preds_ens_zero >= optim_thresh_0 ~ 0,
        preds_ens_bin < thresh & preds_ens_zero < optim_thresh_0 ~ sinh(preds_ens_neg_new),
      )
    ) 
  
  
  for (l in seq_along(levels)) {
    
    
    if (levels[[l]] == "czone") {
      
      tmp <- census  %>% 
        group_by(
          .data[[ levels[[l]] ]] 
          #,year
        ) %>%
        filter(!is.na(czone))
      
      wght <<- "hwx"
      
    } else if (levels[[l]] == "country") {
      
      tmp <- census %>% 
        distinct(sample, serial, .keep_all = T)
      wght <<- "hhwt"
      
    } else {
      
      # remove extra rows from cz crosswalk
      tmp <- census %>% 
        group_by(
          .data[[ levels[[l]] ]]
        ) %>% 
        distinct(sample, serial, .keep_all = T)
      wght <<- "hhwt"
      
    }
    
    additional_data <- tmp %>%
      summarise(
        # other (non-imputed) characteristics
        college = weighted.mean(ifelse(educ_hrp %in% c("college degree", "master or professional degree","PhD","college"), 1 ,0), na.rm = T, weights=.data[[wght]]),
        white = weighted.mean(ifelse(race == "white", 1 ,0), na.rm = T, weights=.data[[wght]]),
        black = weighted.mean(ifelse(race == "black", 1 ,0), na.rm = T, weights=.data[[wght]]),
        hispanic = weighted.mean(ifelse(race == "hispanic", 1 ,0), na.rm = T, weights=.data[[wght]]),
        age = weighted.mean(age, weights = .data[[wght]], na.rm = T),
        
        incwage_mean = weighted.mean(incwage_new, weights = .data[[wght]], na.rm = T),
        incwage_gini = gini(incwage_new, weights = .data[[wght]]),
        
        income_mean = weighted.mean(income, weights = .data[[wght]], na.rm = T),
        income_gini = gini(income, weights = .data[[wght]]),
        
        
        wealth_0_negative = ifelse(pred_wealth_new <= 0, 1, 0) %>%
          weighted.mean(na.rm = T, weights=.data[[wght]]),
        
        wealth_1_to_24.999k = ifelse(pred_wealth_new >= 1 & pred_wealth_new < 25000, 1, 0) %>%
          weighted.mean(na.rm = T, weights=.data[[wght]]),
        
        wealth_25k_to_99.999k = ifelse(pred_wealth_new >= 25000 & pred_wealth_new < 100000, 1, 0) %>%
          weighted.mean(na.rm = T, weights=.data[[wght]]),
        
        wealth_100k_to_499.999k = ifelse(pred_wealth_new >= 100000 & pred_wealth_new < 500000, 1, 0) %>%
          weighted.mean(na.rm = T, weights=.data[[wght]]),
        
        wealth_over_500k = ifelse(pred_wealth_new >= 500000, 1, 0) %>%
          weighted.mean(na.rm = T, weights=.data[[wght]]),
        
        wealth_1_to_4.999k = ifelse(pred_wealth_new >= 1 & pred_wealth_new < 5000, 1, 0) %>%
          weighted.mean(na.rm = T, weights=.data[[wght]]),
        wealth_5k_to_9.999k = ifelse(pred_wealth_new >= 5000 & pred_wealth_new < 10000, 1, 0) %>%
          weighted.mean(na.rm = T, weights=.data[[wght]]),
        wealth_10k_to_24.999k = ifelse(pred_wealth_new >= 10000 & pred_wealth_new < 25000, 1, 0) %>%
          weighted.mean(na.rm = T, weights=.data[[wght]]),
        wealth_25k_to_49.999k = ifelse(pred_wealth_new >= 25000 & pred_wealth_new < 50000, 1, 0) %>%
          weighted.mean(na.rm = T, weights=.data[[wght]]),
        wealth_50k_to_99.999k = ifelse(pred_wealth_new >= 50000 & pred_wealth_new < 100000, 1, 0) %>%
          weighted.mean(na.rm = T, weights=.data[[wght]]),
        wealth_100k_to_249.999k = ifelse(pred_wealth_new >= 100000 & pred_wealth_new < 250000, 1, 0) %>%
          weighted.mean(na.rm = T, weights=.data[[wght]]),
        wealth_250k_to_499.999k = ifelse(pred_wealth_new >= 250000 & pred_wealth_new < 500000, 1, 0) %>%
          weighted.mean(na.rm = T, weights=.data[[wght]])
      ) %>%
      ungroup 
    
    if (levels[[l]] == "country") {
      
      additional_data <- additional_data %>%
        cbind(
          tmp %>%
            distinct(sample,serial, .keep_all = T) %>%
            summarise(households = n()) %>%
            mutate(year = year) %>%
            ungroup
        ) %>%
        ungroup
      
    } else {
      additional_data <- additional_data %>%
        left_join(
          tmp %>%
            distinct(sample,serial, .keep_all = T) %>%
            summarise(households = n()) %>%
            mutate(year = year) %>%
            ungroup
        ) %>%
        ungroup
      
    }
    
    if (year >= 1980) {
      additional_data <- additional_data %>%
        left_join(
          tmp %>%
            summarise(
              house_value = weighted.mean(house_value, na.rm = T, w = .data[[wght]]),
              own_outright = weighted.mean(ifelse(house_tenure == "own_outright", 1, 0), na.rm = T, w = .data[[wght]]),
              own_mortgage = weighted.mean(ifelse(house_tenure == "own_mortgage", 1, 0), na.rm = T, w = .data[[wght]]),
              rent = weighted.mean(ifelse(house_tenure == "rent", 1, 0), na.rm = T, w = .data[[wght]])
            ) %>%
            mutate(
              year = year,
              own = own_outright + own_mortgage
            ) %>%
            ungroup
        )
    } else if (year %in% c(1940,1960,1970)) {
      additional_data <- additional_data %>%
        left_join(
          tmp %>%
            summarise(
              house_value = weighted.mean(house_value, na.rm = T, w = .data[[wght]]),
              own = weighted.mean(ifelse(house_tenure == "own", 1, 0), na.rm = T, w = .data[[wght]]),
              rent = weighted.mean(ifelse(house_tenure == "rent", 1, 0), na.rm = T, w = .data[[wght]])
            ) %>%
            mutate(year = year) %>%
            ungroup
        )
    }
    
    if (short) {
      tmp <- tmp %>% 
        summarise_at(
          
          vars(
            pred_wealth, 
            income,
            pred_wealth_new
          ),
          
          list(
            #weighted calculations
            mean = ~ weighted.mean(., w = .data[[wght]], na.rm = T),
            median = ~ Hmisc::wtd.quantile(., probs = 0.5, weights = .data[[wght]], na.rm = T),
            sd = ~ Hmisc::wtd.var(., weights = .data[[wght]]) %>% sqrt(), 
            gini = ~ gini(., weights = .data[[wght]]),
            
            gini_unweighted = ~ gini(.),
            gini99 = ~ gini(
              .[. <= quantile(., probs = 0.99)], .data[[wght]]
            ),
            gini90 = ~ gini(
              .[. <= quantile(., probs = 0.9)], .data[[wght]]
            ), 
            
            top.01 = ~ Top.01(., w = .data[[wght]]),
            top.1 = ~ Top.1(., w = .data[[wght]]),
            top1 = ~ Top1(., w = .data[[wght]]),
            top10 = ~ Top10(., w = .data[[wght]]),
            bottom50 = ~ Bottom50(., w = .data[[wght]]),
            
            nine_ten = ~ nine_ten(., w = .data[[wght]]),
            nine_five = ~ nine_five(., w = .data[[wght]]),
            five_ten = ~ five_ten(., w = .data[[wght]])
          ) 
        ) %>%
        ungroup %>% 
        mutate_if(
          is.numeric,
          ~ ifelse(. == Inf | . == -Inf | is.nan(.), NA, .)
        ) 
      
      if (levels[[l]] == "country") {
        ineq[[l]] <- tmp %>% 
          cbind(additional_data) %>% 
          select(
            any_of(c("state","czone")), 
            pred_wealth_new_gini,
            everything()
          ) %>% 
          mutate(
            year = year
          ) %>% 
          relocate(
            year, 1
          )
      } else {
        ineq[[l]] <- tmp %>% 
          left_join(additional_data) %>% 
          select(
            any_of(c("state","czone")), 
            pred_wealth_new_gini,
            everything()
          ) %>% 
          mutate(
            year = year
          ) %>% 
          relocate(
            year, 1
          )
        
      }
      
      # end short ineq calc  
    } else {
      tmp <- tmp %>% ungroup
      
      # loop by sub-national areas
      areas <- tmp[[ levels[[l]] ]] %>% unique
      areas <- areas[!is.na(areas)]
      
      if (length(areas) == 0) {
        area_list <- vector("list",length = 1)
      } else {
        area_list <- vector("list",length(areas))
      }
      
      for (a in seq_along(area_list)) {
        if (levels[[l]] != "country") {
          x <- tmp %>% 
            filter(.data[[levels [[l]]]] == areas[[a]]) 
        } else {
          x <- tmp 
        }
        
        
        # calculate uncertainty bands 
        set.seed(a*1283)
        
        fcts <- c(gini, Top1, Top10, Bottom50, weighted.mean)
        errors <- vector("list", length(fcts))
        for (fc in seq_along(fcts)) {
          errors[[fc]] <- boot(
            data = x[, "pred_wealth_new", drop = FALSE],
            statistic = boot.fun,
            R = 100,
            j = x[, wght , drop = FALSE],
            var = "pred_wealth_new",
            fct = fcts[[fc]]
          ) %>% 
            .$t %>% 
            quantile(probs = c(0.025, 0.5, 0.975)) 
        }
        
        errors <- errors %>% 
          bind_rows %>% 
          rename_all(
            ~ paste0("q",.) %>% 
              str_replace_all("%","")
          ) %>% 
          mutate(
            stat = c("gini", "Top1", "Top10", "Bottom50", "Mean")
          ) %>% 
          pivot_wider(
            names_from = stat, 
            values_from = 1:3
          )
        
        
        x <- x %>% 
          summarise_at(
            
            vars(
              pred_wealth,
              income,
              pred_wealth_new
            ),
            
            list(
              #weighted calculations
              mean = ~ weighted.mean(., w = .data[[wght]], na.rm = T),
              median = ~ Hmisc::wtd.quantile(., probs = 0.5, weights = .data[[wght]], na.rm = T),
              sd = ~ Hmisc::wtd.var(., weights = .data[[wght]]) %>% sqrt(), 
              gini = ~ gini(., weights = .data[[wght]]),
              gini_unweighted = ~ gini(.),
              gini99 = ~ gini(
                .[. <= quantile(., probs = 0.99)], .data[[wght]]
              ),
              gini90 = ~ gini(
                .[. <= quantile(., probs = 0.9)], .data[[wght]]
              ), 
              
              top.01 = ~ Top.01(., w = .data[[wght]]),
              top.1 = ~ Top.1(., w = .data[[wght]]),
              top1 = ~ Top1(., w = .data[[wght]]),
              top10 = ~ Top10(., w = .data[[wght]]),
              bottom50 = ~ Bottom50(., w = .data[[wght]]),
              
              nine_ten = ~ nine_ten(., w = .data[[wght]]),
              nine_five = ~ nine_five(., w = .data[[wght]]),
              five_ten = ~ five_ten(., w = .data[[wght]])
            ) 
          ) %>%
          ungroup %>% 
          cbind(
            errors
          ) %>%
          mutate_if(
            is.numeric,
            ~ ifelse(. == Inf | . == -Inf | is.nan(.), NA, .)
          ) 
        
        x[[ levels [[l]] ]] <- areas[[a]]
        
        
        if (levels[[l]] == "country") {
          area_list[[a]] <- x %>% 
            cbind(additional_data) %>% 
            select(
              any_of(c("state","czone")), 
              matches("pred_wealth_new"),
              everything()
            ) %>% 
            mutate(
              year = year
            ) %>% 
            relocate(
              year, 1
            )
        } else {
          area_list[[a]] <- x %>% 
            left_join(additional_data) %>% 
            select(
              any_of(c("state","czone")), 
              matches("pred_wealth_new"),
              everything()
            ) %>% 
            mutate(
              year = year
            ) %>% 
            relocate(
              year, 1
            )
          
        }
        
      }
      
      ineq[[l]] <- area_list %>%
        reduce(rbind)
    }
  }
  
  # name level
  names(ineq) <- levels
  
  save(
    ineq, 
    file = paste0(
      path,"data/inequality_", year, ".Rdata"
    )
  )
  
  message("inequality for ", year, " calculated")
  return(ineq)
}



