
# wrap loop in function for speed gains
adjust <- function(vars_adj, data, year) {
  
  for (i in seq_along(vars_adj)) {
    
    # if variable is missing
    if ( is.null(data[[ vars_adj[[i]] ]]) ) {
      next
    }
    
    # lookup for topcode and adjusted topcode (new max)
    var_sel <- paste0(vars_adj[[i]], "_topcode")
    var_sel_new <- paste0(vars_adj[[i]], "_topcode_new")
    
    # pull out rows in census which are topcoded
    ## (since income lines are aggregated, need not set relate == 1
    ## (~33% (in 2019) of top coded values are not relate == 1
    
    if (vars_adj[[i]] %in% c("house_value","proptx","mortamt1","mortamt2","mortotal")) {
      df_eligible <- data %>% 
        filter(
          relate == 1, 
          str_detect(house_tenure, "^own")
        ) 
      
    } else if (vars_adj[[i]] == "rent") {
      
      df_eligible <- data %>% 
        filter(
          relate == 1, 
          str_detect(house_tenure, "^rent")
        ) 
      
    } else { # if income variable don't take only RELATE==1
      
      df_eligible <- data
      
    }
    
    if (year <= 1980) {
      df_eligible <- df_eligible %>% 
        # add in topcodes for relevant variable
        left_join(
          topcodes_new %>% 
            select(
              year,sample,
              any_of(var_sel)
            ) %>% 
            mutate(sample = as.numeric(sample)), 
          by = c("year","sample")
        ) %>% 
        # filter those at topcode
        filter(
          .data[[ vars_adj[[i]] ]] >= .data[[var_sel]] 
        )
    } else {
      df_eligible <- df_eligible %>% 
        # add in topcodes for relevant variable
        left_join(
          topcodes_new %>% 
            select(
              statefip,year,sample,
              any_of(var_sel)
            ) %>% 
            mutate(sample = as.numeric(sample)), 
          by = c("year","sample","statefip")
        ) %>% 
        # filter those at topcode
        filter(
          .data[[ vars_adj[[i]] ]] >= .data[[var_sel]] 
        )
    }
    
    
    if (nrow(df_eligible) == 0) {
      next
    }
    
    # remove the obs topcoded from data, add back in after adjustment
    data <- data %>% 
      anti_join(df_eligible)
    
    if (year > 1980) {
      # loop through states
      state_fips <- unique(topcodes_new$statefip)
      state_list <- vector("list", length(state_fips))
      for (s in seq_along(state_list)) {
        
        # pull out rows for specific state s
        x <- df_eligible %>% 
          filter(
            statefip == state_fips[[s]]
          )
        
        # new maximum value
        y <- topcodes_new %>% 
          filter(statefip == state_fips[[s]])
        
        # estimate pareto parameters using SCF observations 
        # for observations between topcode and new max
        # https://search.r-project.org/CRAN/refmans/EnvStats/html/epareto.html
        
        # some income vars need to be put back in to original values
        samples <- unique(x$sample)
        year_list <- vector("list",length(samples)) 
        
        for (yr in seq_along(year_list)) {
          
          x_tmp <- x %>% filter(as.numeric(sample) == samples[[yr]])
          y_tmp <- y %>% filter(as.numeric(sample) == samples[[yr]])
          
          if (!(vars_adj[[i]] %in% c("proptx","rent","mortamt1","mortamt2","mortotal"))) {
            dist <- scf_tidy %>% 
              filter(
                # take the values between topcode and new maximum value from SCF to estimate parameters of Pareto
                .data[[ vars_adj[[i]]]] >= asinh( y_tmp[[var_sel]] ) &
                  .data[[ vars_adj[[i]] ]] <= asinh( y_tmp[[var_sel_new]] )#,
                #year >= year_lower & year <= year_upper
              ) %>% 
              .[[ vars_adj[[i]] ]] %>% 
              sinh
          } else {
            
            dist <- scf_tidy %>% 
              filter(
                # take the values between topcode and new maximum value from SCF to estimate parameters of Pareto
                .data[[ vars_adj[[i]]]] >= min(y[[var_sel]]) &
                  .data[[ vars_adj[[i]] ]] <= max(y[[var_sel_new]])#,
                #year >= year_lower & year <= year_upper
              ) %>% 
              .[[ vars_adj[[i]] ]] 
            
          }
          
          # if more than one obs needs adjusting and new max is greater than topcode
          if (nrow(x) > 0 && length(dist) > 1 && y[[var_sel_new]] > y[[var_sel]]) {
            
            # esimate parameters
            pareto_params <- epareto(dist)$parameters
            
            # reproducibility
            set.seed(s*1808)
            
            # create Pareto distribution to sample from 
            pareto_dist <- rparetoTrunc(
              n = nrow(x_tmp)*10,
              scale = pareto_params[["location"]],
              shape = pareto_params[["shape"]],
              lower_bound = y_tmp[[var_sel]],
              upper_bound = y_tmp[[var_sel_new]]
            )
            
            # define new variable in census with adjusted topcodes
            x_tmp[[ paste0(vars_adj[[i]], "_new") ]] <- sample(pareto_dist, nrow(x_tmp), replace = T)
            
            year_list[[yr]] <- x_tmp
          }
        }
        
        x <- year_list %>%
          bind_rows()
        
        
        
        state_list[[s]] <- x
        
      }
      
      state_list <- state_list %>% 
        reduce(bind_rows)
      
      # add rows back in to census
      data <- data %>% 
        full_join(state_list)
      
    } else { # if year less than 1980 (and topcodes are not by state)
      
      # some income vars need to be put back in to original values
      samples <- unique(df_eligible$sample)
      year_list <- vector("list",length(samples)) 
      
      for (yr in seq_along(year_list)) {
        
        x <- df_eligible %>% filter(as.numeric(sample) == samples[[yr]])
        y <- topcodes_new %>% filter(as.numeric(sample) == samples[[yr]])
        
        # if variables are transformed
        if (!(vars_adj[[i]] %in% c("proptx","rent","mortamt1","mortamt2","mortotal"))) { 
          dist <- scf_tidy %>% 
            filter(
              # take the values between topcode and new maximum value from SCF to estimate parameters of Pareto
              .data[[ vars_adj[[i]]]] >= asinh( y[[var_sel]] ) &
                .data[[ vars_adj[[i]] ]] <= asinh( y[[var_sel_new]] )#,
              #year >= year_lower & year <= year_upper
            ) %>% 
            .[[ vars_adj[[i]] ]] %>% 
            sinh
        } else if (vars_adj[[i]] == "mortotal") {
          
          dist <- scf_tidy %>%
            mutate(mortotal = mortamt1 + mortamt2) %>% 
            filter(
              # take the values between topcode and new maximum value from SCF to estimate parameters of Pareto
              .data[[ vars_adj[[i]]]] >= y[[var_sel]] &
                .data[[ vars_adj[[i]] ]] <= y[[var_sel_new]]#,
              #year >= year_lower & year <= year_upper
            ) %>% 
            .[[ vars_adj[[i]] ]] 
          
        } else {
          
          dist <- scf_tidy %>% 
            filter(
              # take the values between topcode and new maximum value from SCF to estimate parameters of Pareto
              .data[[ vars_adj[[i]]]] >= y[[var_sel]] &
                .data[[ vars_adj[[i]] ]] <= y[[var_sel_new]]#,
              #year >= year_lower & year <= year_upper
            ) %>% 
            .[[ vars_adj[[i]] ]] 
          
        }
        
        # if more than one obs needs adjusting and new max is greater than topcode
        if (nrow(x) > 0 && length(dist) > 1 && y[[var_sel_new]] > y[[var_sel]]) {
          
          # esimate parameters
          pareto_params <- epareto(dist)$parameters
          
          # reproducibility
          set.seed(yr*1808)
          
          # create Pareto distribution to sample from 
          pareto_dist <- rparetoTrunc(
            n = nrow(x)*10,
            scale = pareto_params[["location"]],
            shape = pareto_params[["shape"]],
            lower_bound = y[[var_sel]],
            upper_bound = y[[var_sel_new]]
          )
          
          # define new variable in census with adjusted topcodes
          x[[ paste0(vars_adj[[i]], "_new") ]] <- sample(pareto_dist, nrow(x), replace = T)
          
          year_list[[yr]] <- x
        }
      }
      
      # add rows back in to census
      data <- data %>% 
        full_join(x)
      
    }
    
     
    # replace NAs in _new variable with non-adjusted values 
    data[[ paste0(vars_adj[[i]], "_new") ]] <- ifelse( 
      is.na( data[[ paste0(vars_adj[[i]], "_new") ]] ), 
      data [[ vars_adj[[i]] ]], 
      data[[ paste0(vars_adj[[i]], "_new") ]] 
      )
    
    
    message(vars_adj[[i]], " done")
  }
  
  return(data)
  
}


