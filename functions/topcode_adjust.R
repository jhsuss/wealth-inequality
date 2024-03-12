
adjust <- function(vars_adj, data, year) {
  
  df_eligible <- data
  
  # lookup for topcode and adjusted topcode (new max)
  var_sel <- paste0(vars_adj, "_topcode")
  
  # distribution from SCf for estimating pareto parameter
  vars_to_untransform <<- c("house_value","incwage","incbus","incss",
                            "incwelfr","incinvst","incretir","incother")
  
  # scf tidy variable names change pre 1989
  if (year < 1989) {
    scf_tidy <- scf_tidy %>% 
      mutate(
        incwage = ifelse(is.na(incwage), asinh(incws), incwage),
        incinvst = ifelse(is.na(incinvst), asinh(inccap), incinvst)
      )
  }
  
  # for some census years, take adjoining SCF years for distribution
  if (year == 1980 || year < 1980) {
    years_ <-  c(1989,1990,1992)
  } else if (year == 1990) {
    years_ <-  c(1989,1990,1992)
  } else if (year == 1940) {
    years_ <-  c(1949,1950,1951)
  } else{
    years_ <- years
  }
  
  if (vars_adj %in% vars_to_untransform) {
    dist <- scf_tidy %>% 
      filter(
        year <= max(years_) & year >= max(years_) - 5,
        sinh(.data[[ vars_adj ]]) >= threshold
      ) %>% 
      select(
        any_of(vars_adj),
        wgt 
      ) %>% 
      mutate_at(
        vars(any_of( vars_adj )),
        ~ sinh(.)
      )
  } else {
    
    if (year == 1980 && vars_adj == "mortotal") {
      scf_tidy <- scf_tidy %>% 
        mutate(
          mortotal = mortamt1 + mortamt2
        )
    }
    
    dist <- scf_tidy %>% 
      filter(
        # five year window
        year <= max(years_) & year >= max(years_) - 5,
        .data[[ vars_adj ]] >= threshold
      ) %>% 
      select(
        any_of(vars_adj),
        wgt 
      )
  }
  
  
  # estimate Pareto parameter (max-likelihood) with weights   
  if (nrow(dist) < 50) { # if small n, assume 1,5
    pareto_params <- list(
      "pseudoMLE" = 1.5,
      1.5
      )
  } else {
    
    source("functions/pareto_tail_functions.R")
    
    pareto_params <- ml.pareto(
      x = dist %>% rename(wealth = 1, weight = 2),
      wmin = threshold
    )
    names(pareto_params)[[1]] <- "pseudoMLE" 
    
  }
  
  
  # new max = max from SCF
  overall_max <- dist[[ vars_adj ]] %>% max
  
  set.seed(274)
  pareto_dist <- rparetoTrunc(
    n = nrow(df_eligible),
    scale = threshold,
    shape = pareto_params[["pseudoMLE"]], # pareto parameter estimated from SCF distribution
    lower_bound = threshold, # what observation to start sampling from 
    upper_bound = overall_max # new topcode
  )
  
  dist_check <- T
  
  if (dist_check) {
    library(GB2)
    # estimate generalised beta distribution of the second kind
    gb2_params <- GB2::ml.gb2(
      dist[[vars_adj]], 
      w=dist$wgt, 
      method=1, 
      hess=FALSE
    ) 
    # https://rdrr.io/cran/GB2/man/gb2.html
    gb2_dist <- GB2::rgb2(
      n = nrow(df_eligible), 
      shape1 = gb2_params$opt1$par[[1]], 
      scale = gb2_params$opt1$par[[2]], 
      shape2 = gb2_params$opt1$par[[3]], 
      shape3 = gb2_params$opt1$par[[4]]
    )
    
    
    # which distribution most closely fits the distribution in scf?
    ks_pareto <- ks.test(dist[[vars_adj]], pareto_dist)
    ks_gb2 <- ks.test(dist[[vars_adj]], gb2_dist)
    ks_lognorm <- ks.test(dist[[ vars_adj ]],
                          rlnormTrunc(
                            nrow(dist),
                            log(mean(dist[[ vars_adj ]])),
                            log(sd(dist[[ vars_adj ]]))
                            ,min = threshold,
                            max = overall_max
                          )
    )
    
    ks_stats <- c(
      ks_pareto$statistic, 
      ks_gb2$statistic,
      ks_lognorm$statistic
    )
    names(ks_stats) <- c("pareto","gb2","lognorm")
    
    if (which.min(ks_stats) == 1) {
      message("Pareto best fit for data")
      best_fit <- "pareto"
    } else {
      best_fit <- "not_pareto"
      message("Pareto NOT best fit for data")
    }
    
    ks_stats <- ks_stats %>% 
      as_tibble() %>% 
      mutate(
        dist = c("pareto","gb2","lognorm"),
        p_value = c(
          ks_pareto$p.value,
          ks_gb2$p.value,
          ks_lognorm$p.value
        ),
        n_scf = nrow(dist),
        year = year,
        var = vars_adj,
        best_fit
      ) 
  }
  
  # get mod year 
  if (year >= 2008) {
    year_ <- 2020
  } else if (year >= 1990 && year <= 2007) {
    year_ <- 2000
  } else if (year == 1940) {
    year_ <- 1950
  } else {
    year_ <- year
  }
  mod_file <- paste0(
    "topcode_adjust_model_",vars_adj,"_",year,".Rdata"
  )
  mod_files <- list.files("models/", "^topcode_adjust_model")
  # fit ranking model if model doesn't exit in directory
  if (mod_file %in% mod_files) {
    library(caret)
    load(paste0("models/", mod_file))
  } else {
    var_mod <<- vars_adj
    source("scripts/topcode_adjust_models.R")
  }
  gc()
  
  #### predict rank of topcoded observations
  source("functions/one_hot_encoding.R")
  tmp <- df_eligible %>%
    select(any_of(names(test_df))) %>%
    mutate_at(vars(matches("state")), ~ str_to_title(.)) %>% 
    one_hot_encoding(
      test_df %>%
        select_if(is.character) %>%
        names
    ) %>% 
    select(any_of(mod_xgb_rank$feature_names)) 
  
  # some missing cols that should be set to 0
  missing_cols <- mod_xgb_rank$feature_names[!(mod_xgb_rank$feature_names %in% names(tmp))]
  
  for (i in seq_along(missing_cols)) {
    tmp[[ missing_cols[[i]] ]] <- 0
  }
  
  tmp <- select(tmp, any_of(mod_xgb_rank$feature_names))
  
  
  ranks <- matrix(
    nrow = nrow(tmp),
    ncol = 10
  )
  for (r in 1:ncol(ranks)) {
    ranks[,r] <- predict(
      mods_rank[[r]],
      tmp %>%
        as.matrix()
    )

  }
  # rank averages over 10 ranking model outputs
  predicted_rank <- ranks %>%
    scale %>% 
    apply(1,mean)
  
  df_eligible <- df_eligible %>% cbind(predicted_rank) 
  
  rank_check <- T
  # check how much geog matters for rank
  if (rank_check) {
    
    mod_rank_state <- lm(data = df_eligible, predicted_rank ~ state) %>% summary
    
    if (year >= 2012) {
      df_eligible <- df_eligible %>%
        mutate(
          puma_fip = ifelse(nchar(puma) == 3, paste0("00",puma), puma),
          puma_fip = ifelse(nchar(puma_fip) == 4, paste0("0",puma_fip), puma_fip),
          puma_fip = str_c(
            str_sub(statefip,1,2),
            str_sub(puma_fip,-5,-1)
          )
        )
      
    } else if (year == 1960 || year >= 2000 && year <= 2011) {
      df_eligible <- df_eligible %>%
        mutate_at(
          vars(statefip, puma),
          ~ as.numeric(.)
        ) %>% 
        mutate(puma_fip = statefip*10000+puma)
    } 
    if (year == 1950) {
      df_eligible <- df_eligible %>% 
        mutate(
          puma_fip = 1000 * sea
        )
    }
    if (year == 1970) {
      df_eligible <- df_eligible %>% 
        mutate(
          puma_fip = 1000 * statefip + cntygp97
        )
    }
    if (year == 1980) {
      df_eligible <- df_eligible %>% 
        mutate(
          puma_fip = 1000 * statefip + cntygp98
        )
    }
    
    if (year == 1990) {
      df_eligible <- df_eligible %>% 
        mutate(
          puma_fip = ifelse(nchar(puma) == 3, paste0("0",puma), puma),
          puma_fip = str_c(
            str_sub(statefip,1,2),
            str_sub(puma_fip,-4,-1)
          )
        )
    }
    
    mod_rank_puma <- lm(data = df_eligible, predicted_rank ~ factor(puma_fip)) %>% summary
    
    rank_eval <- tribble(
      ~ "level", ~ "adj_rsq", ~ "f_stat",
      "state", mod_rank_state$adj.r.squared, mod_rank_state$fstatistic[[1]],
      "puma", mod_rank_puma$adj.r.squared, mod_rank_puma$fstatistic[[1]],
    ) %>% 
      mutate(
        year = year,
        var = vars_adj
      )
  }
  
  rm(tmp)
  
  ## now adjust by state if year > 1980 (house_value post-2010)
  ## due to state-specific means
  if (year <= 1980 || year <= 2000 && vars_adj == "house_value") {
    
    # and arrange in descending order the fitted pareto distribution
    pareto_dist <- pareto_dist %>% sort %>% rev
    
    df_eligible <- df_eligible %>% 
      arrange(desc(predicted_rank))
    
    if (!(vars_adj %in% c("mortamt1","mortamt2","mortotal","proptx", "rent"))) {
      pareto_dist <- asinh(pareto_dist)
    } 
    
    # define new variable in census with adjusted topcodes
    df_eligible[[ paste0(vars_adj, "_new") ]] <- pareto_dist
    
    evals <- NA
    
  } else {
    # loop through states
    state_fips <- unique(topcodes_new$statefip)
    state_list <- evals <- vector("list", length(state_fips))
    for (s in seq_along(state_list)) {
      
      # pull out rows for specific state s
      x <- df_eligible %>% 
        filter(
          statefip == state_fips[[s]]
        )
      
      if (nrow(x) == 0) {
        next
      }
      
      samples <- unique(x$sample) %>% as.numeric
      year_list <- vector("list",length(samples))
      evals_year <- vector("list",length(samples))
      for (yr in seq_along(year_list)) {
        
        x_tmp <- x %>% filter(sample == samples[[yr]])
        
        if (nrow(x_tmp) == 0) {
          next
        }
        
        # find pareto tail parameter & max for state and year that results in mean that is closest to census mean
        target_mean <- topcodes_new %>% 
          filter(
            sample == samples[[yr]], 
            statefip == state_fips[[s]]
          ) %>% 
          distinct(
            statefip,
            .data[[ var_sel ]]
          ) %>% 
          .[[ var_sel ]]
        
        # state-specific threshold where available
        if (!is.null(topcodes_new$threshold)) {
          thresh <- topcodes_new %>% 
            filter(
              sample == samples[[yr]],
              statefip == state_fips[[s]]
            ) %>% 
            .$threshold
        } else {
          thresh <- threshold
        }
        
        
        fun <- function(params, N = 1e4) {
          
          pareto_tail <- params[[1]]
          dist_max <- params[[2]]
          dist_max <- exp(dist_max)
          
          temp <- rparetoTrunc(
            n = N,
            scale = thresh,
            shape = pareto_tail, # pareto parameter estimated from SCF distribution
            lower_bound = thresh, # what observation to start sampling from 
            upper_bound = dist_max # new topcode
          ) 
          
          xdist <- sqrt((mean(temp) - target_mean)^2)
          
          return(xdist)
        }
        
        if (str_detect(vars_adj, "^(inc|house_value)")) {
          step_size <- 0.1
        } else {
          step_size <- 1e3
        }
        
        set.seed(s*123)
        o <- optim(
          c(
            # start with parameters estimated from SCF 
            pareto_tail = pareto_params[[2]], 
            dist_max = log(overall_max)
          ), 
          fun,
          lower = c(0.25,log(thresh)), # lower constraint
          method = "L-BFGS-B",
          control = list(
            ndeps = c(0.1,step_size), # step size 
            maxit = 1e4 # max iterations
          )
        )
        
        pareto_optim <- matrix(
          nrow = 100, 
          ncol = nrow(x_tmp)
        )
        set.seed(s*123)
        for (r in 1:nrow(pareto_optim)) {
          pareto_optim[r,] <- rparetoTrunc(
            n = nrow(x_tmp),
            scale = thresh,
            shape = o$par[[1]], 
            lower_bound = thresh, 
            upper_bound = exp(o$par[[2]]) 
          ) %>% 
            sort %>% 
            rev 
          
        }
        pareto_optim <- pareto_optim %>% 
          apply(2, mean)
        
        pareto_optim <- pareto_optim %>% sort %>% rev
        
        x_tmp <- x_tmp %>% 
          arrange(desc(predicted_rank))
        
        
        # define new variable in census with adjusted topcodes
        if (vars_adj %in% c("mortamt1","mortamt2","mortotal","proptx", "rent")) {
          x_tmp[[ paste0(vars_adj, "_new") ]] <- pareto_optim
        } else {
          x_tmp[[ paste0(vars_adj, "_new") ]] <- asinh(pareto_optim)
        }
        
        
        
        evals_year[[yr]] <- tibble(
          n = nrow(x_tmp),
          year = x_tmp$year %>% unique %>% as.numeric(),
          sample = x_tmp$sample %>% unique %>% as.numeric(),
          statefip = state_fips[[s]],
          var = vars_adj,
          estimated_pareto_tail = o$par[[1]],
          estimated_max = exp(o$par[[2]]),
          location = thresh,
          sample_max = pareto_optim %>% max,
          sample_mean = pareto_optim %>% mean,
          target_mean, 
          error = o$value
        )
        
        year_list[[yr]] <- x_tmp
        
      }
      
      x <- year_list %>%
        bind_rows()
      
      state_list[[s]] <- x
      
      evals[[s]] <- evals_year %>% bind_rows()
    }
    
    df_eligible <- state_list %>% 
      reduce(bind_rows)
    
    evals <- evals %>% 
      reduce(bind_rows) 
    
    evals <- evals %>% 
      mutate(state = haven::as_factor(statefip)) %>% 
      relocate(
        state,
        .after = statefip
      )
    
  }
  
  df_eligible[[var_sel]] <- NULL
  
  # for eval of predicted rank
  predicted_rank <- df_eligible %>% 
    select(sample, serial, predicted_rank)
  
  df_eligible[["predicted_rank"]] <- NULL
  df_eligible[["threshold"]] <- NULL
  
  pareto_tail <- tibble(
    pareto_tail = pareto_params$pseudoMLE,
    var = vars_adj,
    year = year
  )
  save(
    evals,rank_eval,ks_stats,
    pareto_tail,predicted_rank,
    file = paste0("data/eval_", vars_adj, "_", year, ".Rdata")
  )
  
  
  
  message(vars_adj, " done")
  
  return(
    list(
      df_eligible = df_eligible
    )
  )
  
}


