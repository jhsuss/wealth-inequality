
# identify households that have top-coded value in census / eligible for adjustment
eligible <- function(vars_adj, data, year) {
  
  topcodes_new <- topcodes %>% 
    filter(
      year %in% years
    ) %>% 
    select(
      year:statefip,
      matches( vars_adj )
    )
  
  if (year == 1990 && vars_adj == "rent") {
    topcodes_new <- topcodes_new %>% 
      mutate(
        rent_topcode = ifelse(is.na(rent_topcode),max_rentgrs, rent_topcode)
      )
  }
  
  if (year >= 1990 && vars_adj == "proptx") {
    topcodes_new <- topcodes_new %>% 
      left_join(
        proptx_lookup, 
        by = c("proptx_topcode" = "code")) %>% 
      select(-proptx_topcode) %>%
      rename(proptx_topcode = proptx)
  }
  
  
  topcodes_new <- topcodes_new %>%
    left_join(cpi, by = "year") %>% 
    mutate_at(
      vars(matches(vars_adj)),
      ~ . * cpi_2019
    ) %>% 
    distinct()
  
  
  # BOTTOM CODES -- decision made not to adjust these given small N of bottom-coded households
  if (vars_adj %in% c("incinvst", "incwage", "incbus")) {
    
    bottomcodes <- topcodes_new %>% 
      select(year:statefip, matches("bottomcode"))
  }
  
  # lookup for topcode and adjusted topcode (new max)
  var_sel <- paste0(vars_adj, "_topcode")
  
  # if housing variable, only need to adjust at household-level 
  # if income, need to adjust by individuals before aggregating income to household level
  if (vars_adj %in% c("house_value","proptx","mortamt1","mortamt2","mortotal")) {
    df_eligible <- data %>% 
      filter(
        relate == 1, 
        str_detect(house_tenure, "^own")
      ) 
    
  } else if (vars_adj == "rent") {
    
    df_eligible <- data %>% 
      filter(
        relate == 1, 
        str_detect(house_tenure, "^rent")
      ) 
    
  } else { # if income variable don't take only RELATE==1
    
    df_eligible <- data
    
  }
  
  
  #### isolate the affected population
  vars_adj_means <<- c(
    "incinvst", "incwage", "incwelfr", 
    "incretir","incsupp",
    "incother","incss","incbus",
    "rent","mortamt1","mortamt2"
  )
  # pre-1980 the topcode is national
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
        .data[[ vars_adj ]] >= .data[[var_sel]] 
      )
    
    
    threshold <- topcodes_new[[var_sel]] %>% unique
    
    # after 1980 it is by state, but in some years (post-2000), 
    # the threshold above which values are topcoded is not given
    # so we infer based on the below topcode max
  } else if (year > 2000 && vars_adj %in% vars_adj_means) {
    
    topcodes_new <- data %>% 
      left_join(
        topcodes_new %>% 
          select(
            statefip,year,sample,
            any_of(var_sel)
          ) %>% 
          mutate(sample = as.numeric(sample)), 
        by = c("year","sample","statefip")
      ) %>% 
      group_by(
        sample, statefip, year,
        .data[[ var_sel ]]
      ) %>% 
      filter(
        .data[[ vars_adj ]] < .data[[ var_sel ]]
      ) %>% 
      summarise(
        threshold = max(.data[[ vars_adj ]])
      ) %>% 
      ungroup
    
    if (vars_adj %in% c("rent","mortamt1","mortamt2")) {
      
      df_eligible <- df_eligible %>% 
        left_join(
          topcodes_new %>% 
            select(
              statefip,year,sample,
              any_of(var_sel), 
              matches("threshold")
            ) %>% 
            mutate(sample = as.numeric(sample)), 
          by = c("year","sample","statefip")
        ) %>% 
        filter(
          .data[[ vars_adj ]] > threshold
        )
      
    } else {
      
      df_eligible <- data %>% 
        left_join(
          topcodes_new %>% 
            select(
              statefip,year,sample,
              any_of(var_sel), 
              matches("threshold")
            ) %>% 
            mutate(sample = as.numeric(sample)), 
          by = c("year","sample","statefip")
        ) %>% 
        filter(
          .data[[ vars_adj ]] > threshold
        )
      
    }
    
    threshold <- df_eligible$threshold %>% min
    
    
  } else if (year %in% c(1990,2000) && vars_adj %in% vars_adj_means) {
    
    # where threshold is given, above which mean or median used by state
    if (vars_adj == "incinvst") {
      if (year == 1990) { threshold <- 40000 * cpi$cpi_2019} else {threshold <- 50000 * cpi$cpi_2019}
    } else if (vars_adj == "incwage") {
      if (year == 1990) { threshold <- 140000 * cpi$cpi_2019} else {threshold <- 175000 * cpi$cpi_2019}
    } else if (vars_adj == "incwelfr") {
      if (year == 1990) { threshold <- 10000 * cpi$cpi_2019} else {threshold <- 12300 * cpi$cpi_2019}
    } else if (vars_adj == "incretir") {
      if (year == 1990) { threshold <- 30000 * cpi$cpi_2019} else {threshold <- 52000 * cpi$cpi_2019}
    } else if (vars_adj == "incsupp") {
      if (year == 1990) { threshold <- 10000 * cpi$cpi_2019} else {threshold <- 12300 * cpi$cpi_2019}
    } else if (vars_adj == "incother") {
      if (year == 1990) { threshold <- 20000 * cpi$cpi_2019} else { threshold <- 37800 * cpi$cpi_2019} 
    } else if (vars_adj == "incss") {
      if (year == 1990) { threshold <- 17000 * cpi$cpi_2019} else {threshold <- 18000 * cpi$cpi_2019}
    } else if (vars_adj == "incbus") {
      if (year == 1990) { threshold <- 90000 * cpi$cpi_2019} else {threshold <- 126000 * cpi$cpi_2019}
    } else if (vars_adj == "rent") {
      if (year == 1990) { threshold <- 1000 * cpi$cpi_2019} else {threshold <- 1700 * cpi$cpi_2019}
    } else if (vars_adj == "mortamt1") {
      if (year == 1990) { threshold <- 2000 * cpi$cpi_2019} else {threshold <- 3000 * cpi$cpi_2019}
    } else if (vars_adj == "mortamt2") {
      if (year == 1990) { threshold <- 1000 * cpi$cpi_2019} else {threshold <- 1100 * cpi$cpi_2019}
    }  
    
    df_eligible <- data %>% 
      left_join(
        topcodes_new %>% 
          select(
            statefip,year,sample,
            any_of(var_sel)
          ) %>% 
          mutate(sample = as.numeric(sample)), 
        by = c("year","sample","statefip")
      ) %>% 
      filter(
        .data[[ vars_adj ]] >= threshold 
      )
    
    # for post-1980 proptx and house_value
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
        .data[[ vars_adj ]] >= .data[[var_sel]] 
      )
    
    threshold <- topcodes_new[[var_sel]] %>% min
    
  }
  
  adjustments <- df_eligible %>% 
    select(sample, serial) 
  
  adjustments[[paste0(vars_adj, "_adjust_flag")]] <- 1
 
  return(
    list(
      adjustments = adjustments,
      topcodes_new = topcodes_new,
      threshold = threshold
    )
    ) 
}