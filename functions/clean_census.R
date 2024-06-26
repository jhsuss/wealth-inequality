clean_census <- function(year, 
                         return_data = F,
                         topcode = T # toggle whether want topcode adjusted
) {
  
  file_path <- paste0("data/census_", year, ".gz")
  data <- haven::read_dta(file_path)
  data <- data %>% rename_all( ~ str_to_upper(.))

  if (!is.null(data$REGION)) {
    # numeric value to labels for geographies
    regions <- tibble(
      region_label = data$REGION %>% attributes %>% .$labels %>% names,
      region = data$REGION %>% attributes %>% .$labels %>% as.numeric
    )
  }
  if (!is.null(data$STATEFIP)) {
    states <- tibble(
      state = data$STATEFIP %>% attributes %>% .$labels %>% names,
      statefip = data$STATEFIP %>% attributes %>% .$labels %>% as.numeric()
    )
  }
  
  vars <- c(
    "SAMPLE", # sample identifier
    "PERNUM", # individual identifier
    "SERIAL", # household identifier
    "YEAR",
    "HHWT", # household weight
    "GQ",
    "SEX",
    "AGE",
    "RELATE",
    "RELATED",
    "RACE", 
    "HISPAN",
    "EDUC", 
    "EDUCD",
    "MARST",
    "OWNERSHP",
    "OWNERSHPD",
    "MORTGAGE",
    "VALUEH",
    "PROPTX99",
    "TAXINCL",
    "INSINCL",
    "MORTOTAL",
    "MORTAMT1",
    "MORTAMT2",
    "RENT",
    "VEHICLES",
    "FWAGE1",
    "HHINCOME",
    "IND",
    "OCC",
    "EMPSTAT",
    "CLASSWKR",
    "WKSWORK2",
    "UHRSWORK",
    "HCOVANY",
    "REGION", 
    "STATEFIP", 
    "PUMA","CONSPUMA", "CNTYGP97", "CNTYGP98"
  )
  
  additional_geog <- c(
    "METAREA",
    "LMA", 
    "COMZONE",
    "SEA"
  )
  
  # keep only relevant columns, rename and relabel
  data <- data %>% 
    select(
      any_of(vars),
      starts_with("INC"),
      any_of(additional_geog)
    ) %>% 
    rename_all(
      ~ str_to_lower(.)
    )  
  
  
  # remove people in jail and other institutional contexts
  # and remove those living in group quarters
  data <- data %>% 
    filter(
      relate != 13,
      gq != 3, gq != 4, gq != 0
    ) %>% 
    select(-gq)
  
  
  if (year != 1950) {
    # valueh is N/A when rented, other incomes are N/A in some
    data <- data %>% 
      mutate(
        valueh = ifelse(valueh == 9999999 & ownershp == 2, 0, valueh),
        valueh = ifelse(valueh == 9999999 & ownershp == 1, NA, valueh) # TODO 1980 has some missing values when ownership == 1...1950 model to predict??
      ) %>% 
      filter(!is.na(valueh)) %>% 
      rename(
        house_value = valueh
      )
  }
  
  data <- data %>% 
    mutate_at(
      vars(matches("^inc"), matches("ftotinc")),
      ~ ifelse(. == 999999 | . == 999998 | . == 9999999 | . == 99999, 0, .)
    )
  
  # re-code housing tenure variable to match model
  if (year > 1980) {
    data <- data %>% 
      mutate(
        house_tenure = case_when(
          ownershpd == 0 ~ "other",
          ownershpd >= 20 ~ "rent",
          ownershpd == 12 ~ "own_outright",
          ownershpd == 13 ~ "own_mortgage"
        )
      )
  } else if (year == 1980) {
    data <- data %>% 
      mutate(
        house_tenure = case_when(
          ownershp == 1 & mortgage == 0 ~ "other",
          ownershp == 2 ~ "rent",
          ownershp == 1 & mortgage == 1 ~ "own_outright",
          ownershp == 1 & mortgage >= 2 ~ "own_mortgage"
        )
      )
  } else if (year == 1950) {
    # do nothing -- no housing variables in 1950
  } else {
    data <- data %>% 
      mutate(
        house_tenure = case_when(
          ownershp == 0 ~ "other",
          ownershp == 2 ~ "rent",
          ownershp == 1 ~ "own"
        )
      )
  }
  
  # if year is 2000, add incsupp to incwelfr together (to align with SCF)
  if (year == 2000) {
    data <- data %>%
      rowwise() %>%
      mutate(
        incwelfr = sum(c(incwelfr, incsupp), na.rm = T)  # SCF+ has these combined (might also include INCSUPP?)
      ) %>%
      ungroup
  }
  
  # incbus is both farm and business from 2000
  if (year >= 2000) {
    
    data <- data %>% 
      rename(incbus = incbus00)
    
  } else if (year < 2000 & year >= 1970) {
    
    data <- data %>%
      rowwise() %>% 
      mutate(
        incbus = sum(c(incbus, incfarm), na.rm = T)
      ) %>% 
      ungroup
    
  } else if (year == 1940) {
    
    data <- data %>%
      mutate(
        incnonwg = case_when(
          incnonwg == 2 ~ "50+", 
          incnonwg == 1 ~ "less_than_50"
          ) 
      ) 
    
  } else {
    
    data <- data %>% 
      rename(incbus = incbusfm)
    
  }
  
  
  # household income consistency
  # for pre-1980 data, need to take ftotinc
  if (year == 1970) {
    data <- data %>%
      rename(income = ftotinc)
  } else if (year %in% c(1950,1960)) {
    data <- data %>%
      rename(income = inctot)
  } else if (year == 1940) {
    data <- data %>%
      rename(income = fwage1)
  } else {
    data <- data %>%
      rename(income = hhincome)
  }
  
  
  # convert proptx to numeric 
  if (year >= 1990) {
    proptx_lookup <- data.frame(
      code = 0:69, 
      proptx = c(0,0,
                 seq(25,1000,50),
                 seq(1050,4500,100),
                 4500,4550,4650,4750,4850,4950,
                 5250,5750,6500,7500,8500,9500,
                 10000 # top code of 10000+ to be replaced
      )
    )
    data <- data %>% 
      left_join(
        proptx_lookup, 
        by = c("proptx99" = "code")
      )
  }
  
  if (year > 1980) {
    data <- data %>%
      mutate(
        nvehic = ifelse(vehicles == 9, 0, vehicles)
      )
  }
  
  # rename and recode variables
  data <- data %>%
    rename(
      married_full = marst,
      work_status = empstat,
      wkswork = wkswork2
    ) %>% 
    mutate(
      sex = ifelse(sex == 1, "male", "female"),
      household_size = 1, # this will be summed when going to household level
      # to equivalise hhld: https://en.wikipedia.org/wiki/Equivalisation#:~:text=OECD%2Dmodified%20scale,-The%20OECD%2Dmodified&text=To%20calculate%20equivalised%20income%20using,each%20child%20aged%20under%2014.
      hhequiv = ifelse(relate == 1, 1, NA),
      hhequiv = ifelse(relate > 1 & age >= 14, 0.5, hhequiv),
      hhequiv = ifelse(relate > 1 & age < 14, 0.2, hhequiv),
      
      # character labels
      educ = case_when(
        educd >= 10 & educd <= 61 | educd %in% c(0,2) ~ "high-school, no diploma",
        educd == 62 | educd == 63 ~ "high-school diploma",
        educd == 64 ~ "high-school diploma GED",
        educd %in% c(65,71) ~ "college drop-out",
        educd == 70 | educd >= 80 & educd <= 113 ~ "college degree",
        educd == 114 | educd == 115 | educd == 116 ~ "master or professional degree",
        #educd == 116 ~ "PhD", # in SCF public PhD subsumed into master and above
        educd == 1 ~ "inappropriate"
      ),
      
      race = case_when(
        race == 1 ~ "white",
        race == 2 ~ "black",
        hispan >= 1 ~ "hispanic",
        race >= 3 ~ "other"
      )
    ) %>%
    mutate_at(
      vars(wkswork),
      ~ case_when(
        . == 0 ~ "N/A",
        . == 1 ~ "1-13",
        . == 2 ~ "14-26",
        . == 3 ~ "27-39",
        . == 4 ~ "40-47",
        . == 5 ~ "48-49",
        . == 6 ~ "50-52"
      )
    ) %>% 
    mutate_at(
      vars(married_full),
      ~ case_when(
        . == 1 | . == 2 ~ "married",
        . == 3 ~ "separated",
        . == 4 ~ "divorced",
        . == 5 ~ "widowed",
        . == 6 ~ "never married"
      )
    ) %>% 
    mutate_at(
      vars(matches("work_status")),
      ~ case_when(
        . == 1 ~ "employed",
        . == 2 ~ "unemployed",
        . == 3 | . == 0 ~ "not in labor force"
      )
    ) %>% 
    mutate_at(
      vars(matches("classwkr")),
      ~ case_when(
        . == 2 ~ "employed",
        . == 1 ~ "self-employed",
        . == 0 ~ "N/A"
      )
    ) 
  
  if (year < 1990) {
    data <- data %>% 
      mutate_at(
        vars(matches("^educ")),
        ~ case_when(
          str_detect(., "high-school") ~ "no college",
          str_detect(., "college|master") ~ "college",
          TRUE ~ "inappropriate"
        )
      )
  }
  
  # re-code occupation based on year
  if (year < 2004 & year >= 1940) {
    data <- data %>% 
      mutate_at(
        vars(occ),
        ~ case_when(
          . == 0 ~ "0",
          . >= 3 & . <= 37 |
            . >= 43 & . <= 199 ~ "1",
          . >= 203 & . <= 235 |
            . >= 243 & . <= 285 |
            . >= 303 & . <= 389 ~ "2",
          . >= 403 & . <= 407 |
            . >= 413 & . <= 427 |
            . >= 905 & . <= 905 |
            . >= 433 & . <= 469 ~ "3",
          . >= 473 & . <= 499 ~ "6",
          . >= 503 & . <= 699 ~ "4",
          . >= 703 & . <= 799 |
            . >= 803 & . <= 859 |
            . >= 863 & . <= 889 ~ "5",
          TRUE ~ "0"
        )
      )
  } else if (year >= 2004 & year <= 2010) {
    data <- data %>% 
      mutate_at(
        vars(occ),
        ~ case_when(
          . == 0 ~ "0",
          . >= 10 & . <= 200 |
            . >= 220 & . <= 1530 |
            . >= 1600 & . <= 1860 |
            . >= 2000 & . <= 3650 ~ "1",
          . >= 1540 & . <= 1560 |
            . >= 4700 & . <= 5930 |
            . >= 1900 & . <= 1960 |
            . >= 7900 & . <= 7900 ~ "2",
          . >= 3700 & . <= 4320 |
            . >= 4400 & . <= 4400 | 
            . >= 4420 & . <= 4650 |
            . >= 9800 & . <= 9840 ~ "3",
          . >= 6200 & . <= 7850 |
            . >= 8330 & . <= 8330 |
            . >= 8350 & . <= 8350 |
            . >= 8440 & . <= 8630 |
            . >= 8740 & . <= 8760 |
            . >= 8810 & . <= 8810 ~ "4",
          . >= 4410 & . <= 4410 | 
            . >= 7920 & . <= 8320 |
            . >= 8340 & . <= 8340 |
            . >= 8360 & . <= 8430 |
            . >= 8640 & . <= 8730 |
            . >= 8800 & . <= 8800 |
            . >= 8830 & . <= 9750 ~ "5",
          . >= 205 & . <= 205 | 
            . >= 4340 & . <= 4350 |
            . >= 6000 & . <= 6130 ~ "6",
          TRUE ~ "0"
        )
        
      )
  } else if (year >= 2011) {
    data <- data %>% 
      mutate_at(
        vars(occ),
        ~ case_when(
          . == 0 ~ "0",
          . >= 10 & . <=200 |
            . >= 220 & . <=1530 |
            . >= 1600 & . <=1860 |
            . >= 2000 & . <=3650 ~ "1",
          . >= 1540 & . <=1560 |
            . >= 4700 & . <=5930 |
            . >= 1900 & . <=1960 |
            . >= 7900 & . <=7900 ~ "2",
          . >= 3700 & . <=4320 |
            . >= 4400 & . <=4400 |
            . >= 4420 & . <=4650 |
            . >= 9840 & . <=9840 ~ "3",
          . >= 6200 & . <=7850 |
            . >= 8330 & . <=8330 |
            . >= 8350 & . <=8350 |
            . >= 8440 & . <=8630 |
            . >= 8740 & . <=8760 |
            . >= 8810 & . <=8810 ~ "4",
          . >= 4410 & . <=4410 |
            . >= 7920 & . <=8320 |
            . >= 8340 & . <=8340 |
            . >= 8360 & . <=8430 |
            . >= 8640 & . <=8730 |
            . >= 8800 & . <=8800 |
            . >= 8830 & . <=9750 ~ "5",
          . >= 210 & . <=210 |
            . >= 4340 & . <=4350 |
            . >= 6000 & . <=6130 ~ "6",
          TRUE ~ "0"
        )
      )
  }
  
  # recode ind depending on year
  if (year < 2004 & year >= 1940) {
    
    data <- data %>% 
      mutate_at(
        vars(ind),
        ~ case_when(
          . == 0 ~ "0",
          . >= 10 & . <=31 ~ "1",
          . >= 40 & . <=50 |
            . >= 60 & . <=60 ~ "2",
          . >= 100 & . <=392 ~ "3",
          . >= 500 & . <=571 |
            . >= 580 & . <=691 ~ "4",
          . >= 700 & . <=712 |
            . >= 721 & . <=760 ~ "5",
          . >= 400 & . <=472 |
            . >= 761 & . <=791 |
            . >= 800 & . <=802 |
            . >= 812 & . <=892 ~ "6",
          . >= 900 & . <=932 ~ "7",
          TRUE ~ "0"
        )
      )
    
  } else if (year >= 2004) {
    data <- data %>%
      mutate_at(
        vars(ind),
        ~ case_when(
          . == 0 ~ "0",
          . >= 170 & . <= 290 |
            . >= 7480 & . <= 7480 |
            . >= 7770 & . <= 7770 ~ "1",
          . >= 370 & . <= 490 |
            . >= 770 & . <= 770 ~ "2",
          . >= 1070 & . <= 3990 |
            . >= 6470 & . <= 6480 |
            . >= 8560 & . <= 8560 ~ "3",
          . >= 4070 & . <= 5790 |
            . >= 8680 & . <=8690 ~ "4",
          . >= 6490 & . <=6490 |
            . >= 6695 & . <=6695 |
            . >= 6870 & . <=7080 |
            . >= 7190 & . <=7190 |
            . >= 7580 & . <=7590 |
            . >= 7680 & . <=7680 |
            . >= 8770 & . <=8890 ~ "5",
          . >= 570 & . <=690 |
            . >= 6070 & . <=6390 |
            . >= 6570 & . <=6692 |
            . >= 6770 & . <=6780 |
            . >= 7170 & . <=7180 |
            . >= 7270 & . <=7470 |
            . >= 7490 & . <=7570 |
            . >= 7670 & . <=7670 |
            . >= 7690 & . <=7690 |
            . >= 7780 & . <=8470 |
            . >= 8570 & . <=8670 |
            . >= 8970 & . <=9290 ~ "6",
          . >= 9370 & . <=9890 ~ "7",
          TRUE ~ "0"
        )
      )
    
  } 
  
  
  if (year >= 1980) {
    
    data <- data %>% 
      mutate(
        taxincl = ifelse(taxincl == 2, "yes","no"),
        insincl = ifelse(insincl == 2, "yes","no")
      ) 
  }
  
  if (year >= 2008) {
    
    data <- data %>% 
      mutate_at(
        vars(matches("hcovany|hcovpub|hcovpriv")),
        ~ ifelse(. == 2, "yes","no")
      ) 
    
  }
  
  data <- data %>%
    # add in text  
    left_join(
      states
    ) 
  
  # adjust for inflation, to 2019
  years <- data$year %>% unique %>% as.numeric
  cpi <- read_csv("data/CPI99_lookup.csv")
  cpi <<- cpi %>% 
    mutate(cpi_2019 = CPI99 * (1/0.652)) %>% 
    select(year, cpi_2019) %>% 
    filter(year %in% years) 
  
  # multiply all income and housing items by factor to bring to 2016 dollars
  data <- data %>%
    left_join(cpi, by = "year") %>% 
    mutate_at(
      vars(matches("inc(w|b|s|i|r|o)"),
           matches("house_value|proptx|^mort|^rent")),
      ~ . * cpi_2019
    ) 
  
  # identify households that are top-coded
  if (topcode) {
    
    library(EnvStats, exclude = c("predict", "predict.lm"))
    # load scf clean data for distributional info
    load("data/scf_tidy_full.Rdata")
    # load aggregated topcodes
    load("data/topcodes.Rdata")
    source("functions/pseudoMLE.R")
    
    scf_tidy <<- scf_tidy  %>% 
      rename(
        mortamt1 = paymort1,
        mortamt2 = paymort2
      )
    
    # relevant top-coded variables 
    vars_adj <<- c(
      "mortamt1",
      "mortamt2",
      "mortotal",
      "proptx",
      "house_value",
      "rent",
      "incwage", 
      "incbus",
      "incss",
      "incwelfr",
      "incinvst",
      "incretir",
      "incother"
    )
    
    source("functions/topcode_eligible.R")
    
    topcoded <- vector("list", length(vars_adj))
    
    for (i in seq_along(vars_adj)) {
      
      # if variable is missing
      if ( is.null(data[[ vars_adj[[i]] ]]) ) {
        next
      }
      
      year_ <- year
      
      topcode_available <- topcodes %>% 
        filter(year == year_) %>% 
        .[[ paste0(vars_adj[[i]], "_topcode") ]] %>% 
        .[[1]]
      if ( is.na(topcode_available) ) {
        next
      }
      
      topcoded[[i]] <- eligible(
        vars_adj = vars_adj[[i]], 
        data = data, 
        year = year
      )
      
    }
    
  }
  
  
  
  # aggregate individual-level variables to household-level
  to_agg <- c(
    "household_size",
    "hhequiv",
    "incwage", 
    "incbus",
    "incss",
    "incwelfr",
    "incinvst",
    "incretir",
    "incother"
  )
  
  census <- data %>% 
    group_by(sample, year, serial) %>% 
    summarise_at(
      vars(any_of(to_agg), matches(paste0(to_agg,"_new"))),
      ~ sum(., na.rm = T)
    ) %>% 
    ungroup
  
  # add partner vars to household level
  vars_partner <- c(
    "educ",
    "married_full",
    "work_status",
    "classwkr",
    "occ",
    "ind",
    "wkswork",
    "uhrswork"
  )
  
  tmp_partner <- data %>% 
    filter(
      related %in% c(201,1114)
    ) %>% 
    select(
      sample,serial,year,
      any_of(vars_partner)
    ) %>% 
    rename_at(
      vars(any_of(vars_partner)),
      ~ paste0(., "_partner")
    )
  
  # deal with inconsistencies between SCF and census labels
  tmp_partner <- tmp_partner %>% 
    mutate(
      married_full_partner = ifelse(
        married_full_partner %in% c(
          "divorced",
          "separated",
          "widowed"), 
        "no spouse or partner", 
        married_full_partner
      ), 
      married_full_partner = ifelse(
        married_full_partner == "never married", 
        "living with partner", 
        married_full_partner
      ),
      educ_partner = ifelse(
        educ_partner == "high-school diploma GED", 
        "high-school diploma", 
        educ_partner)
    )  
  

  
  # reduce remaining vars to head of household
  vars <- c(
    "sample","serial","year",
    "hhwt",
    "sex","age",  
    "race","educ",
    "married_full",
    "married_more_than_once",
    "income", "income_new",
    "incnonwg",
    "house_tenure",
    "house_value","house_value_new",
    "mortamt1","mortamt1_new",
    "mortamt2","mortamt2_new",
    "mortotal","mortotal_new",
    "taxincl",
    "insincl",
    "proptx","proptx_new",
    "rent","rent_new",
    "nvehic",
    "hcovany","hcovpriv","hcovpub",
    "work_status",
    "classwkr",
    "occ",
    "ind",
    "wkswork",
    "uhrswork",
    "region", "statefip", "countyfip", "puma",
    additional_geog %>% str_to_lower, 
    "cntygp97", "cntygp98"
  )
  
  data <- data %>%
    filter(relate == 1) %>% 
    select(
      any_of(vars)
    ) %>% 
    # merge partner vars
    left_join(
      tmp_partner,
      by = c("sample","serial","year")
    ) %>% 
    # missing partner values where hrp is not married / has no partner
    mutate_at(
      vars(educ_partner, work_status_partner),
      ~ ifelse(is.na(.), "inappropriate", .)
    ) %>% 
    mutate_at(
      vars(classwkr_partner, wkswork_partner),
      ~ ifelse(is.na(.), "N/A", .)
    ) %>% 
    mutate(
      married_full_partner = ifelse(is.na(married_full_partner), "no spouse or partner", married_full_partner)
    ) %>% 
    mutate_at(
      vars(occ,ind,occ_partner,ind_partner),
      ~ ifelse(is.na(.) | . == "N/A", "0", .)
    )
  
  if (year > 1970) {
    data <- data %>%
      mutate(
        uhrswork_partner = ifelse(is.na(uhrswork_partner), 0, uhrswork_partner)
      )
  }

  rm(tmp_partner)
  
  # rename hrp vars to match fitted models
  data <- data %>% 
    rename(
      educ_hrp = educ,
      occ_hrp = occ,
      ind_hrp = ind
    )
  
  # add back in household head demographics and geography labels
  census <- data %>% 
    left_join(
      census, by = c("sample","serial","year")
    ) 
  
  # remove remaining data
  rm(data)
  
  census <- census %>%
    # add in text  
    left_join(
      states
    ) %>% 
    left_join(
      regions
    )
  
  census <- census %>% 
    # deal with skewed variables
    mutate_at(
      vars(matches("inc(w|b|s|i|r|o)|house_value")),
      ~ asinh(.)
    ) 
  
  # save household-level data (intermediate, pre-top-code adjustment)
  save_path <- paste0("data/census_clean_intermediate_",year,".Rdata")
  save(census, topcoded, file = save_path)
  
  # adjust topcodes 
  if (topcode) {
    
    library(EnvStats, exclude = c("predict", "predict.lm"))
    load("data/scf_tidy_full.Rdata")
    load("data/topcodes.Rdata")
    
    scf_tidy <<- scf_tidy  %>% 
      rename(
        mortamt1 = paymort1,
        mortamt2 = paymort2
      )
    
    # truncated pareto distribution function
    rparetoTrunc <<- function(n, scale, shape, lower_bound = scale, upper_bound = Inf) {
      quantiles <- ppareto(c(lower_bound, upper_bound), scale, shape)
      uniform_random_numbers <- runif(n, quantiles[1], quantiles[2])    
      qpareto(uniform_random_numbers, scale, shape)
    }
    
    years <- census$year %>% unique %>% sort
    
    # income variables to adjust
    vars_adj <<- c(
      "mortamt1",
      "mortamt2",
      "mortotal",
      "proptx",
      "house_value",
      "rent",
      "incwage", 
      "incbus",
      "incss",
      "incwelfr",
      "incinvst",
      "incretir",
      "incother"
    )
    
    source("functions/topcode_adjust.R")
    
    census_adjusted <- vector("list",length(vars_adj))

    # run function
    for (i in seq_along(vars_adj)) {
      
      # if variable is missing
      if ( nrow( topcoded[[i]]$adjustments ) == 0 || is.null(topcoded[[i]])) {
        next
      }
      
      ids <- topcoded[[i]]$adjustments %>% distinct(sample, serial)
      data <- census %>% inner_join(ids, by = c("sample", "serial"))
      
      topcodes_new <<- topcoded[[i]]$topcodes_new
      threshold <<- topcoded[[i]]$threshold
      
      census_adjusted[[i]] <- adjust(
        vars_adj = vars_adj[[i]], 
        data = data, 
        year = year
      )
      gc()
      
      census <- census %>% 
        anti_join(ids, by = c("sample", "serial")) %>% 
        bind_rows(census_adjusted[[i]]$df_eligible)
      
      # replace NAs in _new variable with non-adjusted values 
      census[[ paste0(vars_adj[[i]], "_new") ]] <- ifelse( 
        is.na( census[[ paste0(vars_adj[[i]], "_new") ]] ), 
        census [[ vars_adj[[i]] ]], 
        census[[ paste0(vars_adj[[i]], "_new") ]] 
      )
      
    }
    
    
    
  } # end top-code adjusting
  
  
  # save household-level data
  save_path <- paste0("data/census_clean_",year,".Rdata")
  save(census, topcoded, file = save_path)
  
  message("save complete for ", year)
  
  if (return_data) {
    return(census)
  }
}

