cz_crosswalk <- function(year) {
  library(tidyverse)
  load(paste0("data/census_imputed_wealth_",year,".Rdata"))
  
    if (year == 1980) {
    census <- census %>% 
      mutate(
        cntygp98 = 1000 * statefip + cntygp98
      )
  }
  
  if (year == 1990 | year == 2000 | year == 2005) {
    census <- census %>% 
      mutate(
        puma = ifelse(nchar(puma) == 3, paste0("0",puma), puma),
        puma = str_c(
          str_sub(statefip,1,2),
          str_sub(puma,-4,-1)
        )
      )
  }
  
  if (year > 2010) {
    census <- census %>%
      mutate(
        puma = ifelse(nchar(puma) == 3, paste0("00",puma), puma),
        puma = ifelse(nchar(puma) == 4, paste0("0",puma), puma),
        puma = str_c(
          str_sub(statefip,1,2),
          str_sub(puma,-5,-1)
        )
      )
  }
  
  # cz crosswalk, downloaded from: https://ddorn.net/data.htm
  # and for 1980,1990,2000,2010: https://wmpeople.wm.edu/site/page/pmchenry/crosswalksbetweenpumasandczs
  # for 1960: https://ekrose.github.io/resources/
  crosswalk_path <- paste0(getwd(),"/cz-crosswalks/cz-crosswalks/")
  if (year == 1950 | year == 1940) {
    file_name <- "cw_sea1950_czone.dta" # missing alaska and hawaii
  } else if (year == 1960) {
    file_name <- "cz_puma1960_cw_direct.dta" # missing alaska and hawaii
  } else if (year == 1970) {
    file_name <- "cw_ctygrp1970_czone_corr.dta"
  } else if (year == 1980) {
    file_name <- "cw_ctygrp1980_czone_corr.dta"
  } else if (year == 1990) {
    file_name <- "cw_puma1990_czone.dta"
  } else if (year == 2000 | year == 2010) {
    file_name <- "cw2000_czone2.dta"
  } else if (year == 2020 | year == 2019 | year == 2016 | year == 2013) {
    file_name <- "cw_puma2010_czone.dta"
  }
  
  cz <- haven::read_dta(paste0(crosswalk_path,file_name)) 
  
  cz <- cz %>% 
    rename_with(~ str_replace(.,"\\d.*",""),starts_with("puma")) %>%
    rename_with(~ str_replace(.,"\\d.*",""),starts_with("cz")) %>%
    rename_all(
      recode, 
      county_prop_inpuma = "afactor",
      afact = "afactor",
      sea1950 = "sea",
      cty_grp70 = "cntygp97",
      ctygrp1980 = "cntygp98",
      cntygp98 = "cntygp98",
      cz = "czone"
      ) %>%
    mutate_at(
      vars(matches("puma","sea")),
      ~ as.character(.)
      ) 
  
  # if taking c
  if (year %in% c(
    2000,
    2010
    )
    ) {
    census <- census %>% mutate(puma = statefip*10000+puma)
  }
  
  cz <- cz %>% 
    select(any_of(c("puma","sea", "cntygp97", "cntygp98")), czone, afactor)
  
  # probabilistic allocations (i.e. multiple rows per households with afactor < 1)
  census <- census %>%
    mutate_at(
      vars(matches("puma","sea")),
      ~ as.character(.)
    ) %>%
    left_join(
      cz#,
      #by = "puma"
      ) %>% 
    mutate(
      # here is the new weight to use when computing inequality
      hwx = hhwt * afactor
    ) %>% 
    relocate(
      hwx, .after = hhwt
    ) %>% 
    as_tibble
  
  # see how many are missing 
  missing_factor <- census %>%
    filter(is.na(afactor)) %>% 
    nrow
    
  save_path <- paste0("data/census_imputed_wealth_",year,"_cw.Rdata")
  save(census, file = save_path)
  
  if (missing_factor > 0) {
    message("Problem with crosswalk. ", missing_factor, " households missing 'afactor'")
  } else {
    message("cz crosswalk for census year ",year," complete")
  }
  
  
  
}
