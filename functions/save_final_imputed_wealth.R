# save final version census imputed

load(paste0("data/census_imputed_wealth_",year,".Rdata"))

# these years are missing topcodes for house value
if (year == 2006 | year == 2007) {
  census <- census %>% mutate(house_value_new = house_value)
}

# reverse inverse hyperbolic sine transform and calculate household income
census <- census %>%
  mutate_at(
    vars(matches("inc.*_new$|house_value_new")),
    ~ sinh(.)
  ) %>% 
  mutate(
    income = rowSums(select(., matches("inc.*_new$")), na.rm = T)
  ) 

census <- census %>% rename(region = region_label)

### re-weighting due to combining multiple surveys for some years
if (year == 1990) {
  census <- census %>% 
    mutate(
      hhwt = ifelse(sample == 199002,hhwt*(1/6),hhwt*(5/6))
    )
} else if (year == 2000) {
  census <- census %>% 
    mutate(
      hhwt = ifelse(sample == 200007,hhwt*(1/6),hhwt*(5/6))
    )
} else if (year == 1980) {
  census <- census %>% 
    mutate(
      hhwt = ifelse(sample == 198002,hhwt*(1/6),hhwt*(5/6))
    )
} else if (year == 1970) {
  # both samples used for 1970 are 1%
  census <- census %>% 
    mutate(
      hhwt = hhwt*(1/2)
    )
} 


# final predicted household wealth
census <- census %>% 
  mutate(
    # pred_wealth_new is the predicted value after top-coded thresholds are adjusted
    pred_wealth_new = case_when(
      preds_ens_bin >= thresh ~ exp(preds_ens_pos_new),
      preds_ens_bin < thresh & preds_ens_zero >= optim_thresh_0 ~ 0,
      preds_ens_bin < thresh & preds_ens_zero < optim_thresh_0 ~ sinh(preds_ens_neg_new),
    )
  ) 


# combine puma and statefip (or county group) for CZONE crosswalks
if (year >= 2012) { 
  
  census <- census %>%
    mutate(
      puma_fip = ifelse(nchar(puma) == 3, paste0("00",puma), puma),
      puma_fip = ifelse(nchar(puma_fip) == 4, paste0("0",puma_fip), puma_fip),
      puma_fip = str_c(
        str_sub(statefip,1,2),
        str_sub(puma_fip,-5,-1)
      )
    )
  
} else if (year %in% c(1960,2005:2009,2010,2011)) {
  census <- census %>%
    mutate_at(
      vars(statefip, puma),
      ~ as.numeric(.)
    ) %>% 
    mutate(puma_fip = statefip*10000+puma)
} 

if (year == 1950) {
  census <- census %>% 
    mutate(
      puma_fip = 1000 * sea,
      puma = sea
    )
}
if (year == 1970) {
  census <- census %>% 
    mutate(
      puma_fip = 1000 * statefip + cntygp97,
      puma = cntygp97
    )
}
if (year == 1980) {
  census <- census %>% 
    mutate(
      puma_fip = 1000 * statefip + cntygp98,
      puma = cntygp98
    )
}

if (year == 1990 | year == 2000) {
  census <- census %>% 
    mutate(
      puma_fip = ifelse(nchar(puma) == 3, paste0("0",puma), puma),
      puma_fip = str_c(
        str_sub(statefip,1,2),
        str_sub(puma_fip,-4,-1)
      )
    )
}


save(census, file = paste0("data/census_imputed_wealth_",year,".Rdata"))
