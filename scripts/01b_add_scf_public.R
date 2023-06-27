###############################################################
###### SCRIPT TO CLEAN AND MERGE SCF PUBLIC RELEASE DATA ######
###############################################################

library(tidyverse)
library(haven)

path <- "ADD_PATH_TO_SCF_FILES"

# load SCF summary data extract files, available here:
# https://www.federalreserve.gov/econres/scfindex.htm

# loop through different survey years (summary dataset)
fls <- list.files(
  path,
  ".csv"
  )
years <- c("1989","1992","1995","1998","2001","2004","2007","2010","2013","2016","2019")
scf_list <- vector("list",length(years))
for (f in seq_along(fls)) {
  
  df <- read_csv(paste0(path,"/",fls[[f]]))
  
  numb <- ifelse(f == 1, 2, 1)
  
  scf_list[[f]] <- df %>% 
    rename_all(
      ~ str_to_lower(.)
    ) %>% 
    select(
      impnum = numb,
      wgt,
      age,
      sex = hhsex, 
      educ, # (14 categories) EDUCD
      married, #
      kids,
      lf,
      lifecl, # HHTYPE in census
      famstruct,
      race = racecl4, # 3 = hispanic, 4 = other # x6809
      
      occat1, # 1 = work for someone else, 2 = self-employed/partnership; 3 = retired/disabled; 4  = not working/under 65 out of labor force 
      occ = occat2,
      ind = indcat,
      
      rent,
      
      # income
      wageinc,
      bussefarminc,
      intdivinc,
      ssretinc,
      transfothinc,
      #rentinc,
      penacctwd,
      
      hvehic, # have vehicle (own or business)
      nbusveh, # number of business vehicles
      nvehic, # number of vehicles owned or leased
      own, # if own vehicle
      
      
      hsec_mort, # have second / third mortgage?
      paymort1,
      paymort2,
      
      income, # total household income
      houses, # value of primary residence 
      nfin,fin,
      
      hdebt = mrthel, # below two lines combined
      nh_mort, # purely mortgage and equity loans
      heloc, # how equity lines of credit
      
      late,
      late60,
      bkruplast5,
      
      wealth_gross = asset,
      debt_total = debt,
      net_wealth = networth
      
      
    ) %>% 
    mutate(
      year = years[[f]]
    )   
  
}

# bind list 
scf_extra <- scf_list %>% 
  reduce(rbind)

# tidy variables
scf_extra <- scf_extra %>% 
  mutate(
    id = paste0(year,impnum),
    
    sex = ifelse(sex == 1, "male","female"),
    educ = case_when(
      educ <= 7 & educ >= 1 | educ == -1 ~ "high-school, no diploma",
      educ == 8 ~ "high-school diploma",
      educ == 9 ~ "college drop-out",
      educ >= 10 & educ <= 12 ~ "college degree",
      educ == 13 | educ == 14 ~ "master or professional degree",
      educ == 15 ~ "PhD"
    ),
    married = ifelse(married == 1, "married/LWP", "not married/LWP"),
    
    lf = ifelse(lf == 1, "employed","not_employed"),
    
    famstruct = case_when(
      famstruct == 1 ~ "not married and no children",
      famstruct == 2 ~ "not married and no children and under 55",
      famstruct == 3 ~ "not married and no children and 55+",
      famstruct == 4 ~ "married and children",
      famstruct == 5 ~ "married and no children",
    ),
    
    race = case_when(
      race == 1 ~ "white",
      race == 2 ~ "black",
      race == 3 ~ "hispanic",
      race == 4 ~ "other"
    ),
    
    occ = case_when(
      occ == 1 ~ "managerial/professional",
      occ == 2 ~ "technical/sales/services",
      occ == 3 ~ "production/craft/repair workers, operators, 
    laborers, farmers, foresters, fishers",
      occ == 4 ~ "not working"
    ),
    
    ind = case_when(
      ind == 1 ~ "mining + construction +
    manufacturing",
      ind == 2 ~ "transportation/communications/utilities and
    sanitary services/wholesale trade/finance/insurance and
    real estate/agriculture /retail trade/services/public
    administration",
      ind == 4 ~ "not working"
    ),
    
    hvehic = ifelse(hvehic == 1, "yes", "no"),
    own = ifelse(own == 1, "yes", "no")
  ) %>% 
  relocate(year, id, .after = impnum)



# add extra vars from full dataset
fls <- list.files(
  path,
  ".dta"
)
years <- c("2001","2004","2007","2010","2013","2016","2019","1989","1992","1995","1998")
scf_summary <- vector("list",length(years))

# household members and relationship to respondent (max 12)
hhld_members <- c(
  "x8020", "x102", "x108", 
  "x114", "x120", "x126",
  "x132", "x202", "x208",
  "x214", "x220", "x226"
)

# rounded to nearest 5 years
hhld_members_age <- c(
  "8022","104","110","116","122","128","134","204","210","216","222","228"
)

inc_vars <- c(
  "x5702","b3205", # wages and salaries 
  "x5704","b3206", # business, professional practice and farm income
  "x5706","x5708","x5710", # investments, interest income, dividends, 
  "b3207","b3208","b3209",
  
  "x5712","b3210", # capital gains
  "x5714","b3211", # rental income, trust income, or royalties
  "x5716","b3212", # unemployment insurance or workers comp
  "x5718","b3213", # child support or alimony
  "x5720","b3214", # welfare assistance, incl SSI, i.e. INCWELFR + INCSUPP
  "x5722","b3215", # SS retirement income + disability and retirement programs -- i.e. incss
  "x5724","x5726","b3216", # other income (hrp and partner)
  "x5725","x5727","b3217","b3218", # other sources (pre-2004), IRA = 11, 22 = Welfare via housing subsidy
  
  "x5729","x8182","b3219", # total household income
  "b3201","c1301",
  
  "x6558","x6566","x6574", # retirement income, not SSI,
  
  # pensionacctwd
  "x6464","x6469","x6474","x6479",
  "x6465","x6470","x6475","x6480", # freq
  
  "x6484","x6485","x6489","x6490", # 2004
  
  "x6965","x6971","x6977","x6983",
  "x6966","x6972","x6978","x6984" # freq
  
  
)

ed_vars <- c(
  "x5901","x6101",
  "x5902","x6102",
  "x5905","x6105",
  "x5904","x6104",
  
  # post 2013
  "x5931","x6111",
  "x5932", # high-school regular diploma or GED?
  "x6112", # ibid for spouse/partner
  
  "x6032", "x6132",
  "x6033", "x6133"
  
)

race <- c(
  "x6809", "x6810",
  "x5909"
)

hispanic <- c(
  "x7004"
)

marital <- c(
  "x7372", "x7018",
  "x7377", "x7392", # married before? post 2019
  
  # number of times married, hRP and partner 1989
  "x8001","x8013",
  "x8005","x8007","x8016"
)

health_insurance <- c(
  "x6341", "x6342", "x6343", "x6344", 
  "x6345", "x6346", "x6347", "x6348",
  "x6349",
  "x6350", "x6357",
  
  "x6301","x6302","x6303","x6304","x6305",
  "x6306","x6315","x6316","x6318","x6322","x6329"
)

employ_status <- c(
  "x6678", # employment status spouse/partner
  "x6670" # employment status HRP)
)


unemp <- c(
  "x6780", # unemployed in the last year?
  "x6784",
  "x6781", # how many weeks unemployed last year? WKSUNEMP 1990 census
  "x6785"
  
)

financial_distress <- c(
  "x3005", # delinquent (over 60 days past due)
  "x6774", #bankrupt flag (post-1998)
)

loan_limits <- c(
  "x414", # CC limit
)

geog <- c(
  "b3121", # state
  "b5703", "c1213", #  county
  "b5701", "c1211" # MSA
  )

# loop through different years of full data
for (f in seq_along(fls)) {
  
  df <- haven::read_dta(paste0(path,"/",fls[[f]]))
  
  imp_var <- ifelse(years[[f]] <= 1989, "xx1", "yy1")
  
  df <- df %>% 
    rename_all(~ str_to_lower(.)) %>% 
    select(
      any_of(imp_var),
      any_of(inc_vars),
      any_of(ed_vars),
      
      
      household_size = x101,
      any_of(hhld_members),
      any_of(hhld_members_age),
      
      any_of(race),
      any_of(hispanic),
      
      any_of(marital),
      married_full = x8023,
      married_full_partner = x105,
      
      classwkr = x4106, # employed or self-employed
      classwkr_partner = x4706,
      
      occ_hrp = x7401,
      occ_partner = x7411,
      ind_hrp = x7402,
      ind_partner = x7412,
      
      
      work_status = x4100, # current work status == agg uses 6670 instea
      work_status_partner = x4700,
      
      any_of(employ_status),
      any_of(unemp),
      any_of(financial_distress),
      any_of(loan_limits),
      
      # Hours worked in normal week 
      uhrswork = x4110, # 0 = NA/INAPPROPRIATE
      uhrswork_partner = x4710,
      
      wkswork = x4111,
      wkswork_partner = x4711,
      
      any_of(health_insurance),
      
      mortgage_amount_outstanding = x805,
      
      property_tax_insurance_included = x810,
      proptx = x721,
      proptx_freq = x722, # 
      # INCOME = x5729,
      
      rentinc = x5714
      #mortgage payments, 1 and 2
      # x808,
      # x908
      
      # x809, 909 # frequency of mortgage payments
      
      # x813, x913 # typical mortgage payment
      # x814, x914 # frequency of mortgage payment
    ) %>% 
    mutate(
      year = years[[f]]
    )  
  
  # rename depending on year of survey
  if (years[[f]] > 2007) {
    df <- df %>% 
      rename(
        race_hrp = x6809,
        race_partner = x6810,
        
        marital_status_hrp = x7372,
        marital_status_partner = x7018,
        married_before = x7377, 
        married_before_partner = x7392,
        
        unemployed_last_year = x6780, # unemployed in the last year?
        unemployed_last_year_partner = x6784, # unemployed in the last year?
        
        unemployed_weeks = x6781, # how many weeks unemployed last year? WKSUNEMP 1990 census
        unemployed_weeks_partner = x6785, 
        
        empl_status_partner = x6678, # employment status spouse/partner
        empl_status_hrp = x6670, # employment status HRP) 
        
        
        #### Health insurance
        hcovany = x6341, # HCOVANY
        # different types & how paid for
        health_insurance_medicare = x6342, 
        health_insurance_medicaid = x6343, 
        health_insurance_va = x6344, 
        health_insurance_schip = x6346, 
        
        health_insurance_employer = x6347, 
        health_insurance_union = x6348, 
        health_insurance_personal = x6349, 
        health_insurance_other = x6350,
        
        health_insurance_all_covered = x6357,
        
        
      ) %>% 
      mutate(
        hcovany = ifelse(hcovany == 1, "yes","no"),
        health_care_private = ifelse(
          health_insurance_employer == 1 |
            health_insurance_union == 1 |
            health_insurance_personal == 1|
            health_insurance_other == 1, "yes", "no"
        )
      ) %>%
      mutate_at(
        vars(
          health_insurance_medicare,
          health_insurance_medicaid,
          health_insurance_va,
          health_insurance_employer,
          health_insurance_union,
          health_insurance_personal,
          health_insurance_all_covered
        ),
        ~ ifelse(. == 1, "yes", "no")
      )
  } else {
    
    if (years[[f]] >= 1998 & years[[f]] <= 2007) {
      df <- df %>% 
        rename(
          race_hrp = x6809,
          race_partner = x6810,
          
          marital_status_hrp = x7372,
          marital_status_partner = x7018,
          married_before = x7377, 
          married_before_partner = x7392,
          
          unemployed_last_year = x6780, # unemployed in the last year?
          unemployed_last_year_partner = x6784, # unemployed in the last year?
          unemployed_weeks = x6781, # how many weeks unemployed last year? WKSUNEMP 1990 census
          unemployed_weeks_partner = x6785, 
          
          empl_status_partner = x6678, # employment status spouse/partner
          empl_status_hrp = x6670, # employment status HRP) 
          
        ) 
      
    } else {
      df <- df %>% 
        rename(
          
          race_hrp = x5909#,
          
          #married_number = x8001,
          #married_number_partner = x8013
          
        ) %>% 
        mutate(
          race_hrp = case_when(
            race_hrp == 5 ~ 1,
            race_hrp == 4 ~ 2,
            race_hrp == 3 ~ 3,
            race_hrp %in% c(1,-7) ~ 4
          )
        ) %>% 
        mutate(
          married_before = case_when(
            married_full == 1 & x8005 > 0 & x8007 > 0 ~ 2,
            married_full == 1 & x8005 > 0 & x8007 == 0 ~ 1,
            married_full == 0 | married_full == 6 & married_full != 1 | married_full != 2 ~ 0
          )
        )
      }
   
     df <- df %>% 
       mutate(
         hcovany = ifelse(
           x6301 == 1 & x6315 == 1, "yes", "no"
         ),
         health_insurance_medicare = x6302, 
         health_insurance_medicaid = x6303, 
         health_insurance_va = x6304, 
         health_care_other = x6305,
         
         health_care_private = ifelse(
           x6315 == 1, "yes", "no"
         ),
         health_insurance_employer = x6316, 
         health_insurance_union = x6318, 
         health_insurance_personal = x6322,
         
         health_insurance_all_covered = ifelse(
           x6306 == 1 & x6329 == 1, "yes", "no"
         )
       ) %>%
       mutate_at(
         vars(
           health_insurance_medicare,
           health_insurance_medicaid,
           health_insurance_va,
           health_care_other,
           health_insurance_employer,
           health_insurance_union,
           health_insurance_personal
         ),
         ~ ifelse(. == 1, "yes", "no")
       )
     
  }
  
  # education variables
  if (years[[f]] <= 2013) {
    df <- df %>% 
      mutate(
        educ_hrp = case_when(
          x5901 >= 1 & x5901 <= 4 ~ 1,
          x5901 >= 5 & x5901 <= 6 ~ 2,
          x5901 >= 7 & x5901 <= 8 ~ 3,
          x5901 == 9 ~ 4,
          x5901 == 10 ~ 5,
          x5901 == 11 ~ 6,
          x5901 == 12 & x5902 %in% c(0,5) ~ 7,
          x5901 == 12 & x5902 %in% c(1,2) ~ 8,
          x5901 >= 13 & x5904 == 5 ~ 9,
          x5901 %in% c(13,14,15) & x5905 %in% c(7,11) ~ 10,
          x5901 >= 13 & x5905 %in% c(1,10) ~ 11,
          x5901 >= 13 & x5905 == 2 | x5901 == 16 & x5905 %in% c(11,-7) ~ 12,
          x5901 >= 13 & x5905 %in% c(3,9) | x5901 == 17 & x5905 %in% c(11,7,-7) ~ 13,
          x5901 >= 13 & x5905 %in% c(5, 6, 4, 12) ~ 14
        ),
        
        educ_partner = case_when(
          x6101 >= 1 & x6101 <= 4 ~ 1,
          x6101 >= 5 & x6101 <= 6 ~ 2,
          x6101 >= 7 & x6101 <= 8 ~ 3,
          x6101 == 9 ~ 4,
          x6101 == 10 ~ 5,
          x6101 == 11 ~ 6,
          x6101 == 12 & x6102 %in% c(0,5) ~ 7,
          x6101 == 12 & x6102 %in% c(1,2) ~ 8,
          x6101 >= 13 & x6104 == 5 ~ 9,
          x6101 %in% c(13,14,15) & x6105 %in% c(7,11) ~ 10,
          x6101 >= 13 & x6105 %in% c(1,10) ~ 11,
          x6101 >= 13 & x6105 == 2 | x6101 == 16 & x6105 %in% c(11,-7) ~ 12,
          x6101 >= 13 & x6105 %in% c(3,9) | x6101 == 17 & x6105 %in% c(11,7,-7) ~ 13,
          x6101 >= 13 & x6105 %in% c(5, 6, 4, 12) ~ 14,
          x6101 == 0 | x6101 == -1 ~ 0
        )
        
      ) %>% 
      rename(
        educ_ged = x5902,
        educ_ged_partner = x6102
      )
    
  } else {
    df <- df %>% 
      rename(
        
        educ_hrp = x5931, # educ of HRP
        educ_partner = x6111, # educ of spouse/partner
        
        educ_ged = x5932, # high-school regular diploma or GED?
        educ_ged_partner = x6112, # ibid for spouse/partner
        
        educ_mom = x6032,
        educ_mom_partner = x6132,
        educ_dad = x6033,
        educ_dad_partner = x6133,
      )
  }
  
  scf_summary[[f]] <- df
}

# merge all surveys together
scf_extra2 <- reduce(
  scf_summary,
  bind_rows
)


scf_extra2 <- scf_extra2 %>% 
  rename(
    hispanic = x7004
  ) %>% 
  mutate(
    id = str_c(year,yy1),
    id = ifelse(year == 1989, str_c(year,xx1), id)
  ) %>% 
  mutate_at(
    vars(
      marital_status_hrp, marital_status_partner,
      married_full, married_full_partner
      ),
    ~ case_when(
      . == 1 ~ "married",
      . == 2 ~ "living with partner",
      . == 3 ~ "separated",
      . == 4 ~ "divorced",
      . == 5 ~ "widowed",
      . == 6 ~ "never married",
      . == 0 ~ "no spouse or partner"
    )
  ) %>% 
  mutate(
    married_more_than_once = case_when(
      married_before == 2 ~ "yes",
      married_before == 0 ~ "not applicable",
      married_before == 1 ~ "first marriage"
      ),
    married_more_than_once_partner = case_when(
      married_before_partner == 2 ~ "yes",
      married_before_partner == 0 ~ "not applicable",
      married_before_partner == 1 ~ "first marriage"
    )
  ) %>% 
  mutate_at(
    vars(matches("^educ_(hrp|partner|mom|dad)")),
    ~ case_when(
      . <= 7 & . >= 1 | . == -1 ~ "high-school, no diploma",
      . == 8 ~ "high-school diploma",
      . == 8 & educ_ged | educ_ged_partner == 2 ~ "high-school diploma GED",
      . == 9 ~ "college drop-out",
      . >= 10 & . <= 12 ~ "college degree",
      . == 13 | . == 14 ~ "master or professional degree",
      . == 15 ~ "PhD",
      . == 0 ~ "inappropriate"
    )
    
  ) %>% 
  mutate_at(
    vars(matches("educ_ged")),
    ~ ifelse(. == 1, "yes", "no")
  ) %>% 
  mutate_at(
    vars(race_hrp, race_partner),
    ~ case_when(
      . == 1 ~ "white",
      . == 2 ~ "black",
      . == 3 ~ "hispanic",
      . == 4 ~ "other"
    )
    
  ) %>% 
  mutate_at(
    vars(classwkr, classwkr_partner),
    ~ case_when(
      . == 1 ~ "employed",
      . == 2 ~ "self_employed",
      . >= 3 | . == -7 ~ "other",
      . == 0 ~ "inappropriate"
    )
  ) %>% 
  mutate_at(
    vars(
      work_status, work_status_partner
    ),
    ~ case_when(
      . >= 11 & . <= 17 ~ "working",
      . == 30 | . == 20 | . == 21 ~ "unemployed",
      . == 50 ~ "retired",
      . == 52 ~ "disabled",
      . == 70 ~ "student",
      . == 80 | . == 90 ~ "not employed",
      . == 0 ~ "inappropriate",
      . > 90 ~ "other"
    )
  ) %>% 
  mutate(
    taxincl = ifelse(property_tax_insurance_included %in% c(1,3), "yes","no"),
    insincl = ifelse(property_tax_insurance_included %in% c(2,3), "yes","no"),
  ) %>% 
  mutate_at(
    vars(matches("^uhrswork")),
    ~ ifelse(. == -2 | . >= 85, 85, .) 
  ) %>% 
  mutate_at(
    vars(matches("^unemployed_last_year")),
    ~ case_when(
      . == 1 ~ "yes",
      . == 5 ~ "no",
      . == 0 ~ "inappropriate"
    )
  ) %>% 
  mutate(
    proptx_freq = case_when(
      proptx_freq == 2 ~ "weekly",
      proptx_freq == 3 ~ "every two weeks",
      proptx_freq == 4 ~ "monthly",
      proptx_freq == 5 ~ "quarterly",
      proptx_freq == 6 ~ "yearly",
      proptx_freq %in% c(-7,-1,0) | proptx_freq >= 11 ~ "other",
    ),
    # get property tax to yearly to match census
    proptx = case_when(
      proptx_freq == "weekly" ~ proptx * 52,
      proptx_freq == "every two weeks" ~ proptx * 26,
      proptx_freq == "monthly" ~ proptx * 12,
      proptx_freq == "quarterly" ~ proptx * 4,
      TRUE ~ proptx 
    )
  ) 


save(scf_extra, scf_extra2, file = "data/scf_extra_vars_raw.Rdata")

# adjust for CPI to 2016 dollars, 
# from 2019 for SCF_extra, and from year of survey for rest
# taken from: https://www.bls.gov/data/inflation_calculator.htm
# rebased from June 1999
cpi <- read_csv("data/CPI99_lookup_expanded.csv")
cpi_2016 <- cpi %>% 
  filter(year == 2016) %>% 
  .$CPI99
cpi <- cpi %>% 
  mutate(cpi = CPI99 * (1/cpi_2016)) %>% 
  distinct(year, cpi) 

cpi_2019 <- cpi %>% filter(year == 2019) %>% .$cpi

# rebase to 2016 (all in 2019 dollars)
scf_extra <- scf_extra %>% 
  mutate_at(
    vars(rent, wageinc:penacctwd,paymort1:net_wealth),
    ~ . * cpi_2019
  ) 

# re-arrange by year, select vars 
# and rebase to 2016 depending on year
scf_extra2 <- scf_extra2 %>% 
  arrange(year) %>%
  mutate(year = as.numeric(year)) %>% 
  left_join(
    cpi,
    by = "year"
  ) %>% 
  mutate_at(
    vars(mortgage_amount_outstanding, proptx, rentinc, 
         x5702, x5704, x5706, x5708, x5710, x5712, x5716, x5718, x5720, x5722, x5724, x5726
         ),
    ~ . * cpi
  ) %>% 
  select(
    matches(
      "^(household_size|educ_|race_|mar(i|r)|classwkr|occ_|ind_|work_status|unemployed|uhrswork|wkswork|mortgage_amount|proptx|rentinc|hcovany|health_insura|taxinc|insincl)",
      ),
    x5702, x5704, x5706, x5708, x5710, x5712, x5716, x5718, x5720, x5722, x5724, x5725, x5726, x5727,
    x8020:x220,x226
  ) 

# tidy before merging with SCF+
scf_extra <- scf_extra %>% 
  select(
    year:net_wealth,
  ) %>% 
  cbind(scf_extra2) 

scf_extra <- scf_extra %>% 
  rowwise() %>%
  mutate(
    welfare_other_hrp = ifelse(x5725 == 22, x5724, 0),
    welfare_other_partner = ifelse(x5727 == 22, x5726, 0),
    other_hrp = ifelse(x5725 != 11 & x5725 != 22, x5724, 0),
    other_partner = ifelse(x5727 != 11 & x5727 != 22 & !is.na(x5727), x5726, 0),
    retir_hrp = ifelse(year <= 2004 & x5725 == 11, x5724, 0),
    retir_partner = ifelse(year <= 2004 & x5727 == 11, x5726, 0),
    
    incwage = x5702,
    incbus = x5704,
    incss = x5722,
    incwelfr = sum(c(x5716, x5720, welfare_other_hrp, welfare_other_partner), na.rm = T),
    incinvst = sum(c(x5706, x5708, x5710, rentinc), na.rm = T),
    incretir = ifelse(year <= 2004, sum(c(retir_hrp,retir_partner),na.rm=T), penacctwd),
    incother = sum(
      c(x5718, x5712, other_hrp, other_partner), 
      na.rm = T
      )
  ) %>% 
  ungroup %>% 
  mutate(
    proptx = ifelse(proptx < 0, 0, proptx)
  )  %>% 
  # TODO some of these numbers are imputed, so not unique by impnum...
  mutate_at(
    vars(ind_hrp,ind_partner,
         occ_hrp,occ_partner),
    ~ as.character(.)
  )


scf_extra_num <- scf_extra %>% 
  group_by(id) %>% 
  summarise_if(
    is.numeric,
    ~ mean(., na.rm = T)
  )

scf_extra_char <- scf_extra %>% 
  select_if(is.character) %>% 
  distinct(id, .keep_all = T)


# save as scf_tidy
scf_extra <- scf_extra_num %>% 
  cbind(scf_extra_char %>% select(-id)) %>% 
  select(
    id, year,everything()
  ) 


# household size and number of adults
scf_extra <- scf_extra %>% 
  mutate_at(
    vars(x8020:x226),
    ~ ifelse(. != 4 & . != 13 & . != 36 & . != 0, 1, 0)
  ) %>% 
  rowwise() %>% 
  mutate(
    adults = sum(c_across(x8020:x226),na.rm=T)
  ) %>% 
  ungroup %>% 
  mutate(
    household_size_check = adults + kids, 
    hhequiv_new = 1 + ((adults-1) * 0.5) + kids * 0.2
  )

save(scf_extra, file = "data/scf_extra_vars_clean.Rdata")

# add in weights from SCF+
load("data/scf_tidy.Rdata")
wgt_extra <- scf_tidy %>% 
  select(id,wgt_tidy = wgt, hhequiv) 

scf_old <- scf_tidy %>% 
  filter(year < 1989)

scf_tidy <- scf_extra %>% 
  left_join(
    wgt_extra,
    by = "id"
  ) %>% 
  mutate(year = as.numeric(year)) %>% 
  full_join(
    scf_old %>% 
      rename(
        wgt_tidy = wgt,
        wgt = wgt_original
        )
  ) 


# create and transform a few more variables to match census
scf_tidy <- scf_tidy %>% 
  mutate(
    have_wealth = ifelse(net_wealth <= 0, "no", "yes"),
    have_wealth = factor(have_wealth),
    
    # create indicator variable if own house
    # and if so log(value)
    house_value = ifelse(is.na(houses),house,houses),
    house_tenure = ifelse(house_value == 0, "rent", "own_outright"),
    house_tenure = ifelse(house_tenure == "own_outright" & hdebt > 0, "own_mortgage", house_tenure),
    house_value = asinh(house_value),
    houses = NULL, house = NULL,
    
    # hhtype = case_when(
    #   married_full == "married" & hhsex == "male" & kids > 0 ~ "Married-couple family household",
    #   married_full == "married" & hhsex == "male" & kids > 0 ~ "Married-couple family household",
    #   famstruct == "not married and no children" ~ ,
    #   
    # ),
    nvehic = ifelse(nvehic > 7, 7, nvehic),
    hcovpriv = ifelse(
      health_insurance_employer == "yes" |
        health_insurance_union == "yes" |
        health_insurance_personal == "yes",
      "yes",
      "no"
    ),
    hcovpub = ifelse(
      health_insurance_medicare == "yes" |
        health_insurance_medicaid == "yes" |
        health_insurance_va == "yes",
      "yes",
      "no"
    )
  ) %>% 
  mutate_at(
    vars(matches("work_status")),
    ~ case_when(
      . == "working" ~ "employed",
      . == "unemployed" ~ "unemployed",
      . == "inappropriate" ~ "inappropriate",
      TRUE ~ "not in labor force"
    )
  ) %>% 
  mutate_at(
    vars(matches("classwkr")),
    ~ case_when(
      # TODO add extra category for inappropriate (i.e. no spouse/partner) 
      # for classwkr and wkswork
      . == "employed" ~ "employed",
      . == "self_employed" ~ "self-employed",
      TRUE ~ "N/A"
    )
  ) %>% 
  mutate_at(
    vars(incwage:incother),
    ~ asinh(.)
  ) %>% 
  mutate_at(
    vars(wkswork, wkswork_partner),
    ~ case_when(
      . == 0 ~ "N/A",
      . >= 1 & . <= 13 ~ "1-13",
      . >= 14 & . <= 26 ~ "14-26",
      . >= 27 & . <= 39 ~ "27-39",
      . >= 40 & . <= 47 ~ "40-47",
      . >= 48 & . <= 49 ~ "48-49",
      . >= 50 & . <= 52 ~ "50-52"
    )
  ) %>% 
  # only one in the below category
  mutate(
    educ_partner = ifelse(educ_partner == "high-school diploma GED",
                      "high-school diploma",
                      educ_partner
                      )
  )

save(scf_tidy, file = "data/scf_tidy_full.Rdata")
