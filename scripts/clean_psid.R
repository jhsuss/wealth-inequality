library(tidyverse)
library(readxl)

psid <- read_xlsx(
  "//Mafp-nwsrv/data/Advanced Analytics/AA_sensitive/_People/Joel Suss/wealth-inequality/wealth-inequality-main/data/PSID_2019/J314575.xlsx"
  )

# refine and select vars
psid <- psid %>% 
  mutate(
    educ_hrp = case_when(
      ER76908 == 1 | ER76908 == 2 ~ "high-school diploma",
      ER76908 == 3 | ER76908 == 0 ~ "high-school, no diploma",
      ER76922 >= 1 & ER76922 < 9 & ER76923 == 1 ~ "college degree",
      ER76922 >= 1 & ER76922 < 9 & ER76923 == 5 ~ "college drop-out",
      ER76924 >= 3 & ER76924 <= 97 ~ "master or professional degree" 
    ),
    educ_partner = case_when(
      ER76763 == 1 | ER76763 == 2 ~ "high-school diploma",
      ER76763 == 3 | ER76763 == 0 ~ "high-school, no diploma",
      ER76777 >= 1 & ER76777 < 9 & ER76778 == 1 ~ "college degree",
      ER76777 >= 1 & ER76777 < 9 & ER76778 == 5 ~ "college drop-out",
      ER76779 >= 3 & ER76779 <= 97 ~ "master or professional degree" 
    ),
    
    race = case_when(
      ER76896 > 0 & ER76896 <= 7 ~ "hispanic",
      ER76897 == 1 ~ "white",
      ER76897 == 2 ~ "black",
      ER76897 >= 3 | ER76897 <= 7 ~ "other"
    ),
    
    house_tenure = case_when(
      ER72048 == 1 & ER72030 == 1 ~ "own_mortgage",
      ER72030 == 1 & ER72048 != 1 ~ "own_outright",
      ER72030 == 5 ~ "rent"
    )
  ) %>% 
  rowwise() %>% 
  mutate(
    incwage = sum(c(
      ER77299,ER77327, # wages 
      ER77301,ER77329, # bonuses
      ER77303,ER77331, # overtime
      ER77305,ER77333, # tips
      ER77307,ER77335, # commission
      ER77311,ER77339, # additional jobs
      ER77313,ER77341, # misc labor income
      ER77416 # total labor income of other family members
      )
      ),
    incbus = sum(c(
      ER77291, # hrp and spouse bus income 
      ER77294, # hrp and spouse farm income
      ER77309,ER77337 # prof practice
      )),
    incss = sum(c(
      ER77446,ER77444,ER77446
      ), na.rm = T),
    incsupp = sum(c(
      ER77355,ER77385, ER77423 # SSI
      )),
    incwelfr = sum(c(
      ER77353,ER77383,ER77421, # income from TANF etc.
      ER77357,ER77387,ER77425, # other welfare
      ER77355,ER77385, ER77423, # supplemental social security
      ER77369,ER77399, #unemployment compensation
      ER77371,ER77401, # workers compensation
      ER77373,ER77403,ER77435 # child support
    )),
    incinvst = sum(c(
      ER77316,ER77344, # rent income
      ER77318,ER77346, # dividends
      ER77320,ER77348, # interest income
      ER77322,ER77350, #trust income
      ER77418 # asset income of rest of family
    )),
    incretir = sum(c(
      ER77359, ER77389, ER77427, # VA pension
      ER77361, ER77391, ER77429,# retirement/pensions
      ER77363, ER77393, # annuities
      ER77365, ER77395, # IRAs
      ER77367, ER77397 # other retirement
      
    )),
    incother = sum(c(
      ER77375, ER77405, # alimony
      ER77377, ER77407, # help from relatives
      ER77379, ER77409, # help from others
      ER77381, ER77411,ER77439 # misc transfers
      ))
  ) %>% 
  ungroup %>% 
  select(
    id = ER77621, #ER72002,
    state = ER72004,
    
    net_wealth = ER77511,
    
    household_size = ER72016,
    
    sex = ER72018,
    age = ER72017,
    
    educ_hrp, 
    educ_partner,
    race,
    
    married_full = ER72024,
    
    income = ER77448,
    
    incwage,
    incbus,
    incss,
    incsupp,
    incwelfr,
    incinvst,
    incretir,
    incother,
    
    house_value = ER72031,
    house_tenure, 
    
    proptx = ER72045,
    mortamt1 = ER72053,
    mortamt2 = ER72074,
    taxincl = ER72055,
    insincl = ER72056,
    rent = ER72090,
    nvehic = ER72857,
    
    hcovany = ER76696,
    
    classwkr = ER72198,
    classwkr_partner = ER72475,
    
    work_status = ER72164,
    work_status_partner = ER72441,
    
    occ_hrp = ER72195, occ_partner = ER72472,
    ind_hrp = ER72196, ind_partner = ER72473,
    
    uhrswork = ER72172,
    uhrswork_partner = ER72449,
    
    wkswork = ER77249,
    wkswork_partner = ER77270
    ) 


# mutations to align with SCF labelling
psid <- psid %>% 
  mutate(
    sex = ifelse(sex == 1, "male", "female"),
    married_full = case_when(
      married_full == 1 ~ "married",
      married_full == 2 ~ "never married",
      married_full == 3 ~ "widowed",
      married_full == 4 ~ "divorced",
      married_full == 5 ~ "separated"
    ),
    taxincl = ifelse(taxincl == 1, "yes", "no"),
    insincl = ifelse(insincl == 1, "yes", "no"),
    nvehic = ifelse(nvehic > 10, NA, nvehic),
    hcovany = ifelse(hcovany == 1, "yes", "no"),
    rent = ifelse(rent == 99999, NA, rent),
    year = 2019,
    
    
    house_value = ifelse(house_value == 9999999 | house_value == 9999998, NA, house_value), 
    proptx = ifelse(proptx == 99999 | proptx == 99998, NA, proptx), 
    mortamt1 = ifelse(mortamt1 == 99999 | mortamt1 == 99998, NA, mortamt1),
    mortamt2 = ifelse(mortamt2 == 99999 | mortamt2 == 99998, NA, mortamt2)
  ) %>% 
  mutate_at(
    vars(matches("work_status")),
    ~ case_when(
      . == 1 ~ "employed",
      . == 3 ~ "unemployed",
      . == 2 | . >= 4 ~ "not in labor force",
      TRUE ~ "inappropriate"
    )
  ) %>% 
  mutate_at(
    vars(matches("classwkr")),
    ~ case_when(
      . == 1 | . == 2 ~ "employed",
      . == 3 ~ "self-employed",
      TRUE ~ "N/A"
    )
  ) %>% 
  mutate_at(
    vars(starts_with("occ")),
    ~ case_when(
      . == 0 ~ "N/A",
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
      TRUE ~ "N/A"
    )
  ) %>% 
  mutate_at(
    vars(starts_with("ind_")),
    ~ case_when(
      . == 0 ~ "N/A",
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
      TRUE ~ "N/A"
    )
  )

# asinh the skewed vars
psid <- psid %>% 
  mutate_at(
    vars(house_value, starts_with("inc")),
    ~ asinh(.)
  )


save(psid, file = "data/psid_2019.Rdata")

