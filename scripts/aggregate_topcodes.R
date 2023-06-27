library(tidyverse)

# aggregate top codes
top1940_1980 <- readxl::read_xls("data/1940-1980_topcodes.xls")
top1990_1 <- readxl::read_xls("data/1990_1percent_topcodes.xls")
top1990_5 <- readxl::read_xls("data/1990_5percent_topcodes.xls")
top2000_1 <- readxl::read_xls("data/2000_1percent_topcodes.xls")
top2000_5 <- readxl::read_xls("data/2000_5percent_topcodes.xls")
top2005 <- readxl::read_xls("data/2005acs_topcodes.xls")
top2010 <- readxl::read_xls("data/2010acs_topcodes.xls")
top2011 <- readxl::read_xls("data/0911acs_topcodes.xls")
top2015 <- readxl::read_xls("data/2015acs_topcodes.xls")
top2019 <- readxl::read_xls("data/2019acs_topcodes.xls")
top2020 <- readxl::read_xls("data/2020acs_topcodes.xls")
top2021 <- readxl::read_xls("data/2021acs_topcodes.xls")

fips <- top1990_1 %>% distinct(state,statefip)
save(fips, file = "data/state_fips.Rdata")
# 198001
# 197003
# 197004
# 196002 # 5% sample
# 195001
# 194002 # 100% count

topcodes <- bind_rows(
  top1940_1980 %>% mutate(sample = as.character(sample)),
  top1990_1 %>% mutate(year = 1990, sample = "199002"),
  top1990_5 %>% mutate(year = 1990, sample = "199001"),
  top2000_1 %>% mutate(year = 2000, sample = "200007") %>% left_join(fips) %>% mutate(statefip = ifelse(state == "Washington, DC", 11, statefip)),
  top2000_5 %>% mutate(year = 2000, sample = "200001") %>% left_join(fips) %>% mutate(statefip = ifelse(state == "Washington, DC", 11, statefip)),
  top2005 %>% mutate(year = 2005, sample = "200501") %>% left_join(fips) %>% mutate(statefip = ifelse(state == "Washington, DC", 11, statefip)),
  top2010 %>% mutate(year = 2010, sample = "201001"),
  top2011 %>% 
    mutate(year = 2011, sample = "201103") %>% 
    rename(
      incbus = incbus00,
      incbus_bottomcode = incbus00_min,
      incinvst_bottomcode = incinvst_min,
      proptx = proptx99
    ) %>% 
    rename_at(vars(proptx:incother),~ str_c(.,"_topcode")),
  top2015 %>% mutate(year = 2015, sample = "201501"),
  top2019 %>% mutate(year = 2019, sample = "201901"),
  top2020 %>% mutate(year = 2020, sample = "202001"),
  top2021 %>% mutate(year = 2021, sample = "202101")
)


topcodes <- topcodes %>%
  mutate(
    house_value_topcode = ifelse(year == 1990, 400000, house_value_topcode),
    house_value_topcode = ifelse(year >= 2000 & year <= 2005, 1e6, house_value_topcode),
    statefip = ifelse(is.na(statefip) & str_detect(state,"Puerto"),72, statefip)
)

topcodes <- topcodes %>% 
  select(year, sample, state, statefip, everything())


save(topcodes, file = "data/topcodes.Rdata")
