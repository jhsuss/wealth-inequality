########### Spaghetti plot -------
library(tidyverse)
library(ggrepel)
library(sf)

#https://github.com/dcl-docs/ussf
#remotes::install_github("dcl-docs/ussf")
czs <- ussf::boundaries(geography = "cz") %>% #%>% ggplot() + geom_sf()
  rename(cz = cz_1990) %>% 
  mutate(cz = as.numeric(cz)) %>% 
  st_transform("EPSG:5070")

load("data/wealth_inequality.Rdata")

ineq_cz <- ineq_cz %>% 
  relocate(year,.before = czone) %>%  
  left_join(
    czs,
    by = c("czone" = "cz")
  ) %>% 
  arrange(czone,year)


country_avg <- ineq_country %>% 
  distinct(year,mean_wealth = mean) 

# sphagetti plot
spagh <- ineq_cz %>% 
  group_by(year) %>%
  mutate(
    mean_country_wealth = mean(mean),
    mean_country_gini = mean(gini)
  ) %>% 
  ungroup() %>% 
  mutate(
    mean_wealth_ratio = mean / mean_country_wealth,
    gini_ratio = gini / mean_country_gini,
    place = paste0(place, ", ", state)
  ) 

fig_spagh_highlighted <- ggplot(
  spagh %>%
    distinct(place,year,.keep_all = T) %>% 
    group_by(place) %>% 
    mutate(
      high = ifelse(
        max(mean_wealth_ratio) >= 2.5 & !str_detect(place, "Quincy|Brick|Newark" ) , "yes", "no"
      )
    ) %>% 
    ungroup(),
  aes(
    x = year, 
    y = mean_wealth_ratio, 
    group = place, 
    color = high
    )
  ) + 
  geom_line(alpha = 0.4, size = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  labs(
    x = "Year", 
    y = "Relative wealth"
  )  +
  scale_x_continuous(breaks = seq(1960,2030,10)) +
  theme(
    axis.text.x = element_text(angle = 90)
  ) +
  scale_color_manual( 
    values = c( 
      "yes"="dark orange", 
      "no"="gray" 
    ), 
    guide = "none" ) +
  geom_text_repel(
    data = spagh  %>%
      distinct(place,year,.keep_all = T) %>% 
      group_by(place) %>% 
      mutate(
        high = ifelse(
          max(mean_wealth_ratio) >= 2.5 & !str_detect(place, "Quincy|Brick|Newark" ) , "yes", "no"
        )
      ) %>% 
      ungroup() %>% 
      filter(high == "yes") %>%
      arrange(desc(year)) %>% 
      distinct(place, .keep_all = T) %>% 
      mutate(year == 2020),
    aes(label = place, group = place), nudge_x = 2, direction = "y", hjust = "left"
  ) + 
  theme_minimal(base_size = 20) 


fig_spagh_highlighted

