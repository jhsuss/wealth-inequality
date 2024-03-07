########## Maps of wealth and wealth inequality ---------- 

library(sf)
library(cowplot)
library(tidyverse)
library(cowplot)

# load shapefiles
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

df <- ineq_cz %>% 
  filter(
    year == 1960 | year == 2020
  ) %>% 
  arrange(czone, year) %>% 
  group_by(year) %>% 
  mutate(
    mean = scale(mean) %>% as.numeric,
    gini_scaled = scale(gini) %>% as.numeric
  ) %>% 
  ungroup %>% 
  mutate(
    interval_mean = cut_interval(mean, n = 6),
    interval_gini = cut(gini_scaled, c(-4.17,-1.84,-0.674,0.489,1.65,2.81,3.98))
  ) %>% 
  relocate(matches("counts"),.after  = mean)


map_1960_mean <- ggplot(
  data = df %>% 
    filter(year == 1960)
) +
  geom_sf(
    mapping = aes(
      geometry = geometry,
      fill = interval_mean,
    ), color = "gray"
  ) + 
  labs( 
    fill = "Mean wealth 1960\n(Z-scores)"
    ) +
  scale_fill_manual(
    values = c(
      "#fef0d9",
      "#fdd49e",
      "#fdbb84",
      "#fc8d59",
      "#ef6548",
      "#d7301f",
      "#990000"
    )
  ) +
  theme_map(font_size = 20)


map_2020_mean <- ggplot(
  data = df %>% 
    filter(year == 2020)
  ) +
  geom_sf(
    mapping = aes(
      geometry = geometry,
      fill = interval_mean,
    ), color = "gray"
  ) + 
  labs( fill = "Mean wealth 2020\n(Z-scores)" , title = " " ) +
  scale_fill_manual(
    values = c(
      "#fef0d9",
      "#fdd49e",
      "#fdbb84",
      "#fc8d59",
      "#ef6548",
      "#d7301f",
      "#990000"
    )
  ) +
  theme_map(font_size = 20)



map_between_inequality_change <- egg::ggarrange(
  map_1960_mean,
  map_2020_mean,
  ncol = 1
)




map_1960_gini <- ggplot(
  data = df %>% 
    filter(year == 1960)
) +
  geom_sf(
    mapping = aes(
      geometry = geometry,
      fill = interval_gini
    ), color = "gray"
  ) + 
  labs( fill = "Gini wealth 1960\n(Z-scores)" 
  ) +
  scale_fill_manual(
    values = c(
      
      "#fef0d9",
      "#fdd49e",
      "#fdbb84",
      "#fc8d59",
      "#ef6548",
      "#d7301f",
      "#990000"
    )
  ) + 
  theme_map(font_size = 20)


map_2020_gini <- ggplot(
  data = df %>% 
    filter(year == 2020)
) +
  geom_sf(
    mapping = aes(
      geometry = geometry,
      fill = interval_gini
    ), color = "gray"
  ) + 
  labs( fill = "Gini wealth 2020\n(Z-scores)"  ,
        title = " ") +
  scale_fill_manual(
    values = c(
      
      "#fef0d9",
      "#fdd49e",
      "#fdbb84",
      "#fc8d59",
      "#ef6548",
      "#d7301f",
      "#990000"
      
    )
  ) + 
  theme_map(font_size = 20)



map_within_inequality_change <- egg::ggarrange(
  map_1960_gini,
  map_2020_gini,
  ncol = 1
)


map_between_inequality_change
map_within_inequality_change
