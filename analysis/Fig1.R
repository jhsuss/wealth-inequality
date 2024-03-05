library(tidyverse)
library(sf)

# load inequality data
load("data/wealth_inequality.Rdata")
income_between <- haven::read_dta("data/convergence_CZ_1940_2021_householdreal_adjusted_BASICS.dta")

# create/merge between area inequality
income_between <- income_between %>% 
  select(
    year, czone,
    ww, mww, wi, mwi, pop
  ) %>% 
  filter(!is.na(mww)) %>% 
  group_by(year) %>% 
  summarise(
    `Income inequality across CZs` = reldist::gini(mww,pop)
  ) 

df_between <- ineq_cz %>% 
  group_by(year) %>% 
  summarise(
    `Wealth inequality across CZs` = reldist::gini(mean,hhlds)
  ) %>% 
  full_join(income_between, by = "year") %>%
  arrange(year) %>% 
  filter(year >= 1960) %>% 
  pivot_longer(cols = 2:ncol(.)) %>% 
  mutate(
    name = factor(name, levels = c(
      "Wealth inequality across CZs",
      "Income inequality across CZs"
    ))
  )


fig_inter_cz_ineq <- ggplot(
  df_between %>% 
    filter(!is.na(value), !is.na(name))
  ) +
  geom_line(
    aes(x = year,
        y = value,
        linetype = name,
        group = name),
    linewidth = 1.25
  ) + 
  labs(
    x = "Year",
    y = "Gini Coefficient",
    linetype = ""
    ,caption = "Inter-regional disparities in average wealth and income"
  ) + 
  theme_minimal(base_size = 18) + 
  theme(
    legend.position = "bottom",
    plot.caption = element_text(size = 20)
  ) +
  guides(
    linetype = guide_legend(
      nrow = 2, 
      byrow = T
    )
  )


# create within area inequality data
load("data/regions_lookup.Rdata")
rgns <- rgns %>%
  rename(
    division = region
  ) %>% 
  mutate(
    division = str_replace_all(division, " Div.*", ""),
    region = case_when(
      division %in% c("New England", "Middle Atlantic") ~ "Northeast",
      division %in% c("East North Central", "West North Central") ~ "Midwest",
      division %in% c("South Atlantic", "East South Central", "West South Central") ~ "South",
      division %in% c("Mountain", "Pacific") ~ "Pacific",
    )
  )

# Commuting Zone shapefiles
# https://github.com/dcl-docs/ussf
# install.packages("remotes")
# remotes::install_github("dcl-docs/ussf")
czs <- ussf::boundaries(geography = "cz") %>% 
  rename(cz = cz_1990) %>% 
  mutate(cz = as.numeric(cz)) %>% 
  st_transform("EPSG:5070")

ineq_cz <- ineq_cz %>% 
  left_join(
    czs,
    by = c("czone" = "cz")
  )


df_within <- ineq_cz %>% 
  filter(year == 1960 | year == 2020) %>% 
  distinct(year, czone, gini, place, state) %>% 
  arrange(czone, year) %>%
  pivot_wider(
    names_from=year, values_from = gini
  ) %>% 
  left_join(
    rgns %>% distinct(region, division, czone),
    by = "czone"
  )  

fig_between_cz_ineq <- ggplot(
  df_within %>% 
    distinct(place, .keep_all = T)
  ) +
  geom_point(
    aes(x = `1960`, y = `2020`, color = region), 
    size = 3, 
    shape = 16,
    alpha = 0.7
  ) +
  labs(
    color = "",
    x = "Wealth inequality within CZs, 1960\n(Gini Coefficients)",
    y = "Wealth inequality within CZs, 2020\n(Gini Coefficients)",
    caption = "Changes in wealth inequality within Commuting Zones"
  ) + 
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(size = 20)
  ) +
  guides(color = guide_legend(nrow=2, byrow = T)) +
  scale_color_viridis_d()



# bring together both figures
main_fig <- cowplot::plot_grid(
  fig_inter_cz_ineq, fig_between_cz_ineq, 
  nrow = 1
)

main_fig

# save
ggsave(main_fig,
       width = 16, height = 8,
       units = "in",
       dpi = 600,
       file = "main_figure.png")


