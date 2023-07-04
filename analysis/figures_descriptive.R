library(tidyverse)
library(gghighlight)
library(sf)
library(ggrepel)

# load inequality data
load("wealth_inequality.Rdata")

tmp <- ineq_cz %>% 
  group_by(year) %>%
  summarise(
    `Wealth inequality across CZs` = ineq::Gini(mean),
    `Income inequality across CZs` = ineq::Gini(income_mean)
  ) %>% 
  pivot_longer(cols = 2:3) %>% 
  mutate(
    name = factor(name, levels = c(
      "Wealth inequality across CZs",
      "Income inequality across CZs"
    ))
  )

fig_inter_cz_ineq <- ggplot(
  tmp
  ) +
  geom_line(
    aes(x = year,
        y = value,
        linetype = name),
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
  guides(linetype = guide_legend(nrow=2, byrow = T))


ggsave(fig_inter_cz_ineq,
       width = 12, height = 9,
       units = "in",
       dpi = 300,
       file = "figures/fig_inter-regional_inequality.png"
       )


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
#https://github.com/dcl-docs/ussf
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


tmp <- ineq_cz %>% 
  filter(year == 1960 | year == 2020) %>% 
  distinct(year, czone, gini, place, state) %>% 
  mutate(place = paste0(place, ", ", state)) %>% 
  arrange(czone, year) %>%
  pivot_wider(
    names_from = year, 
    values_from = gini
  ) %>% 
  left_join(
    rgns %>% distinct(region, division, czone),
    by = "czone"
  )  

fig_scatter_labels <- ggplot(
  tmp %>% 
    filter(czone != "34401") %>% 
    distinct(place, .keep_all = T)
) +
  geom_abline(slope = 1, linetype = "dashed", size = 1) +
  geom_point(
    aes(x = `1960`, y = `2020`, color = region), 
    size = 1.8) +
  geom_text_repel(
    data = tmp %>%
      filter(czone != "34401") %>% 
      distinct(place, .keep_all = T) %>% 
      filter(abs(`2020` - `1960`) >= 0.1),
    aes(x = `1960`, y = `2020`, color = region, label = place),
    show.legend = FALSE,
    force = 20,
    size = 3,
    max.overlaps = 40
  ) +
  labs(
    color = "",
    x = "Wealth inequality within CZs, 1960\n(Gini Coefficients)",
    y = "Wealth inequality within CZs, 2020\n(Gini Coefficients)",
    caption = "Changes in wealth inequality within Commuting Zones"
  ) + 
  scale_x_continuous(limits = c(0.6,0.95)) + 
  scale_y_continuous(limits = c(0.6,0.95)) + 
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(size = 20)
  ) +
  guides(color = guide_legend(nrow=2, byrow = T))

ggsave(fig_scatter_labels,
       width = 12, height = 9,
       units = "in",
       dpi = 300,
       file = "figures/scatter_labels.png"
       )


main_fig <- cowplot::plot_grid(
  fig_inter_cz_ineq, fig_scatter_labels, 
  nrow = 1
)

ggsave(main_fig,
       width = 16, height = 8,
       units = "in",
       dpi = 300,
       file = "figures/main_figure.png"
       )



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

fig_spagh_highlighted <- ggplot(spagh) +
  geom_line(
    aes(
      x = year, 
      y = mean_wealth_ratio, 
      group = place, 
      color = place
    ),
    alpha = 0.4, size = 1.2
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  labs(
    x = "Year", 
    y = "Relative wealth", 
    color = "Region"
  )  +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(1940,2020,10)) +
  theme(
    axis.text.x = element_text(angle = 90)
  ) +
  gghighlight(max(mean_wealth_ratio) >= 3) 

ggsave(fig_spagh_highlighted,
       width = 12, height = 9,
       units = "in",
       dpi = 300,
       file = "figures/fig_spaghetti_highlight.png"
       )
