library(tidyverse)

wid <- read_csv("data/WID_gini.csv")
wid_top1 <- read_csv("data/WID_top1.csv")

wid <- wid_top1 %>% 
  left_join(wid, by = c("year")) %>% 
  rename_at(
    vars(gini, matches("^(top|bottom)")),
    ~ str_c("wid_",.)
  )

# load Kuhn et al data
kuhn <- read_csv("data/kuhn_gini.csv")
kuhn <- kuhn %>% 
  mutate_at(
    vars(gini_kuhn, gini90_kuhn,gini99_kuhn),
    ~ . / 100
  )
# load combined ineq data
load("wealth_inequality.Rdata")

# filter for country-level and join with WID levels
tmp <- wid %>% 
  full_join(
    ineq_country,
    by = "year"
  ) %>% 
  distinct %>% 
  left_join(
    kuhn, 
    by = "year"
  )


# Gini comparison
gini <- tmp %>%
  filter(year >= 1960) %>% 
  select(
    year,
    Ensemble = pred_wealth_new_gini,
    `Saez & Zucman` = wid_gini,
    Kuhn = gini_kuhn
  ) %>% 
  pivot_longer(
    cols = c(Ensemble, Kuhn, `Saez & Zucman`),
    names_to = "estimate"
  ) %>% 
  filter(!is.na(value))

fig_gini <- ggplot(
  gini,
  aes(
    x = year, 
    y = value
    )
  ) + 
  geom_line(
    aes(linetype = estimate)
    ) +
  geom_point(
    aes(shape = estimate)
  ) +
  # add CIs
  # geom_ribbon(
  #   data = tmp %>%
  #     filter(
  #       year >= 1960,
  #       !is.na(pred_wealth_new_gini)
  #       ),
  #   aes(
  #     y = pred_wealth_new_gini,
  #     ymin = pred_wealth_new_gini_lower,
  #     ymax = pred_wealth_new_gini_upper
  #   ),
  #   alpha = 0.2,
  #   color = "gray"
  # ) +
  labs(x = "Year", y = "Gini", linetype = "", shape = "") +
  theme_minimal(base_size = 14) 

# share comparison
share <- tmp %>%
  # TODO create group for top1, top10, bottom to facet
  #mutate()
  select(
    year,
    `Ensemble Top 1%` = pred_wealth_new_top1,
    `Ensemble Top 10%` = pred_wealth_new_top10,
    `Ensemble Bottom 50%` = pred_wealth_new_bottom50,
    `Saez & Zucman Top 1%` = wid_top1,
    `Saez & Zucman Top 10%` = wid_top10,
    `Saez & Zucman Bottom 50%` = wid_bottom50
  ) %>% 
  pivot_longer(
    cols = c(`Ensemble Top 1%`:`Saez & Zucman Bottom 50%`),
    names_to = "estimate"
  ) %>% 
  filter(
    !is.na(value),
    year >= 1960
    )

fig_share <- ggplot(
  share,
  aes(
    x = year, 
    y = value,
    group = estimate
  )
  ) + 
  geom_line(
    aes(linetype = estimate)
  ) +
  geom_point(
    aes(shape = estimate)
  ) +
  labs(x = "Year", y = "Proportion total wealth", linetype = "", shape = "") +
  theme_minimal(base_size = 14)



save(
  fig_gini,
  fig_share,
  file = "figures/figs_country_level_validation.Rdata"
)

fig_country_validation <- cowplot::plot_grid(
  fig_gini + 
    theme_minimal(base_size = 14), 
  fig_share +
    theme_minimal(base_size = 14), 
  nrow = 2
  )

ggsave(fig_country_validation,
       width = 12, height = 9,
       units = "in",
       dpi = 300,
       file = "figures/fig_country_validation.png")


ggsave(fig_share,
       width = 1900, height = 1000,
       units = "px",
       dpi = 300,
       file = "figures/fig_validation_saez_shares_updated.jpg"
       )

ggsave(fig_gini,
       width = 12, height = 9,
       units = "in",
       dpi = 300,
       file = "figures/fig_country_validation_gini.jpg"
       )


# top .1% and .01% comparison
saez <- read_csv("data/saez_zucman_2016.csv")

# https://www.federalreserve.gov/releases/z1/dataviz/dfa/distribute/table/#quarter:129;series:Net%20worth;demographic:networth;population:all;units:shares
dfa_shares <- read_csv("data/scf_shares.csv")
dfa_shares <- dfa_shares %>% 
  filter(
    !is.na(Date),
    str_detect(Date, "Q4$")
  ) %>% 
  mutate(
    year = str_extract(Date,"^.{4}")
  ) %>% 
  select(
    year, 
    `Top 0.1% (DFA)` = `Top 0.1%`
  ) %>% 
  mutate_all(
    ~ as.numeric(.)
  )


saez <- saez %>%
  select(
    year, `Top 0.1%`, `Top 10%`, `Top 0.01%`
    ) %>%
  mutate_at(
    vars(`Top 0.1%`, `Top 10%`, `Top 0.01%`),
    ~ str_replace_all(.,"%","") %>% 
      as.numeric
  ) %>% 
  full_join(
    ineq_country %>% 
      select(
        year, 
        pred_wealth_new_top.1, 
        pred_wealth_new_top10,
        pred_wealth_new_top.01
        ) %>% 
      mutate_at(
        vars(pred_wealth_new_top.1, 
             pred_wealth_new_top10,
             pred_wealth_new_top.01),
        ~ . * 100 
      ),
    by = "year"
  ) %>% 
  distinct %>% 
  full_join(
    dfa_shares, 
    by = "year"
  )


fig_0.1 <- ggplot(
  saez %>% 
    filter(year >= 1950) %>% 
    select(
      year,
      Ensemble = pred_wealth_new_top.1,
      `Top 0.1% (Saez & Zucman)` = `Top 0.1%`,
      `Top 0.1% (DFA)`
    ) %>% 
    pivot_longer(
      cols = c(Ensemble, `Top 0.1% (Saez & Zucman)`, `Top 0.1% (DFA)`),
      names_to = "estimate"
    ) %>% 
    filter(!is.na(value)),
  aes(
    x = year, 
    y = value
  )
) + 
  geom_line(
    aes(linetype = estimate)
  ) +
  geom_point(
    aes(shape = estimate)
  ) +
  labs(x = "Year", y = "Share (%)", linetype = "", shape = "") +
  theme_minimal(base_size = 14) 

ggsave(fig_0.1,
       width = 12, height = 9,
       units = "in",
       dpi = 300,
       file = "figures/fig_validation_saez_shares_0.1.png"
       )



  
fig_0.01 <- ggplot(
  saez %>% 
    filter(year >= 1950) %>% 
    select(
      year,
      Ensemble = pred_wealth_new_top.01,
      `Saez & Zucman Top 0.01%` = `Top 0.01%`
    ) %>% 
    pivot_longer(
      cols = c(Ensemble, `Saez & Zucman Top 0.01%`),
      names_to = "estimate"
    ) %>% 
    filter(!is.na(value)),
  aes(
    x = year, 
    y = value
    )
  ) + 
  geom_line(
    aes(linetype = estimate)
  ) +
  geom_point(
    aes(shape = estimate)
  ) +
  labs(x = "Year", y = "Share (%)", linetype = "", shape = "") +
  theme_minimal(base_size = 14) 

ggsave(fig_0.01,
       width = 12, height = 9,
       units = "in",
       dpi = 300,
       file = "figures/fig_validation_saez_shares_0.01.png"
       )
