############# country-level validation -------------
library(tidyverse)
library(ggrepel)

load("data/wealth_inequality.Rdata")
smith <- read_csv("data/smith_fig1b_replication_data.csv")

smith <- smith %>% 
  select(
    year = Year, 
    matches("^Top (1|0.1|0.01|)")
  ) %>% 
  full_join(
    ineq_country %>% 
      select(
        year, 
        `Top 1% Ensemble` = top1,
        `Top 0.1% Ensemble` = top.1,
        `Top 0.01% Ensemble` = top.01,
        `Top 0.001% Ensemble` = top.001
      ) %>% 
      filter(year >= 1950) %>% 
      mutate_at(
        vars(matches("Ensemble")),
        ~ . * 100
      )
  ) %>% 
  arrange(year) %>% 
  pivot_longer(
    cols = -year,
    names_to = "estimate"
  ) %>% 
  filter(!is.na(value)) %>% 
  mutate(
    threshold = str_extract(estimate, "Top.*%"),
    estimate = str_remove(estimate, "Top.*% ")
  ) %>% 
  mutate(
    estimate = ifelse(estimate == "Baseline Estimate", "SZZ (2023)", estimate),
    estimate = ifelse(estimate == "Harmonized SCF", "SCF", estimate),
    estimate = ifelse(estimate == "Harmonized SCF with Forbes", "SCF w/ Forbes", estimate),
    estimate = ifelse(estimate == "Equal Return, Individual (PSZ 2018, Extended)", "PSZ (2018)", estimate),
    estimate = ifelse(estimate == "Revised Saez Zucman (2020)", "SZ (2020)", estimate)
  ) %>% 
  mutate(
    estimate = factor(
      estimate,
      levels = c(
        "Ensemble", 
        "SZZ (2023)",
        "SZ (2020)",
        "PSZ (2018)",
        "SCF w/ Forbes",
        "SCF"
      )
    )
  )

smith$threshold <- smith$threshold %>% 
  factor(
    levels = rev(
      sort(
        unique(smith$threshold)
      )
    )
  )

country_validation <- ggplot(
  smith %>% filter(estimate == "Ensemble", year > 1950),
  aes(
    x = year, 
    y = value
    )
  ) + 
  geom_line(
    aes(group = estimate),
    data = smith %>% filter(estimate != "Ensemble", year > 1950),
    size = 1.1,
    color = "gray"
  ) +
  geom_point(
    data = smith %>% filter(estimate != "Ensemble", year > 1950),
    aes(group = estimate),
    shape = 2,
    size = 1.1,
    color = "gray"
  ) +
  geom_line(
    size = 1.2,
    color = "dark orange"
  ) +
  geom_point(
    size = 1.2
  ) +
  facet_wrap( ~ threshold, scales = "free_y", nrow = 2) +
  labs(
    x = "Year", y = "Share (%)" 
  ) +
  geom_text_repel(
    data = smith %>% 
      group_by(estimate) %>% 
      filter(year == max(year)),
    aes(label = estimate),
    max.overlaps = 11
  ) + 
  theme_minimal(base_size = 20) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(5,"cm"),
    axis.text.x = element_text(angle = 90)
  )  + 
  scale_x_continuous(
    n.breaks = 9
  ) 

country_validation

