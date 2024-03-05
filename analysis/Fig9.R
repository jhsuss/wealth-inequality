##### Comparison with SIPP state-level wealth estimates --------

library(tidyverse)

load("data/wealth_inequality.Rdata")

# SIPP data downloaded from: https://www.census.gov/topics/income-poverty/wealth/data/tables.2020.List_2110684178.html#list-tab-List_2110684178 
sipp20 <- read_csv(
  "data/state_wealth_estimates_2020.csv",
  na = c("(B)")
)

sipp20 <- sipp20 %>% 
  left_join(
    ineq_state %>% 
      filter(year == 2020) %>% 
      mutate(
        state = ifelse(year == 2020, str_to_title(state), state)
      ) %>%
      select(
        state,
        matches("^(mean|median)$")
      )
  ) 

cor_coef <- cor(
  sipp20$sipp_mean, 
  sipp20$mean, 
  use = "complete.obs")

fig_sipp_mean <- ggplot(
  sipp20,
  aes(x = mean, y = sipp_mean)
) +
  geom_point() + 
  geom_smooth(method = "lm", color = "dark orange") +
  labs(y = "Mean (SIPP)", 
       x = "Mean (Census imputed)",
       subtitle = paste0("r = ", round(cor_coef,2))
  ) + 
  scale_x_continuous(labels = scales::label_scientific()) +
  theme_minimal(base_size = 14) 

cor_coef <- cor(
  sipp20$sipp_median, 
  sipp20$median, 
  use = "complete.obs")

fig_sipp_median <- ggplot(
  sipp20,
  aes(x = median, y = sipp_median)
) +
  geom_point() + 
  geom_smooth(method = "lm", color = "dark orange") +
  labs(y = "Median (SIPP)", 
       x = "Median (Census imputed)",
       #title = "State median wealth comparison (2020)",
       subtitle = paste0("r = ", round(cor_coef,2))
  ) + 
  scale_x_continuous(labels = scales::label_scientific()) +
  theme_minimal(base_size = 14) 

fig_sipp <- cowplot::plot_grid(
  fig_sipp_mean, fig_sipp_median, nrow = 1
)

fig_sipp
