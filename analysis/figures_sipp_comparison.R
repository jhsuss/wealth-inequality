library(tidyverse)

##### SIPP state-level wealth estimates 
path <- ""

load(paste0(path, "wealth_inequality.Rdata"))


path <- "C:/Users/326392/Documents/wealth-inequality/"

sipp20 <- read_csv(
  paste0(path,"data/state_wealth_estimates_2020.csv"),
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
       #title = "State mean wealth comparison (2020)",
       subtitle = paste0("r = ", round(cor_coef,2))
  ) + 
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
  theme_minimal(base_size = 14) 

fig_sipp <- cowplot::plot_grid(
  fig_sipp_mean, fig_sipp_median, nrow = 1
)

ggsave(fig_sipp,
       width = 12, height = 9,
       units = "in",
       dpi = 300,
       file = paste0(path,"figures/fig_validation_sipp_mean_median.png"))


