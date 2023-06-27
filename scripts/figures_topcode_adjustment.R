# figures for topcode adjustment

library(tidyverse)

year <- 2020

load(paste0("data/inequality_", year, ".Rdata"))

tmp <- ineq$state %>% 
  bind_rows(
    ineq$country %>% 
      mutate(state = "US")
  ) %>% 
  # reorder by largest difference in ginis
  mutate(
    diff = pred_wealth_new_gini - pred_wealth_gini,
    state = str_to_title(state)
  ) %>% 
  arrange(pred_wealth_new_gini) %>% 
  mutate(
    state = factor(state, state)
  ) %>% 
  select(
    state, pred_wealth_gini, pred_wealth_new_gini, diff
  ) 

fig_adj <- ggplot(tmp,
       aes(x = as.numeric(state))
  ) +
  geom_segment( 
    aes(
      xend = as.numeric(state), 
      y = pred_wealth_gini, 
      yend = pred_wealth_new_gini), 
    color = "grey"
  ) +
  geom_point( 
    data = tmp %>% 
      select(-diff) %>% 
      pivot_longer(
        cols = c(pred_wealth_gini, pred_wealth_new_gini)
      ),
    aes(
      y = value,
      color = name
    ), 
    size = 2 
  ) +
  coord_flip()+
  #theme_ipsum() +
  theme_minimal(base_size = 18) +
  labs(x = "", y = "Gini Coefficient") +
  scale_colour_manual(
    name = "", 
    values = c('#762a83','#009688'), 
    labels = c('Unadjusted topcodes','Adjusted topcodes')
  ) +
  theme(legend.position = "bottom") + 
  # add second axis for change in Gini 
  # h/t https://stackoverflow.com/questions/45361904/duplicating-and-modifying-discrete-axis-in-ggplot2
  scale_x_continuous(
    breaks = 1:length(levels(tmp$state)),
    labels = tmp$state,
    sec.axis = sec_axis(
      ~ .,
      breaks = 1:length(tmp$diff),
      labels = tmp$diff %>% round(digits=3),
      name = "Difference in Gini"
    )
  ) +
  guides(color = guide_legend(nrow=2, byrow = T))

ggsave(fig_adj,
       width = 12, height = 12,
       units = "in",
       dpi = 300,
       file = "figures/fig_topcode_adjustment.png"
       )


