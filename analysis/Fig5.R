
################# TOPCODE ADJUSTMENT IMPACT --------------

library(tidyverse)

load("data/wealth_inequality.Rdata")
# load unadjusted inequality estimates at the state-level
load("data/state_topcode_unadjusted_2020.Rdata")

df_adj <- ineq_state %>% 
  filter(year == 2020) %>% 
  select(
    gini_new = gini,
    everything()
  ) %>% 
  left_join(
    state %>% select(state,gini)
  ) %>% 
  # reorder by largest difference in Gini
  mutate(
    diff = gini_new - gini,
    state = str_to_title(state)
  ) %>% 
  arrange(gini_new) %>% 
  mutate(
    state = factor(state, state)
  ) %>% 
  select(
    state, gini_new, gini, diff
  ) 

fig_adj <- ggplot(
  df_adj,
  aes(x = as.numeric(state))
  ) +
  geom_segment( 
    aes(
      xend = as.numeric(state), 
      y = gini, 
      yend = gini_new), 
    color = "grey"
  ) +
  geom_point( 
    data = df_adj %>% 
      select(-diff) %>% 
      pivot_longer(
        cols = c(gini, gini_new)
      ),
    aes(
      y = value,
      color = name
    ), 
    size = 2 
  ) +
  coord_flip()+
  theme_minimal(base_size = 18) +
  labs(x = "", y = "Gini Coefficient") +
  scale_colour_manual(
    name = "", 
    values = c('#fdae6b','#e6550d'), 
    labels = c('Unadjusted topcodes','Adjusted topcodes')
  ) +
  theme(legend.position = "bottom") + 
  # add second axis for change in Gini 
  scale_x_continuous(
    breaks = 1:length(levels(df_adj$state)),
    labels = df_adj$state,
    sec.axis = sec_axis(
      ~ .,
      breaks = 1:length(df_adj$diff),
      labels = df_adj$diff %>% round(digits=3),
      name = "Difference in Gini"
    )
  ) +
  guides(color = guide_legend(nrow=2, byrow = T))

fig_adj

