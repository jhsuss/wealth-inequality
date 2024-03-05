library(tidyverse)

year <- 2020

load(paste0("data/train_test_",year,".Rdata"))

load("data/shapley_values_binary.Rdata")
phi_bin <- phi

load("data/shapley_values_pos.Rdata")
phi_pos <- phi

load("data/shapley_values_neg.Rdata")
phi_neg <- phi

# Shapley values overall
phi_bin_total <- phi_bin %>% 
  summarise_all(
    ~ mean(abs(.))
  ) %>% 
  t() %>% 
  as.data.frame %>% 
  mutate(var = row.names(.)) %>%  
  tibble() %>% 
  rename(shap = V1) %>% 
  arrange(desc(shap))

# Shapley values for those over 1 million overall
phi_bin <- test_df %>% 
  select(net_wealth) %>% 
  cbind(phi_bin)

phi_bin %>% 
  filter(net_wealth <= 25000) %>% 
  summarise_all(
    ~ mean(abs(.))
  ) %>% 
  t() %>% 
  as.data.frame %>% 
  mutate(var = row.names(.)) %>%  
  tibble() %>% 
  filter(var != "net_wealth") %>% 
  rename(shap = V1) %>% 
  arrange(desc(shap)) 

fig_bin <- ggplot(
  phi_bin_total %>% 
    arrange(shap) %>% 
    mutate(
      var = ifelse(var == "married_full","marst",var),
      var = ifelse(var == "house_value","valueh",var),
      var = ifelse(var == "house_tenure","ownershp",var),
      var = str_replace(var, "work_status","empstat"),
      var = str_to_upper(var),
      var = factor(var, levels = var)
      ),
  aes(y = var, x = shap)
  ) +
  geom_col() +
  labs(x = "Mean absolute Shapley value",
       y = "",
       subtitle = "Binary ensemble"
       ) +
  theme_minimal(base_size = 12)

# Positive values
phi_pos <- test_df %>% 
  select(net_wealth) %>% 
  cbind(phi_pos)

phi_pos_total <- phi_pos %>%
  filter(net_wealth > 0) %>% 
  summarise_all(
    ~ mean(abs(.))
  ) %>% 
  t() %>% 
  as.data.frame %>% 
  mutate(var = row.names(.)) %>%  
  tibble() %>% 
  rename(shap = V1) %>% 
  arrange(desc(shap)) %>% 
  filter(var != "net_wealth")

fig_pos <- ggplot(
  phi_pos_total %>% 
    arrange(shap) %>% 
    mutate(
      var = ifelse(var == "married_full","marst",var),
      var = ifelse(var == "house_value","valueh",var),
      var = ifelse(var == "house_tenure","ownershp",var),
      var = str_replace(var, "work_status","empstat"),
      var = str_to_upper(var),
      var = factor(var, levels = var)
    ),
  aes(y = var, x = shap)
) +
  geom_col() +
  labs(x = "Mean absolute Shapley value",
       y = "",
       subtitle = "Positive wealth ensemble"
  ) +
  theme_minimal(base_size = 12)

# over 10million
phi_pos_rich <- phi_pos %>% 
  filter(
    net_wealth >= 1e7
  ) %>% 
  select(-net_wealth) %>% 
  summarise_all(
    ~ mean(abs(.))
  ) %>% 
  t() %>% 
  as.data.frame %>% 
  mutate(var = row.names(.)) %>%  
  tibble() %>% 
  rename(shap = V1) %>% 
  arrange(desc(shap))

# And for 25k-0
phi_pos_poor <- phi_pos %>% 
  filter(
    net_wealth > 0 & net_wealth <=25000
  ) %>% 
  select(-net_wealth) %>% 
  summarise_all(
    ~ mean(abs(.))
  ) %>% 
  t() %>% 
  as.data.frame %>% 
  mutate(var = row.names(.)) %>%  
  tibble() %>% 
  rename(shap = V1) %>% 
  arrange(desc(shap))


tmp <- bind_rows(
  phi_pos_rich %>% 
    slice(1:10) %>% 
    mutate(level = "Households at and over $10mn"),
  phi_pos_poor %>% 
    slice(1:10) %>% 
    mutate(level = "Households between $0 and $25k")
  ) 

fig_pos_subset <- ggplot(
 tmp %>% 
   mutate(
     var = ifelse(var == "married_full","marst",var),
     var = ifelse(var == "house_value","valueh",var),
     var = ifelse(var == "house_tenure","ownershp",var),
     var = str_replace(var, "work_status","empstat"),
     var = str_to_upper(var)
   ),
 aes(y = reorder(var,shap), x = shap)
  ) +
  geom_col(
    aes(fill = level), 
    position = "dodge",
    width = 0.5
    ) +
  scale_fill_brewer() + 
  labs(x = "Mean absolute Shapley value",
       y = "",
       #subtitle = "Positive wealth ensemble",
       fill = ""
  ) +
  theme_minimal(base_size = 18)+
  theme(
    legend.position = "bottom"
  ) 

# for negative wealth
phi_neg <- test_df %>% 
  select(net_wealth) %>% 
  cbind(phi_neg)

phi_neg_total <- phi_neg %>%
  filter(net_wealth < 0) %>% 
  summarise_all(
    ~ mean(abs(.))
  ) %>% 
  t() %>% 
  as.data.frame %>% 
  mutate(var = row.names(.)) %>%  
  tibble() %>% 
  rename(shap = V1) %>% 
  arrange(desc(shap)) %>% 
  filter(var != "net_wealth")

fig_neg <- ggplot(
  phi_neg_total %>% 
    arrange(shap) %>% 
    mutate(
      var = ifelse(var == "married_full","marst",var),
      var = ifelse(var == "house_value","valueh",var),
      var = ifelse(var == "house_tenure","ownershp",var),
      var = str_replace(var, "work_status","empstat"),
      var = str_to_upper(var),
      var = factor(var, levels = var)
    ),
  aes(y = var, x = shap)
) +
  geom_col() +
  labs(x = "Mean absolute Shapley value",
       y = "",
       subtitle = "Negative wealth ensemble"
  ) +
  theme_minimal(base_size = 12)




fig_shap <- cowplot::plot_grid(fig_bin,fig_pos,fig_neg, nrow= 1) 

fig_shap
fig_pos_subset
