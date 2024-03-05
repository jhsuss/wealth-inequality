library(tidyverse)

# load clean psid data
load("data/psid_2019.Rdata")

psid <- psid %>% 
  na.omit %>% 
  filter(state != 0) %>%  # state 0 is for inappropriate US territory or foreign country
  mutate_at(
    vars(wkswork,wkswork_partner),
    ~ case_when(
      . == 0 ~ "N/A",
      . >= 1 & . <= 13 ~ "1-13",
      . >= 14 & . <= 26 ~ "14-26",
      . >= 27 & . <= 39 ~ "27-39",
      . >= 40 & . <= 47 ~ "40-47",
      . >= 48 & . <= 49 ~ "48-49",
      . >= 50 & . <= 52 ~ "50-52"
    )
  ) %>% 
  mutate_at(
    vars(occ_hrp,occ_partner, ind_hrp,ind_partner),
    ~ ifelse(. == "N/A", "0", .)
  )

# load models
year <- 2020

#load(paste0("data/train_test_",year,".Rdata"))

mods <- c(
  "svm",
  "knn",
  "rf",
  "glm",
  "net",
  "xgb",
  "ensemble"
)
types <- c(
  "pos","zero"
)

for (m in mods) {
  for (t in types) {
    
    x <- paste0("mod_",m,"_",t,"_",year,".Rdata")
    if (x %in% list.files("models/")) {
      load(paste0("models/",x))
    }
    
  }
  
}  # end load models



psid_pos <- psid %>% filter(net_wealth > 0)
test_pos <- tibble(
  preds_svm_pos = predict(mod_svm_pos, psid_pos),
  preds_knn_pos = predict(mod_knn_pos, psid_pos),
  preds_rf_pos = predict(mod_rf_pos, psid_pos),
  preds_xgb_pos = predict(mod_xgb_pos, psid_pos),
  preds_net_pos = predict(mod_net_pos, psid_pos),
  preds_glm_pos = predict(mod_glm_pos, psid_pos),
  preds_lasso_pos = predict(mod_lasso_pos, psid_pos)
)


preds_psid_pos <- predict(mod_ens_lm_pos, test_pos)


preds_pos <- tibble(
  preds_psid_pos,
  y = psid %>% filter(net_wealth > 0) %>% .$net_wealth %>% log,
  state = psid %>% filter(net_wealth > 0) %>% .$state
)

rmse_pos <- mean((preds_pos$preds_psid_pos - preds_pos$y)^2) %>% sqrt()

fig_pos <- ggplot(
  data = preds_pos,
  aes(y = preds_psid_pos,
      x = y
  )
) + 
  geom_point(alpha = 0.3) + 
  geom_abline(
    size = 1.2,
    slope = 1, 
    color = "dark orange", 
    linetype = "dashed"
  ) + 
  theme_minimal(base_size = 20) +
  labs(
    x = "Actual net wealth (log)", 
    y = "Predicted net wealth (log)",
    #caption = paste0("N = ", nrow(preds_pos) %>% scales::comma()),
    caption = "Positive wealth"#subtitle = paste0("Positive wealth \n","RMSE = ",round(rmse_pos,2))#,
    #title = "Out of sample validation (PSID 2019)"
  )



psid_neg <- psid %>% filter(net_wealth < 0)
test_neg <- tibble(
  preds_svm_neg = predict(mod_svm_neg, psid_neg),
  preds_knn_neg = predict(mod_knn_neg, psid_neg),
  preds_rf_neg = predict(mod_rf_neg, psid_neg),
  preds_xgb_neg = predict(mod_xgb_neg, psid_neg),
  preds_net_neg = predict(mod_net_neg, psid_neg),
  preds_glm_neg = predict(mod_glm_neg, psid_neg),
  preds_lasso_neg = predict(mod_lasso_neg, psid_neg)
)


preds_psid_neg <- predict(mod_ens_lm_neg, test_neg)

preds_neg <- data.frame(
  preds_psid_neg,
  y = psid %>% filter(net_wealth < 0) %>% .$net_wealth %>% asinh,
  state = psid %>% filter(net_wealth < 0) %>% .$state
)

rmse_neg <- mean((preds_neg$preds_psid_neg - preds_neg$y)^2) %>% sqrt()

fig_neg <- ggplot(
  data = preds_neg,
  aes(y = preds_psid_neg,
      x = y
  )
) + 
  geom_point(alpha = 0.3) + 
  geom_abline(
    size = 1.2,
    slope = 1, 
    color = "dark orange", 
    linetype = "dashed"
  ) + 
  theme_minimal(base_size = 20) +
  labs(
    x = "Actual net wealth (inverse hyperbolic sine)", 
    y = "Predicted net wealth (inverse hyperbolic sine)",
    #title = "",
    caption = "Negative wealth"#paste0("Negative wealth \n","RMSE = ",round(rmse_neg,2)),
    #caption = paste0("N = ", nrow(preds_neg) %>% scales::comma())
  )


fig_psid <- cowplot::plot_grid(fig_pos, fig_neg, nrow = 1) 

fig_psid