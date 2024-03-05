library(tidyverse)
#library(pROC)

year <- 2020

load("data/train_test_",year,".Rdata")

# load models
mods <- c(
  "svm",
  "knn",
  "rf",
  "glm",
  "net",
  "xgb",
  "ensemble"
)
types <- c("pos",
           "neg"
)

for (m in mods) {
  for (t in types) {
    
    x <- paste0("mod_",m,"_",t,"_",year,".Rdata")
    if (x %in% list.files(paste0(path,"models/"))) {
      load(paste0(path,"models/",x))
    }
    
  }
  
}  # end load models

test_pos <- test_df %>%
  filter(net_wealth > 0)

# level 1 predctions
preds_pos <- cbind(
  test_pos,
  preds_rf_pos = predict(mod_rf_pos, test_pos),
  preds_xgb_pos = predict(mod_xgb_pos, test_pos),
  preds_net_pos = predict(mod_net_pos, test_pos),
  preds_glm_pos = predict(mod_glm_pos, test_pos),
  preds_lasso_pos = predict(mod_lasso_pos, test_pos),
  preds_knn_pos = predict(mod_knn_pos, test_pos),
  preds_svm_pos = predict(mod_svm_pos, test_pos)
)

preds_pos$preds_ens <- predict(mod_ens_lm_pos, preds_pos)

# RMSE
rmse_pos <- mean((preds_pos$preds_ens - log(preds_pos$net_wealth))^2) %>% sqrt()



fig_pos <- ggplot(
  data = preds_pos,
  aes(y = preds_ens,
      x = log(net_wealth)
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
    caption = "Positive wealth"
  ) 



##### NEGATIVE wealth

test_neg <- test_df %>%
  filter(net_wealth < 0)

# level 1 predctions
preds_neg <- cbind(
  test_neg,
  preds_rf_neg = predict(mod_rf_neg, test_neg),
  preds_xgb_neg = predict(mod_xgb_neg, test_neg),
  preds_net_neg = predict(mod_net_neg, test_neg),
  preds_glm_neg = predict(mod_glm_neg, test_neg),
  preds_lasso_neg = predict(mod_lasso_neg, test_neg),
  preds_knn_neg = predict(mod_knn_neg, test_neg),
  preds_svm_neg = predict(mod_svm_neg, test_neg)
)

preds_neg$preds_ens <- predict(mod_ens_lm_neg, preds_neg)


# RMSE
rmse_neg <- mean((preds_neg$preds_ens - asinh(preds_neg$net_wealth))^2) %>% sqrt()

fig_neg <- ggplot(
  data = preds_neg,
  aes(y = preds_ens,
      x = asinh(net_wealth)
  )
) + 
  geom_point(alpha = .3) + 
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
    caption = "Negative wealth" 
  )


figs_test_sample <- cowplot::plot_grid(fig_pos,fig_neg, nrow = 1)

figs_test_sample
