binary_summary <- function (
  data, 
  lev = NULL, 
  model = NULL
  ) {
  pr_auc <- try(
    MLmetrics::PRAUC(
      data[, lev[2]],
      ifelse(data$obs == lev[2], 1, 0)),
    silent = TRUE)
  
  brscore <- try(
    mean((data[, lev[2]] - ifelse(data$obs == lev[2], 1, 0)) ^ 2),
    silent = TRUE)
  
  rocObject <- try(pROC::roc(
    ifelse(data$obs == lev[2], 1, 0), data[, lev[2]],
    direction = "<", quiet = TRUE), silent = TRUE
    )
  
  if (inherits(pr_auc, "try-error")) pr_auc <- NA
  
  if (inherits(brscore, "try-error")) brscore <- NA
  
  rocAUC <- if (inherits(rocObject, "try-error")) {
    NA
  } else {
    rocObject$auc
  }
  
  tmp <- unlist(
    e1071::classAgreement(
      table(data$obs,
            data$pred)))[c("diag", "kappa")]
  
  out <- c(Acc = tmp[[1]],
           Kappa = tmp[[2]],
           AUCROC = rocAUC,
           AUCPR = pr_auc,
           Brier = brscore,
           Precision = caret:::precision.default(data = data$pred,
                                                 reference = data$obs,
                                                 relevant = lev[2]),
           Recall = caret:::recall.default(data = data$pred,
                                           reference = data$obs,
                                           relevant = lev[2]),
           F = caret:::F_meas.default(data = data$pred, reference = data$obs,
                                      relevant = lev[2]))
  out
}
