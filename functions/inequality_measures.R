# weighted functions that can also work with boot()
# helper functions 

# unweighted
# Top1 <- function(x) {
#   sum(x[x >= quantile(x,p=0.99,na.rm=T)],na.rm = T) / sum(x,na.rm = T)
# }
# nine_ten <- function(x) {
#   quantile(x,p=0.90,na.rm = T) / quantile(x,p=0.10,na.rm=T)
# }
# nine_five <- function(x) {
#   quantile(x,p=0.90,na.rm=T) / quantile(x,p=0.50,na.rm=T)
# }
# five_ten <- function(x) {
#   quantile(x,p=0.50,na.rm=T) / quantile(x,p=0.10,na.rm=T)
# }


Top.01 <- function(x,w) {
  
  y <- sum(x[x >= Hmisc::wtd.quantile(x,probs=0.9999,na.rm=T, weights = w)],na.rm = T) 
  
  z  <-  sum(x,na.rm = T)
  
  return(ifelse(z != 0, y/z, NA))
}


Top.1 <- function(x,w) {
  
  y <- sum(x[x >= Hmisc::wtd.quantile(x,probs=0.999,na.rm=T, weights = w)],na.rm = T) 
  
  z  <-  sum(x,na.rm = T)
  
  return(ifelse(z != 0, y/z, NA))
}

Top1 <- function(x,w) {
  
  y <- sum(x[x >= Hmisc::wtd.quantile(x,probs=0.99,na.rm=T, weights = w)],na.rm = T) 
  #y <- sum(x[x >= quantile(x, probs=0.99, na.rm=T)],na.rm = T) 
  
  z  <-  sum(x,na.rm = T)
  
  return(ifelse(z != 0, y/z, NA))
}

Top10 <- function(x,w) {
  
  y <- sum(x[x >= Hmisc::wtd.quantile(x,probs=0.90,na.rm=T, weights = w)],na.rm = T) 
  
  z  <-  sum(x,na.rm = T)
  
  return(ifelse(z != 0, y/z, NA))
}

Bottom50 <- function(x,w) {
  
  y <- sum(x[x <= Hmisc::wtd.quantile(x,probs=0.50,na.rm=T, weights = w)],na.rm = T) 
  
  z  <-  sum(x,na.rm = T)
  
  return(ifelse(z != 0, y/z, NA))
}


nine_ten <- function(x,w) {
  
  y <- Hmisc::wtd.quantile(x,probs=0.90,na.rm = T, weights = w) 
  z <- Hmisc::wtd.quantile(x,probs=0.10,na.rm = T, weights = w)
  
  return(ifelse(z != 0, y/z, NA))
}

five_ten <- function(x,w) {
  
  y <- Hmisc::wtd.quantile(x,probs=0.50,na.rm = T, weights = w) 
  z <- Hmisc::wtd.quantile(x,probs=0.10,na.rm = T, weights = w)
  
  return(ifelse(z != 0, y/z, NA))
  
}

nine_five <- function(x,w) {
  
  y <- Hmisc::wtd.quantile(x,probs=0.90,na.rm = T, weights = w)
  z <- Hmisc::wtd.quantile(x,probs=0.50,na.rm = T, weights = w)
  return(ifelse(z != 0, y/z, NA))
}

nine_five.fun <- function(d, i, j) {
  d <- d[i, ]
  w <- j[i, ]
  
  y <- Hmisc::wtd.quantile(d[["pred_wealth"]],probs=0.90,na.rm = T,weights=w[[wght]] %>% as.numeric)
  z <- Hmisc::wtd.quantile(d[["pred_wealth"]],probs=0.50,na.rm = T,weights=w[[wght]] %>% as.numeric)
  return(ifelse(z != 0, y/z, NA))
  
}


mean.fun <- function(dat, idx) mean(dat[idx], na.rm = TRUE)

sd.fun <- function(dat, idx) sd(dat[idx], na.rm = TRUE)


gini.fun <- function(d, i, j) {
  d <- d[i, ]
  w <- j[i, ]
  
  return(
    gini(d[["pred_wealth"]], 
              w[[wght]] %>% as.numeric)
    )   
}

gini_new.fun <- function(d, i, j) {
  d <- d[i, ]
  w <- j[i, ]
  
  return(
    gini(d[["pred_wealth_new"]], 
         w[[wght]] %>% as.numeric)
  )   
}

gini_new.fun <- function(d, i, j, var) {
  d <- d[i, ]
  w <- j[i, ]
  
  return(
    gini(d[[var]], 
         w[[wght]] %>% as.numeric)
  )   
}


gini_ihs.fun <- function(d, i, j) {
  d <- d[i, ]
  w <- j[i, ]
  
  return(
    gini(d[["pred_wealth_ihs"]], 
         w[[wght]] %>% as.numeric)
  )   
}

gini_net.fun <- function(d, i, j) {
  d <- d[i, ]
  w <- j[i, ]
  
  return(
    gini(d[["pred_wealth_net"]], 
         w[[wght]] %>% as.numeric)
  )   
}



boot.fun <- function(d, i, j, var, fct = gini) {
  d <- d[i, ]
  w <- j[i, ]
  
  return(
    fct(
      d[[var]], 
      w[[wght]] %>% as.numeric
      )
  )   
}

