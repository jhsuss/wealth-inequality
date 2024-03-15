###### helper functions to compute different inequality measures 

Top.001 <- function(x,w=NULL) {
  
  if (is.null(w)) {
    
    y <- sum(x[x >= quantile(x,probs=0.99999,na.rm=T)],na.rm = T)
    z <- sum(x,na.rm = T)
    
  } else {
    df <- tibble(x,w)
    
    y <- df %>% 
      filter(x >= Hmisc::wtd.quantile(x,probs=0.99999,na.rm=T, weights = w)) 
    
    # in case the top x percentile is the max obs, filter fails 
    if (nrow(y) == 0) {
      y <- df %>% 
        arrange(desc(x)) %>% 
        slice(1) %>%  
        summarise(
          x = sum(x*w)
        ) %>% 
        .$x
    } else {
      y <- y %>% 
        summarise(
          x = sum(x*w)
        ) %>% 
        .$x
    }
    
    z <- sum(x*w,na.rm = T)
  }
  
  return(ifelse(z != 0, y/z, NA))
}

Top.01 <- function(x,w=NULL) {
  
  if (is.null(w)) {
    
    y <- sum(x[x >= quantile(x,probs=0.9999,na.rm=T)],na.rm = T)
    z <- sum(x,na.rm = T)
    
  } else {
    df <- tibble(x,w)
    
    y <- df %>% 
      filter(x >= Hmisc::wtd.quantile(x,probs=0.9999,na.rm=T, weights = w)) 
    
    # in case the top x percentile is the max obs, filter fails 
    if (nrow(y) == 0) {
      y <- df %>% 
        arrange(desc(x)) %>% 
        slice(1) %>%  
        summarise(
          x = sum(x*w)
        ) %>% 
        .$x
    } else {
      y <- y %>% 
        summarise(
          x = sum(x*w)
        ) %>% 
        .$x
    }
      
    z <- sum(x*w,na.rm = T)
  }
  
  return(ifelse(z != 0, y/z, NA))
}


Top.1 <- function(x,w=NULL) {
  
  if (is.null(w)) {
    
    y <- sum(x[x >= quantile(x,probs=0.999,na.rm=T)],na.rm = T)
    z <- sum(x,na.rm = T)
    
  } else {
    df <- tibble(x,w)
    
    y <- df %>% 
      filter(x >= Hmisc::wtd.quantile(x,probs=0.999,na.rm=T, weights = w)) 
    
    # in case the top x percentile is the max obs, filter fails 
    if (nrow(y) == 0) {
      y <- df %>% 
        arrange(desc(x)) %>% 
        slice(1) %>%  
        summarise(
          x = sum(x*w)
        ) %>% 
        .$x
    } else {
      y <- y %>% 
        summarise(
          x = sum(x*w)
        ) %>% 
        .$x
    }
    
    z <- sum(x*w,na.rm = T)
  }
  
  
  return(ifelse(z != 0, y/z, NA))
}

Top1 <- function(x,w = NULL) {
  
  if (is.null(w)) {
    
    y <- sum(x[x >= quantile(x,probs=0.99,na.rm=T)],na.rm = T)
    z <- sum(x,na.rm = T)
    
  } else {
    df <- tibble(x,w)
    
    y <- df %>% 
      filter(x >= Hmisc::wtd.quantile(x,probs=0.99,na.rm=T, weights = w)) 
    
    # in case the top x percentile is the max obs, filter fails 
    if (nrow(y) == 0) {
      y <- df %>% 
        arrange(desc(x)) %>% 
        slice(1) %>%  
        summarise(
          x = sum(x*w)
        ) %>% 
        .$x
    } else {
      y <- y %>% 
        summarise(
          x = sum(x*w)
        ) %>% 
        .$x
    }
    
    z <- sum(x*w,na.rm = T)
  }
  
  return(ifelse(z != 0, y/z, NA))
}

Top10 <- function(x,w=NULL) {
  
  if (is.null(w)) {
    
    y <- sum(x[x >= quantile(x,probs=0.90,na.rm=T)],na.rm = T)
    z <- sum(x,na.rm = T)
    
  } else {
    df <- tibble(x,w)
    
    y <- df %>% 
      filter(x >= Hmisc::wtd.quantile(x,probs=0.90,na.rm=T, weights = w)) 
    
    # in case the top x percentile is the max obs, filter fails 
    if (nrow(y) == 0) {
      y <- df %>% 
        arrange(desc(x)) %>% 
        slice(1) %>%  
        summarise(
          x = sum(x*w)
        ) %>% 
        .$x
    } else {
      y <- y %>% 
        summarise(
          x = sum(x*w)
        ) %>% 
        .$x
    }
    
    z <- sum(x*w,na.rm = T)
  }
  return(ifelse(z != 0, y/z, NA))
}

Bottom50 <- function(x,w=NULL) {
  
  if (is.null(w)) {
    
    y <- sum(x[x <= quantile(x,probs=0.50,na.rm=T)],na.rm = T)
    z <- sum(x,na.rm = T)
    
  } else {
    df <- tibble(x,w)
    
    y <- df %>% 
      filter(x <= Hmisc::wtd.quantile(x,probs=0.50,na.rm=T, weights = w)) %>% 
      summarise(
        x = sum(x*w)
      ) %>% 
      .$x
    
    z <- sum(x*w,na.rm = T)
  }
  
  return(ifelse(z != 0, y/z, NA))
}


Top.5 <- function(x,w=NULL) {
  
  if (is.null(w)) {
    
    y <- sum(x[x >= quantile(x,probs=0.995,na.rm=T)],na.rm = T)
    z <- sum(x,na.rm = T)
    
  } else {
    df <- tibble(x,w)
    
    y <- df %>% 
      filter(x >= Hmisc::wtd.quantile(x,probs=0.995,na.rm=T, weights = w)) 
    
    # in case the top x percentile is the max obs, filter fails 
    if (nrow(y) == 0) {
      y <- df %>% 
        arrange(desc(x)) %>% 
        slice(1) %>%  
        summarise(
          x = sum(x*w)
        ) %>% 
        .$x
    } else {
      y <- y %>% 
        summarise(
          x = sum(x*w)
        ) %>% 
        .$x
    }
    
    z <- sum(x*w,na.rm = T)
  }
  
  
  return(ifelse(z != 0, y/z, NA))
}

Top5 <- function(x,w = NULL) {
  
  if (is.null(w)) {
    
    y <- sum(x[x >= quantile(x,probs=0.95,na.rm=T)],na.rm = T)
    z <- sum(x,na.rm = T)
    
  } else {
    df <- tibble(x,w)
    
    y <- df %>% 
      filter(x >= Hmisc::wtd.quantile(x,probs=0.95,na.rm=T, weights = w)) 
    
    # in case the top x percentile is the max obs, filter fails 
    if (nrow(y) == 0) {
      y <- df %>% 
        arrange(desc(x)) %>% 
        slice(1) %>%  
        summarise(
          x = sum(x*w)
        ) %>% 
        .$x
    } else {
      y <- y %>% 
        summarise(
          x = sum(x*w)
        ) %>% 
        .$x
    }
    
    z <- sum(x*w,na.rm = T)
  }
  
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


#### functions to work with boot()

mean.fun <- function(dat, idx) mean(dat[idx], na.rm = TRUE)

sd.fun <- function(dat, idx) sd(dat[idx], na.rm = TRUE)


gini.fun <- function(d, i, j, var) {
  d <- d[i, ]
  w <- j[i, ]
  
  return(
    gini(d[[var]], 
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

