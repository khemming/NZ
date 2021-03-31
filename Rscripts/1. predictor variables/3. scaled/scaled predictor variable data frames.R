
# library ---------------------------------------------------------
  library(tidyverse)
  
  rm(list = ls())

# data files ------------------------------------------------------
  nz_pv <- read.csv("Data files/NZ/NZ predictor varaibles 288.csv") %>%
           mutate(country = "NZ")
  aus_pv <- read.csv("Data files/Australia/Australia predictor varaibles 2538.csv") %>%
            mutate(country = "AUS")
  
# combine and scale
  dat <- rbind(aus_pv, nz_pv)
  pv <- dat %>% 
        select(amt:ts) %>%
        scale(., center = T, scale = T)
  metadat <- dat %>% 
             select(-c(amt:ts, country))
# check
  pv2 <- data.frame(cbind(dat$country, pv)) %>%
         rename(country = V1)
  aus_pv_s <- pv2 %>% filter(country == "AUS")
  hist(as.numeric(aus_pv_s$amt))
  nz_pv_s <- pv2 %>% filter(country == "NZ")    
  hist(as.numeric(nz_pv_s$amt))
  
# save each countries scaled data
  pv3 <- cbind(metadat, pv2)

  aus_pv_s2 <- pv3 %>% filter(country == "AUS")
  nz_pv_s2 <- pv3 %>% filter(country == "NZ")
  
# double check  
  hist(as.numeric(nz_pv_s$amt))
  hist(as.numeric(nz_pv_s2$amt))

# save
  write.csv(aus_pv_s, "Results/csv/predictor variables/Australia predictor variables 2538.csv", row.names = F)
  write.csv(nz_pv_s, "Results/csv/predictor variables/NZ predictor variables 288.csv", row.names = F)

# -------------------------------------------------------------    