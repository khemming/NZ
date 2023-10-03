
####################################################################
# modelling NZ & Aus native/non-native C3 C4 patterns to respective predictors
####################################################################

# library ------------------------------------------------------------------
  library(tidyverse)
  library(broom)
  library(magrittr)
  library(raster)
  
  rm(list = ls())

# data ---------------------------------------------------------------------   
# NZ rasters
  setwd("Results/rasters/scaled/NZ")
  nz_list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", nz_list)
  nz_stack <- stack(nz_list)
  names(nz_stack) <- names
  list2env(setNames(unstack(nz_stack), names(nz_stack)), .GlobalEnv)
  nz_spp <- as.data.frame(nz_stack, na.rm = F)
  
# Aus rasters
  setwd("C:/Users/s436862/Dropbox/NZ-biogeography/Results/rasters/scaled/Australia")
  
  aus_list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", aus_list)
  aus_stack <- stack(aus_list)
  names(aus_stack) <- names
  list2env(setNames(unstack(aus_stack), names(aus_stack)), .GlobalEnv)
  aus_spp <- as.data.frame(aus_stack, na.rm = F)
  
  setwd("C:/Users/s436862/Dropbox/NZ-biogeography")
  
# predictor variables
  nz_pv <- read.csv("Results/csv/predictor variables/NZ predictor variables scaled.csv", header = T) 
  glimpse(nz_pv)  
  aus_pv <- read.csv("Results/csv/predictor variables/Aus predictor variables scaled.csv", header = T) 
  glimpse(aus_pv)  
  
# bind spp and varaibles and subset to land cells 
  nz_dat <- bind_cols(nz_spp, nz_pv) %>%
            filter(cell_category == "land")
  glimpse(nz_dat)  
  
  aus_dat <- bind_cols(aus_spp, aus_pv) %>%
    filter(cell_category == "land")
  glimpse(aus_dat)  

# models --------------------------------------------------------------
# NZ native
  m_nz_nc3 <- lm(NZ_native_C3 ~ hii + th + pcoldq + pwarmq + ts + arid + amt + prop_cover, data = nz_dat)
  summary(m_nz_nc3)
  m_nz_nc4 <- lm(NZ_native_C4 ~ hii + th + pcoldq + pwarmq + ts + arid + amt + prop_cover, data = nz_dat)
  summary(m_nz_nc4)
# non-native
  m_nz_nnc3 <- lm(NZ_nonnative_C3 ~ hii + th + pcoldq + pwarmq + ts + arid + amt + prop_cover, data = nz_dat)
  summary(m_nz_nnc3)
  m_nz_nnc4 <- lm(NZ_nonnative_C4 ~ hii + th + pcoldq + pwarmq + ts + arid + amt + prop_cover, data = nz_dat) # one cell with non-native C4 richness
  summary(m_nz_nnc4)
  
# Aus native 
  m_aus_nc3 <- lm(Aus_native_C3 ~ hii + th + pcoldq + pwarmq + ts + arid + amt + prop_cover, data = aus_dat)
  summary(m_aus_nc3)
  m_aus_nc4 <- lm(Aus_native_C4 ~ hii + th + pcoldq + pwarmq + ts + arid + amt + prop_cover, data = aus_dat)
  summary(m_aus_nc4)

# save workspace to use for plotting
  save.image("Data files/rdata/model_data.RData") 
  
# check ------------------------------------------------------
# plotting residuals of models
  plot(m_nz_nc3$residuals)
  plot(m_nz_nc4$residuals)
  plot(m_nz_nnc3$residuals)
  plot(m_nz_nnc4$residuals)
  
  plot(m_aus_nc3$residuals)
  plot(m_aus_nc4$residuals)
  
# ----------------------------------------------------------------------------
  
  
