
# library -------------------------------------------------------------
  library(raster)
  library(tidyverse)
  
  rm(list = ls())

# data ----------------------------------------------------------------
# spp data
  setwd("C:/Users/s436862/Dropbox/NZ/Results/rasters/log")
  files_list <- list.files(pattern = ".grd")
  
# NZ
  nz_list <- Filter(function(x) grepl("NZ_", x), files_list)
  nz_names <- gsub(pattern = "\\.grd$", "", nz_list)
  nz_stack <- stack(nz_list)
  names(nz_stack) <- nz_names
  
# Aus
  aus_list <- Filter(function(x) grepl("Aus_", x), files_list)
  aus_names <- gsub(pattern = "\\.grd$", "", aus_list)
  aus_stack <- stack(aus_list)
  names(aus_stack) <- aus_names
 
  setwd("C:/Users/s436862/Dropbox/NZ")
  
# environmental data -----------------------------------------------------------
# NZ
  nz_pv <- read.csv("Results/csv/predictor variables/NZ predictor variables 288.csv")
  head(nz_pv)  
  
# land-only cell values
  nz_pv2 <- nz_pv %>% filter(cell_category == "land")
  write.csv(nz_pv2, "Results/csv/predictor variables/NZ predictor variables 83.csv", row.names = F)
  
# species - environmental variables data frame
  nz_spp_df <- as.data.frame(nz_stack)
  nz_spp_pv <- cbind(nz_spp_df, nz_pv)
  
  write.csv(nz_spp_pv, "Results/csv/predictor variables/NZ spp predictor variables 288.csv", row.names = F)

# land-only cell values
  nz_spp_pv3 <- nz_spp_pv %>% filter(cell_category == "land")
  head(nz_spp_pv3)
  write.csv(nz_spp_pv3, "Results/csv/predictor variables/NZ spp predictor variables 83.csv", row.names = F)
  
# Aus
  aus_pv <- read.csv("Results/csv/predictor variables/Australia predictor variables 2538.csv")
  head(aus_pv) 
  
  aus_pv2 <- aus_pv %>% filter(cell_category == "land")
  
  write.csv(aus_pv2, "Results/csv/predictor variables/Australia predictor variables 1141.csv", row.names = F)   
  
# species - environmental variables data frame
  aus_spp_df <- as.data.frame(aus_stack)
  aus_spp_pv <- cbind(aus_spp_df, aus_pv)
  
  write.csv(aus_spp_pv, "Results/csv/predictor variables/Australia spp predictor variables 2538.csv", row.names = F)
  
# land-only cell values
  aus_spp_pv2 <- aus_spp_pv %>% filter(cell_category == "land")
  head(aus_spp_pv2)
  write.csv(aus_spp_pv2, "Results/csv/predictor variables/Australia spp predictor variables 1141.csv", row.names = F)    

# number of cells with species richness estimates -----------------------------------
# by country, pathway and status 
  aus_nC3 <- length(na.omit(getValues(aus_stack$Aus_native_C3)))
  aus_nC4 <- length(na.omit(getValues(aus_stack$Aus_native_C4)))
  
  aus_nnC3 <- length(na.omit(getValues(aus_stack$Aus_nonnative_C3)))
  aus_nnC4 <- length(na.omit(getValues(aus_stack$Aus_nonnative_C4)))
  
  nz_nC3 <- length(na.omit(getValues(nz_stack$NZ_native_C3)))
  nz_nC4 <- length(na.omit(getValues(nz_stack$NZ_native_C4)))
  
  nz_nnC3 <- length(na.omit(getValues(nz_stack$NZ_nonnative_C3)))
  nz_nnC4 <- length(na.omit(getValues(nz_stack$NZ_nonnative_C4)))
  
# ----------------------------------------------------------------------      
  

  
  