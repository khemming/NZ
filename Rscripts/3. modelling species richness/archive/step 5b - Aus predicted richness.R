

# library ------------------------------------------------------------------
  library(raster)
  library(tidyverse)
  library(broom)
  library(magrittr)
  
  
  rm(list = ls())
  
# NZ -----------------------------------------------------------------------   
# model data
  load("Data files/rdata/NZ models.RData")
  
# raster
  nz <- raster("Data files/Australia/NZ 83.grd")
  
# predictor variables
  pv <- read.csv("Results/csv/predictor variables/NZ predictor variables 83.csv")
  
# cell ID
  cell_id <- pv %>% dplyr::select(cell_id)
  
# no. of spp
  spp <- 3
  
# Australia predicted distributions
# two distributions
  pred_mat <- matrix(ncol = spp, 
                     nrow = nrow(cell_id))
  
  for (i in 1:ncol(pred_mat)){
    pred_mat[,i] <- predict(model_list[[i]], newdata = pv)
  }
  
# check
  head(pred_mat[, 2])
  
# rasterise predicted distributions (pd) 
# turn 1003 cells to 2538 in correct order
  pd <- data.frame(cbind(cell_id, pred_mat))
  glimpse(pd)  
  
# generate list of occupied cells
  oc_list <- pd$cell_id
  
# make a matrix with all missing values
  x <- matrix(NA, nrow = length(getValues(aus)), ncol = spp)
  class(pred_mat) <- "numeric"
  
# add the occupied cells
  x[oc_list, ] <- pred_mat
  
# produce rasters 
# function requires: raster column = from data frame 'x'
#                    file name = useful name for file save 
  raster_fun <- function(raster_col, file_name){
    
    r1 <- setValues(aus, x[, raster_col])
    r2 <- calc(r1, fun = function(x) {x[x<0] <- 0; return(x)})
    r3 <- calc(r2, fun = function(x) {x[x>1] <- 1; return(x)})
    
    save <- paste0("Results/rasters/predicted/", file_name, ".grd")
    
    writeRaster(r3, save, overwrite = T)
    return(plot(r3))
    
  }
  
  colnames(x) <- c("Aus_native_C3_predicted", 
                   "Aus_native_C4_predicted")
  
  for (i in 1:ncol(x)){
    
    raster_fun(i, colnames(x)[i])
    
  }
  
# Aus -----------------------------------------------------------------------   
  rm(list = ls())
  
# model data
  load("Data files/rdata/Aus models.RData")

# raster
  aus <- raster("Data files/Australia/Australia 1141.grd")
  
# predictor variables
  pv <- read.csv("Results/csv/predictor variables/Australia predictor variables 1141.csv")
  
# cell ID
  cell_id <- pv %>% dplyr::select(cell_id)

# no. of spp
  spp <- 2
  
# Australia predicted distributions
# two distributions
    pred_mat <- matrix(ncol = spp, 
                     nrow = nrow(cell_id))
  
  for (i in 1:ncol(pred_mat)){
      pred_mat[,i] <- predict(model_list[[i]], newdata = pv)
  }

# check
  head(pred_mat[, 2])

# rasterise predicted distributions (pd) 
# turn 1003 cells to 2538 in correct order
  pd <- data.frame(cbind(cell_id, pred_mat))
  glimpse(pd)  

# generate list of occupied cells
  oc_list <- pd$cell_id
  
# make a matrix with all missing values
  x <- matrix(NA, nrow = length(getValues(aus)), ncol = spp)
  class(pred_mat) <- "numeric"
  
# add the occupied cells
  x[oc_list, ] <- pred_mat
  
# produce rasters 
# function requires: raster column = from data frame 'x'
#                    file name = useful name for file save 
  raster_fun <- function(raster_col, file_name){
    
    r1 <- setValues(aus, x[, raster_col])
    r2 <- calc(r1, fun = function(x) {x[x<0] <- 0; return(x)})
    r3 <- calc(r2, fun = function(x) {x[x>1] <- 1; return(x)})
    
    save <- paste0("Results/rasters/predicted/", file_name, ".grd")
    
    writeRaster(r3, save, overwrite = T)
    return(plot(r3))
    
  }
  
  colnames(x) <- c("Aus_native_C3_predicted", 
                   "Aus_native_C4_predicted")
  
  for (i in 1:ncol(x)){
    
    raster_fun(i, colnames(x)[i])
    
  }
  
# ---------------------------------------------------------------------------------  
  

  
  
  
