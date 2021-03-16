

# library ------------------------------------------------------------------
  library(raster)
  library(tidyverse)
  library(broom)
  library(magrittr)
  
  rm(list = ls())
  
# data ----------------------------------------------------------------   
# model data
  load("Data files/rdata/Aus models.RData")
  aus_models <- model_list
  load("Data files/rdata/NZ models.RData")
  nz_models <- model_list
  
  spp_models <- c(aus_models, nz_models )

# NZ and predictor variables
  nz <- raster("Data files/NZ/NZ 83.grd")
  nz_pv <- read.csv("Results/csv/predictor variables/NZ predictor variables 83.csv")
  
# Aus and predictor variables
  aus <- raster("Data files/Australia/Australia 1141.grd")
  aus_pv <- read.csv("Results/csv/predictor variables/Australia predictor variables 1141.csv")
  
# data frame to predict to NZ (x) --------------------------------------------------
# cell ID
  cell_id <- nz_pv %>% dplyr::select(cell_id)
  
# predict NZ and Aus distributions across NZ 
  nz_pred_mat <- matrix(ncol = length(spp_models), 
                        nrow = nrow(cell_id))
  
  for (i in 1:ncol(nz_pred_mat)){
    nz_pred_mat[,i] <- predict(spp_models[[i]], newdata = nz_pv)
  }

# check
  head(nz_pred_mat)

# rasterise predicted distributions (pd) 
# turn 83 cells to 288 in correct order
  nz_pd <- data.frame(cbind(cell_id, nz_pred_mat))
  glimpse(nz_pd)  

# generate list of occupied cells
  oc_list <- nz_pd$cell_id
  
# make a matrix with all missing values
  x <- matrix(NA, nrow = length(getValues(nz)), ncol = length(spp_models))
  head(x)
  class(nz_pred_mat) <- "numeric"
  
# add the occupied cells
  x[oc_list, ] <- nz_pred_mat
  
# data frame to predict to Aus (y) --------------------------------------------------
# cell ID
  cell_id <- aus_pv %>% dplyr::select(cell_id)
  
# predict distributions 
  aus_pred_mat <- matrix(ncol = length(aus_models), 
                         nrow = nrow(cell_id))
  
  for (i in 1:ncol(aus_pred_mat)){
    aus_pred_mat[,i] <- predict(aus_models[[i]], newdata = aus_pv)
  }
  
# check
  head(aus_pred_mat)
  
# rasterise predicted distributions (pd) 
# turn 1141 cells to 2538 in correct order
  aus_pd <- data.frame(cbind(cell_id, aus_pred_mat))
  glimpse(aus_pd)  
  
# generate list of occupied cells
  oc_list <- aus_pd$cell_id
  
# make a matrix with all missing values
  y <- matrix(NA, nrow = length(getValues(aus)), ncol = length(aus_models))
  head(y)
  class(aus_pred_mat) <- "numeric"
  
# add the occupied cells
  y[oc_list, ] <- aus_pred_mat
  
# produce rasters ------------------------------------------
# function requires: raster column = from data frame ("x" or "y")
#                    name = useful name for file save 
#                    predicted_to = which country is being predicted on to
  raster_fun <- function(raster, df, column, name, predicted_to){
    
    r1 <- setValues(raster, df[, column])
    save <- paste0("Results/rasters/predicted/", name, predicted_to)
    
    writeRaster(r1, save, overwrite = T)
    return(plot(r1))
    
  }
  
# predicted to NZ
  raster <- nz
  df <- x
  colname <- names(spp_models)
  predicted_to <- "_predicted.grd"
  
  for (i in 1:ncol(x)){
    
    raster_fun(raster, df, i, colname[i], predicted_to)
    
  }
  
# predicted to Australia
  raster <- aus
  df <- y
  colname <- names(aus_models)
  predicted_to <- "_predicted_to_Aus.grd"
  
  for (i in 1:ncol(y)){
    
    raster_fun(raster, df, i, colname[i], predicted_to)
    
  }
  
# ---------------------------------------------------------------------------------  
  

  
  
  
