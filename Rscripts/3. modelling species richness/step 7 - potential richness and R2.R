


# library -------------------------------------------------------------
  library(raster)
  library(tidyverse)

  rm(list = ls())

# data ----------------------------------------------------------------
# observed richness
  setwd("C:/Users/s436862/Dropbox/NZ/Results/rasters/log")
  files_ls <- list.files(pattern = ".grd")
  
  nz_ls <- Filter(function(x) grepl("NZ_", x), files_ls)
  nz_names <- gsub(pattern = "\\.grd$", "", nz_ls)
  nz_stack <- stack(nz_ls)
  names(nz_stack) <- nz_names
  list2env(setNames(unstack(nz_stack), names(nz_stack)), .GlobalEnv)
  
  aus_ls <- Filter(function(x) grepl("Aus_", x), files_ls)
  aus_names <- gsub(pattern = "\\.grd$", "", aus_ls)
  aus_stack <- stack(aus_ls)
  names(aus_stack) <- aus_names
  list2env(setNames(unstack(aus_stack), names(aus_stack)), .GlobalEnv)
 
# predicted richness to NZ
  setwd("C:/Users/s436862/Dropbox/NZ/Results/rasters/predicted")
  files_ls <- list.files(pattern = "predicted.grd")
  
  nz_ls <- Filter(function(x) grepl("NZ_", x), files_ls)
  nz_names <- gsub(pattern = "\\.grd$", "", nz_ls)
  nz_stack <- stack(nz_ls)
  names(nz_stack) <- nz_names
  list2env(setNames(unstack(nz_stack), names(nz_stack)), .GlobalEnv)
  
  aus_ls <- Filter(function(x) grepl("Aus_", x), files_ls)
  aus_names <- gsub(pattern = "\\.grd$", "", aus_ls)
  aus_stack <- stack(aus_ls)
  names(aus_stack) <- aus_names
  list2env(setNames(unstack(aus_stack), names(aus_stack)), .GlobalEnv)
  
# predicted richness to Aus
  setwd("C:/Users/s436862/Dropbox/NZ/Results/rasters/predicted")
  files_ls <- list.files(pattern = "to_Aus.grd")
  aus_names2 <- gsub(pattern = "\\.grd$", "", files_ls)
  pr2aus_stack <- stack(files_ls)
  names(pr2aus_stack) <- aus_names2
  list2env(setNames(unstack(pr2aus_stack), names(pr2aus_stack)), .GlobalEnv)
  
  setwd("C:/Users/s436862/Dropbox/NZ")

# correlation exploration ----------------------------------------------------------    
  cor(getValues(NZ_native_C3_predicted), 
      getValues(Aus_native_C3_predicted), 
      method = "pearson", use = "complete.obs") # 0.74
  
  cor(getValues(NZ_native_C3_predicted), 
      getValues(NZ_nonnative_C3), 
      method = "pearson", use = "complete.obs") # -0.38
  
  cor(getValues(Aus_native_C3_predicted), 
      getValues(NZ_nonnative_C3), 
      method = "pearson", use = "complete.obs") # -0.45
  
  cor(getValues(NZ_nonnative_C3_predicted), 
      getValues(NZ_nonnative_C3), 
      method = "pearson", use = "complete.obs") # 0.73
  
  cor(getValues(NZ_nonnative_C4_predicted), 
      getValues(NZ_nonnative_C4), 
      method = "pearson", use = "complete.obs") # 0.56
  
  cor(getValues(Aus_native_C3_predicted_to_Aus), 
      getValues(Aus_native_C3), 
      method = "pearson", use = "complete.obs") # 0.79
  
  cor(getValues(Aus_native_C4_predicted_to_Aus), 
      getValues(Aus_native_C4), 
      method = "pearson", use = "complete.obs") # 0.82
  
# potential richness scaled to between 0 and 1 --------------------------------------    
# function
# requires: predicted raster  = native predicted
#           observed raster  = non-native observed
#           raster.name = identity of file location, in quotes
  rr_pot <- function(pred_raster, obs_raster, save_name) {
    obs_raster[is.na(obs_raster[])] <- 0 
    obs_raster2 <- obs_raster/cellStats(obs_raster, stat = "max", na.rm = T)
    pred_raster2 <- pred_raster/cellStats(pred_raster, stat = "max", na.rm = T)
    rr_pot1 <- pred_raster2 - obs_raster2
    rr_pot2 <- calc(rr_pot1, fun = function(x) {x[x<0] <- 0; return(x)})
    
    rasterfile <- paste0("Results/rasters/potential/", save_name, ".grd")
    
    writeRaster(rr_pot2, rasterfile, overwrite = T)
    return(plot(rr_pot2))
    
  }
  
  
# potential nonnative using
# NZ native C3   
  rr_pot(NZ_native_C3_predicted, NZ_nonnative_C3, "NZ_N_nonnative_C3_potential")
# nonnative C3  
  rr_pot(NZ_nonnative_C3_predicted, NZ_nonnative_C3, "NZ_NN_nonnative_C3_potential")
# nonnative C4
  rr_pot(NZ_nonnative_C4_predicted, NZ_nonnative_C4, "nonnative_C4_potential")
  
# Aus native C3 
  rr_pot(Aus_native_C3_predicted, NZ_nonnative_C3, "Aus_nonnative_C3_potential")
# Aus native C4 
  rr_pot(Aus_native_C4_predicted, NZ_nonnative_C4, "Aus_nonnative_C4_potential")
  
# r2 ----------------------------------------------------------------------
  adj_r2 <- function(obs_raster, pred_raster){
    y <- getValues(obs_raster)
    yhat <- getValues(pred_raster)
    r_squared = 1 - sum((y - yhat)^2, na.rm = T) / sum((y - mean(y, na.rm = T))^2, na.rm = T)
    r_squared
    
  # adjusted r2
    n <- length(na.omit(y))
    p <- 9
    adj_r_squared = 1 - (1 - r_squared) * ((n - 1)/(n - p - 1))
    return(round(adj_r_squared, 3)) 
  } # ajd_r2 end
  
# run 
  r2 <- data.frame(model = c("C3_NZ_native",
                             "C3_NZ_nonnative",
                             "C4_NZ_nonnative",
                             
                             "C3_Aus_native",
                             "C3_Aus_nonnative",
                             
                             "C4_Aus_native",
                             "C4_Aus_nonnative",
                             
                             "C3_NZ_native_x_Aus_native",
                             "C3_NZ_nonnative_x_NZ_native",
                             "C3_NZ_nonnative_x_Aus_native",
                             "C3_NZ_nonnative_x_Aus_nonnative",
                             
                             "C4_NZ_nonnative_x_Aus_native",
                             "C4_NZ_nonnative_x_Aus_nonnative"),
                   adj_r2 = NA)
  
  r2$adj_r2[1] <- adj_r2(NZ_native_C3, NZ_native_C3_predicted)
  r2$adj_r2[2] <- adj_r2(NZ_nonnative_C3, NZ_nonnative_C3_predicted)
  r2$adj_r2[3] <- adj_r2(NZ_nonnative_C4, NZ_nonnative_C4_predicted)
  
  r2$adj_r2[4] <- adj_r2(Aus_native_C3, Aus_native_C3_predicted_to_Aus)
  r2$adj_r2[5] <- adj_r2(Aus_nonnative_C3, Aus_nonnative_C3_predicted_to_Aus)
  
  r2$adj_r2[6] <- adj_r2(Aus_native_C4, Aus_native_C4_predicted_to_Aus)
  r2$adj_r2[7] <- adj_r2(Aus_nonnative_C4, Aus_nonnative_C4_predicted_to_Aus)
  
  r2$adj_r2[8] <- adj_r2(NZ_native_C3, Aus_native_C3_predicted)
  r2$adj_r2[9] <- adj_r2(NZ_nonnative_C3, NZ_native_C3_predicted)
  r2$adj_r2[10] <- adj_r2(NZ_nonnative_C3, Aus_native_C3_predicted)  
  r2$adj_r2[11] <- adj_r2(NZ_nonnative_C3, Aus_nonnative_C3_predicted) 
  
  r2$adj_r2[12] <- adj_r2(NZ_nonnative_C4, Aus_native_C4_predicted)  
  r2$adj_r2[13] <- adj_r2(NZ_nonnative_C4, Aus_nonnative_C4_predicted) 
  
  r2 
  
# checks
  plot(getValues(NZ_native_C3) ~ getValues(NZ_native_C3_predicted))          # good agreement
  plot(getValues(NZ_nonnative_C3) ~ getValues(NZ_native_C3))                 # explains poor R2
  plot(getValues(NZ_nonnative_C3) ~ getValues(NZ_nonnative_C3_predicted))    # good
  
  plot(getValues(Aus_native_C3) ~ getValues(Aus_native_C3_predicted_to_Aus)) # good agreement
  
# save  
  write.csv(r2, "Results/csv/models/R2.csv", row.names = F)  
  
# -------------------------------------------------------------------
  
  
  