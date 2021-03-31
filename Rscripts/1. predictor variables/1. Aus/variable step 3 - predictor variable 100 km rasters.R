

# library -----------------------------------------------------------------------------
  library(raster)
  
  rm(list = ls())
  
# data ---------------------------------------------------------------------------------  
  setwd("Data files/predictor variables/Australia 1 km")
  
  files <- list.files(pattern = ".grd")
  names <- gsub(pattern = ".grd", "", files)
  stack <- stack(files) # ignore error message
  list2env(setNames(unstack(stack), names), .GlobalEnv)
  
  setwd("C:/Users/s436862/Dropbox/NZ")
  
  
# scale resolution from 1 km to 100 km -------------------------------------
# mean
  amt_ag <- aggregate(amt, fact = 100, fun = mean)
  writeRaster(amt_ag, "Results/rasters/predictor variables/Australia 100 km/amt", overwrite = T)
  
  arid_ag <- aggregate(arid, fact = 100, fun = mean)
  writeRaster(arid_ag, "Results/rasters/predictor variables/Australia 100 km/arid", overwrite = T)
  
  pcoldq_ag <- aggregate(pcoldq, fact = 100, fun = mean)
  writeRaster(pcoldq_ag, "Results/rasters/predictor variables/Australia 100 km/pcoldq", overwrite = T)
  
  pwarmq_ag <- aggregate(pwarmq, fact = 100, fun = mean)
  writeRaster(pwarmq_ag, "Results/rasters/predictor variables/Australia 100 km/pwarmq", overwrite = T)
  
  ts_ag <- aggregate(ts, fact = 100, fun = mean)
  writeRaster(ts_ag, "Results/rasters/predictor variables/Australia 100 km/ts", overwrite = T)
  
  hii_ag <- aggregate(hii, fact = 100, fun = mean)
  writeRaster(hii_ag, "Results/rasters/predictor variables/Australia 100 km/hii", overwrite = T)
  
# topographic heterogeneity (sd)
  th <- aggregate(elev, fact = 100, fun = sd)
  writeRaster(th, "Results/rasters/predictor variables/Australia 100 km/th", overwrite = T)
  
# ----------------------------------------------------------------------------