
########################################################################################
# step three: 100 km scale data frame of EVs
########################################################################################

# scope --------------------------------------------------
# 100 km dataframe of all of EVs, centred and scaled

# library ---------------------------------------------------------
  library(raster)
  library(dplyr)
  library(rgdal)
  library(purrr)
  
  rm(list = ls())
  
# data ---------------------------------------------------  
# cell id, category, and proportion cover
  cells <- read.csv("Data files/New Zealand/nz 100-km.csv")
  head(cells)
# rasters 
  setwd("C:/Users/s436862/Dropbox/NZ-biogeography/Data files/EVs/step 2 - 100-km")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)

  setwd("C:/Users/s436862/Dropbox/NZ-biogeography")
    
# dataframe -------------------------------------------------------------------
  ef.v <- getValues(c.stack)
  ef.df <- as.data.frame(ef.v)

# scale and nomralise 
  efs_norm <- scale(ef.df, center = T, scale = T)
  efs_norm <- data.frame(efs_norm)
  
  efs_scaled <- cbind(cells, efs_norm)  
  head(efs_scaled)

  write.csv(efs_scaled, file = "C:/Users/s436862/Dropbox/NZ-biogeography/Data files/EVs/step 3 - CSV/100-km scaled.csv", row.names = F)
  
  
# --------------------------------------------------------------------------  
  
  
  
  
  
  