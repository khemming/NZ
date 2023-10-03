

# library -------------------------------------------------------------
  library(raster)
  library(tidyverse)

  rm(list = ls())

# data ----------------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/NZ/Results/rasters/iNEXT")
  files_list <- list.files(pattern = ".grd")
  
# NZ
  nz_list <- Filter(function(x) grepl("NZ_", x), files_list)
  nz_names <- gsub(pattern = "\\.grd$", "", nz_list)
  nz_stack <- stack(nz_list)
  names(nz_stack) <- nz_names
  list2env(setNames(unstack(nz_stack), names(nz_stack)), .GlobalEnv)
  
# Aus
  aus_list <- Filter(function(x) grepl("Aus_", x), files_list)
  aus_names <- gsub(pattern = "\\.grd$", "", aus_list)
  aus_stack <- stack(aus_list)
  names(aus_stack) <- aus_names
  list2env(setNames(unstack(aus_stack), names(aus_stack)), .GlobalEnv)
  
  setwd("C:/Users/s436862/Dropbox/NZ")

# log richness -----------------------------------------------    
# requires: raster =  loaded from above
#           folder = country file extension "log and scaled to NZ/" or "log and scaled to Aus/"
#           raster.name = file location, observed or predicted
  prop_sr <- function(raster, raster.name) {
    
    log_t <- log(raster)
   
    rasterfile <- paste0("Results/rasters/log/", raster.name, ".grd")
    
    writeRaster(log_t, rasterfile, overwrite = T)
    return(plot(log_t))
    
  }
  
# in New Zealand   
# native
  prop_sr(NZ_native_C3, "NZ_native_C3")
  prop_sr(NZ_native_C4, "NZ_native_C4")
  
# nonnative  
  prop_sr(NZ_nonnative_C3, "NZ_nonnative_C3")
  prop_sr(NZ_nonnative_C4, "NZ_nonnative_C4")
  
# in Australia 
# native
  prop_sr(Aus_native_C3, "Aus_native_C3")
  prop_sr(Aus_native_C4, "Aus_native_C4")
  
# nonnative 
  prop_sr(Aus_nonnative_C3, "Aus_nonnative_C3")
  prop_sr(Aus_nonnative_C4, "Aus_nonnative_C4")
  
# ----------------------------------------------------
  
  