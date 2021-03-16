#######################################################################
# potential species richness
#######################################################################

# scrope --------------------------------------------------------------
# take observed non-native richness away from predicted native richness

# library -------------------------------------------------------------
  library(raster)
  library(tidyverse)
  
  rm(list = ls())
  
# data ----------------------------------------------------------------
# NZ rasters
  setwd("Results/rasters/scaled/NZ")
  nz_list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", nz_list)
  nz_stack <- stack(nz_list)
  names(nz_stack) <- names
  list2env(setNames(unstack(nz_stack), names(nz_stack)), .GlobalEnv)
  nz_spp <- as.data.frame(nz_stack, na.rm = F)
  
  setwd("C:/Users/s436862/Dropbox/NZ-biogeography")
  
# potential relative richness ---------------------------------------------------    
# function
# requires: predicted raster = native predicted
#           observed raster  = non-native observed
#           raster.name      = identity for file location
  rr_pot <- function(pred.raster, obs.raster, save.name) {
    obs.raster[is.na(obs.raster[])] <- 0 
    rr_pot1 <- pred.raster - obs.raster
    rr_pot2 <- calc(rr_pot1, fun = function(x) {x[x<0] <- 0; return(x)})
    
    rasterfile <- paste0("Results/rasters/scaled/NZ/", save.name, ".grd")
    
    writeRaster(rr_pot2, rasterfile, overwrite = T)
    return(plot(rr_pot2))
    
  }
  
  
# NZ (pred nat) - NZ (non-nat)
  rr_pot(NZ_predicted_C3, NZ_nonnative_C3, "NZ_potential_C3")
  rr_pot(NZ_predicted_C4, NZ_nonnative_C4, "NZ_potential_C4")
  
# Aus (pred) - NZ (non-nat)
  rr_pot(Aus_predicted_C3, NZ_nonnative_C3, "Aus_potential_C3")
  rr_pot(Aus_predicted_C4, NZ_nonnative_C4, "Aus_potential_C4")

# -------------------------------------------------------------------