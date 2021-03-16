
# scope -------------------------------------------
# statisticss:
# the number of records
# number of species
# cropped using Australia raster
# for:
# C3/C4/total
# native/nonnative

# library -----------------------------------------
  library(tidyverse)
  library(raster)
  library(rgdal)

  rm(list = ls())

# data --------------------------------------------
  poa <- readRDS("Data files/ALA/ALA master data/master grass records.rds") %>%
    dplyr::select(species, latitude, longitude, status, pp)
  
  aus <- raster("Data files/Australia/Australia 100 km.grd")

# crop records ------------------------------------
  xy <- cbind(poa$longitude, poa$latitude)
  cell <- raster::extract(aus, xy)
  
  table(cell, exclude = NULL)
    
  poa$aus_record <- ifelse(!is.na(cell), "yes", "no")
  table(poa$aus_record, exclude = NULL)

  poa_cell <- poa %>% filter(aus_record == "yes")      

# record and species summaries by origin -----------
# first, by records
  poa_rec <- poa_cell %>% group_by(status, pp) %>%
              summarise(records = n())
  
    
# --------------------------------------------------    
    
  
  
  
  