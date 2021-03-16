
# date created: 16/4/18
# last modified: 19/9/18

# This is 100-km rasters and a dataframe of all of them


# Library ---------------------------------------------------------
  library(raster)
  library(dplyr)
  library(rgdal)
  library(purrr)
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")

# Data ---------------------------------------------------  
  arid <- raster("EFs/Rasters 100-km/arid.grd")
  rz <- raster("EFs/Rasters 100-km/rz.grd")
  sp <- raster("EFs/Rasters 100-km/sp.grd")
  st <- raster("EFs/Rasters 100-km/st.grd")
  elev <- raster("EFs/Rasters 100-km/elev.grd")
  pet <- raster("EFs/Rasters 100-km/pet.grd")
  pawc <- raster("EFs/Rasters 100-km/pawc.grd")
  pewc <- raster("EFs/Rasters 100-km/pewc.grd")
  amt <- raster("EFs/Rasters 100-km/amt.grd")
  mdr <- raster("EFs/Rasters 100-km/mdr.grd")
  iso <- raster("EFs/Rasters 100-km/iso.grd")
  ts <- raster("EFs/Rasters 100-km/ts.grd")
  twarmm <- raster("EFs/Rasters 100-km/twarmm.grd")
  tcoldm <- raster("EFs/Rasters 100-km/tcoldm.grd")
  tar <- raster("EFs/Rasters 100-km/tar.grd")
  hii <- raster("EFs/Rasters 100-km/hii.grd")
  twetq <- raster("EFs/Rasters 100-km/twetq.grd")
  tdryq <- raster("EFs/Rasters 100-km/tdryq.grd")
  twarmq <- raster("EFs/Rasters 100-km/twarmq.grd")
  tcoldq <- raster("EFs/Rasters 100-km/tcoldq.grd")
  ap <- raster("EFs/Rasters 100-km/ap.grd")
  pwetm <- raster("EFs/Rasters 100-km/pwetm.grd")
  pdrym <- raster("EFs/Rasters 100-km/pdrym.grd")
  ps <- raster("EFs/Rasters 100-km/ps.grd")
  pwetq <- raster("EFs/Rasters 100-km/pwetq.grd")
  pdryq <- raster("EFs/Rasters 100-km/pdryq.grd")
  pwarmq <- raster("EFs/Rasters 100-km/pwarmq.grd")
  pcoldq <- raster("EFs/Rasters 100-km/pcoldq.grd")
  th <- raster("EFs/Rasters 100-km/th.grd")
  
# shapefile
  aus.shp <- readOGR("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/Australia/australia_shapefile.shp")
  
# Cell identification
  cell_id <- raster("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/Australia/aus_100km_cell_id")
  
# combine & name  
  ef <- stack(cell_id,    hii,  arid,     rz,     th,
                   sp,     st,  elev,    pet,   pawc, 
                 pewc,    amt,   mdr,    iso,     ts, 
               twarmm, tcoldm,   tar,  twetq,  tdryq, 
               twarmq, tcoldq,    ap,  pwetm,  pdrym, 
                   ps,  pwetq, pdryq, pwarmq, pcoldq) 
      
  names(ef) <- c("cell.id",    "hii",   "arid",     "rz",    "th",
                      "sp",     "st",  "elev",   "pet",   "pawc", 
                    "pewc",    "amt",   "mdr",    "iso",     "ts", 
                  "twarmm", "tcoldm",   "tar",  "twetq",  "tdryq", 
                  "twarmq", "tcoldq",    "ap",  "pwetm",  "pdrym", 
                      "ps",  "pwetq", "pdryq", "pwarmq", "pcoldq")
               
      
# Dataframe --------------------------------------------------------------------
# Add in terrestrial cell code
# this means that whenever I select for "terrestrial", it'll just be those 1003 cells. All other cells will be 'Ocean'
# How to do?   
  ef.v <- getValues(ef)
  ef.df <- as.data.frame(ef.v)

  ef.df.c <- mutate(ef.df, cell.cat = case_when(!is.na(pdryq) ~ "terrestrial",
                                                         TRUE ~ "ocean"))
  
  sum(is.na(ef.df.c$cell.cat))
  
# reorder so cell.cat is first column
  ef.df.reo <- ef.df.c[, c(31, 1:30)]

# save raw efs
  write.csv(ef.df.reo, file = "C:/Users/s436862/Dropbox/Climate Matching/1. Data files/EFs/CSV/100-km raw.csv", row.names = F)
  

# scale and nomralise 
  efs_scale <- dplyr::select(ef.df.reo, -cell.cat, -cell.id)
  efs_norm <- scale(efs_scale, center = T, scale = T)
  efs_norm <- data.frame(efs_norm)
  
  cell.cat <- ef.df.reo$cell.cat
  cell.id <- ef.df.reo$cell.id
  
  efs_scaled <- cbind(cell.id, cell.cat, efs_norm)  
  
# save  
  write.csv(efs_scaled, file = "C:/Users/s436862/Dropbox/Climate Matching/1. Data files/EFs/CSV/100-km scaled.csv", row.names = F)
  
  
  
  
  
  
  
  
  