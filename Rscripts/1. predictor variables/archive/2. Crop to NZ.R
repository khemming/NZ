

# date created: 17/7/17
# last updated: 26/2/19

# Cropping rasters to NZ region

  rm(list = ls())
  
  library(raster)
  library(ggplot2)
  library(ggmap)
  library(tidyr)
  library(rgdal)
  library(maptools)
  
  setwd("C:/Users/s436862/Dropbox/NZ biogeography/Data files")

# Cropping -----------------------------------------------------
# Note: memory is limited, so I am completing this five rasters at a time, clearing environment between with 'set-up'.  
  
# Creating New Zealand raster template -------------------------------
# # arbitrary raster dough to cookie cut
#   nz <- raster("EVs/Raw NZ cropped/arid.grd")
#   plot(nz)
# 
# # crop to nz-only    
#   nz.s <- readOGR("New Zealand/nz-coastlines-and-islands-polygons-topo-150k.shp")
#   nz.c <- crop(nz, nz.s)
#   plot(nz.c)
# 
# # almost to right size - still has an offshore island I will remove
#   ext <- extent(c(165, 180, -49, -30))
#   nz.c2 <- crop(nz.c, ext)
#   plot(nz.c2) # beautiful
#   
# # save raster
#   writeRaster(nz.c2, "New Zealand/NZ 1-km.grd")
#   
# dataframe for later --- terrestrial categories

# --------------------------------------------------------------------    
  
# 1-5 ----------------------------------------------------------------
# Set up -------------------------------------------------------------  
# Template: 1-km resolution nztralia raster
  rm(list = ls())
  nz.s <- readOGR("New Zealand/nz-coastlines-and-islands-polygons-topo-150k.shp")
  
  projection(nz.s) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  nz <- raster("New Zealand/NZ 1-km.grd")
  
# Projection
  pro.crs <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# AMT -------------------------------------------
  amt <- raster("EVs/Raw NZ cropped/amt.grd")
  amt.c <- crop(amt, nz.s)
  plot(amt.c)
  
  amt.pro <- projectRaster(amt.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(amt.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(amt.pro, "EVs/1-km cropped/amt.grd", prj = T, overwrite = T)  
  
# MDR --------------------------
  mdr <- raster("EVs/Raw NZ cropped/mdr.grd") 
  mdr.c <- crop(mdr, nz.s)
  plot(mdr.c)
  
  mdr.pro <- projectRaster(mdr.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(mdr.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(mdr.pro, "EVs/1-km cropped/mdr.grd", prj = T, overwrite = T)  
  
# ISO ---------------------------------------------------------------------------  
  iso <- raster("EVs/Raw NZ cropped/iso.grd")
  iso.c <- crop(iso, nz.s)
  plot(iso.c)
  
  iso.pro <- projectRaster(iso.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(iso.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(iso.pro, "EVs/1-km cropped/iso.grd", prj = T, overwrite = T)  

# TS -----------------------------------------------------------------------      
  ts <- raster("EVs/Raw NZ cropped/ts.grd")
  ts.c <- crop(ts, nz.s)
  plot(ts.c)
  
  ts.pro <- projectRaster(ts.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(ts.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(ts.pro, "EVs/1-km cropped/ts.grd", prj = T, overwrite = T)  
  
# TAR --------------------------------------------------------------  
  tar <- raster("EVs/Raw NZ cropped/ts.grd")
  tar.c <- crop(tar, nz.s)
  plot(tar.c)
  
  tar.pro <- projectRaster(tar.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(tar.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
 writeRaster(tar.pro, "EVs/1-km cropped/tar.grd", prj = T, overwrite = T)  
  
# ----------------------------------
  
# 6-10 ---------------------------
# Set up -------------------------------------------------------------  
# Template: 1-km resolution nztralia raster
  rm(list = ls())
  
  nz.s <- readOGR("New Zealand/nz-coastlines-and-islands-polygons-topo-150k.shp")
  
  projection(nz.s) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"    
  
  nz <- raster("New Zealand/NZ 1-km.grd")
  
# Projection
  pro.crs <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
# TWARMM --------------------------------------------------------------    
  twarmm <- raster("EVs/Raw NZ cropped/twarmm.grd")
  twarmm.c <- crop(twarmm, nz.s)
  plot(twarmm.c)
  
  twarmm.pro <- projectRaster(twarmm.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(twarmm.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(twarmm.pro, "EVs/1-km cropped/twarmm.grd", prj = T, overwrite = T)  
  
# TCOLDM -------------------------------------------------------------
  tcoldm <- raster("EVs/Raw NZ cropped/tcoldm.grd")
  tcoldm.c <- crop(tcoldm, nz.s)
  plot(tcoldm.c)
  
  tcoldm.pro <- projectRaster(tcoldm.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(tcoldm.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(tcoldm.pro, "EVs/1-km cropped/tcoldm.grd", prj = T, overwrite = T)  
  
# TWETQ ------------------------------------------------------------------
  twetq <- raster("EVs/Raw NZ cropped/twetq.grd")
  twetq.c <- crop(twetq, nz.s)
  plot(twetq.c)
  
  twetq.pro <- projectRaster(twetq.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(twetq.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(twetq.pro, "EVs/1-km cropped/twetq.grd", prj = T, overwrite = T)  
  
# TDRYQ ----------------------------------------------------------------------  
  tdryq <- raster("EVs/Raw NZ cropped/tdryq.grd")
  tdryq.c <- crop(tdryq, nz.s)
  plot(tdryq.c)
  
  tdryq.pro <- projectRaster(tdryq.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(tdryq.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(tdryq.pro, "EVs/1-km cropped/tdryq.grd", prj = T, overwrite = T)  
  
# TWARMQ --------------------------------------------------------------------
  twarmq <- raster("EVs/Raw NZ cropped/twarmq.grd")
  twarmq.c <- crop(twarmq, nz.s)
  plot(twarmq.c)
  
  twarmq.pro <- projectRaster(twarmq.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(twarmq.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(twarmq.pro, "EVs/1-km cropped/twarmq.grd", prj = T, overwrite = T)  
# ----------------------------------
  
# 11-15 --------------------------- 
# Set up -------------------------------------------------------------  
# Template: 1-km resolution nztralia raster
  rm(list = ls())
  
  nz.s <- readOGR("New Zealand/nz-coastlines-and-islands-polygons-topo-150k.shp")
  
  projection(nz.s) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"    
  
  nz <- raster("New Zealand/NZ 1-km.grd")
  
  # Projection
  pro.crs <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
# TCOLDQ -------------------------------------------------------------------  
  tcoldq <- raster("EVs/Raw NZ cropped/tcoldq.grd")
  tcoldq.c <- crop(tcoldq, nz.s)
  plot(tcoldq.c)
  
  tcoldq.pro <- projectRaster(tcoldq.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(tcoldq.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(tcoldq.pro, "EVs/1-km cropped/tcoldq.grd", prj = T, overwrite = T)  
# AP -----------------------------------------------------------------------
  ap <- raster("EVs/Raw NZ cropped/ap.grd")
  ap.c <- crop(ap, nz.s)
  plot(ap.c)
  
  ap.pro <- projectRaster(ap.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(ap.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(ap.pro, "EVs/1-km cropped/ap.grd", prj = T, overwrite = T)  
# PWETM --------------------------------------------------------------------
  pwetm <- raster("EVs/Raw NZ cropped/pwetm.grd")
  pwetm.c <- crop(pwetm, nz.s)
  plot(pwetm.c)
  
  pwetm.pro <- projectRaster(pwetm.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(pwetm.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(pwetm.pro, "EVs/1-km cropped/pwetm.grd", prj = T, overwrite = T)  
# PDRYM --------------------------------------------------------------------
  pdrym <- raster("EVs/Raw NZ cropped/pdrym.grd")
  pdrym.c <- crop(pdrym, nz.s)
  plot(pdrym.c)
  
  pdrym.pro <- projectRaster(pdrym.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(pdrym.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(pdrym.pro, "EVs/1-km cropped/pdrym.grd", prj = T, overwrite = T)  
# PS ------------------------------------------------------------------------  
  ps <- raster("EVs/Raw NZ cropped/ps.grd")
  ps.c <- crop(ps, nz.s)
  plot(ps.c)
  
  ps.pro <- projectRaster(ps.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(ps.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(ps.pro, "EVs/1-km cropped/ps.grd", prj = T, overwrite = T)  
# ---------------------------------  

# 16-20 ------------------------- 
# Set up -------------------------------------------------------------  
# Template: 1-km resolution nztralia raster
  rm(list = ls())
  nz.s <- readOGR("New Zealand/nz-coastlines-and-islands-polygons-topo-150k.shp")
  
  projection(nz.s) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"    
  
  nz <- raster("New Zealand/NZ 1-km.grd")
  
  # Projection
  pro.crs <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
# PWETQ ---------------------------------------------------------------------
  pwetq <- raster("EVs/Raw NZ cropped/pwetq.grd")
  pwetq.c <- crop(pwetq, nz.s)
  plot(pwetq.c)
  
  pwetq.pro <- projectRaster(pwetq.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(pwetq.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(pwetq.pro, "EVs/1-km cropped/pwetq.grd", prj = T, overwrite = T)  
# PDRYQ ----------------------------------------------------------------------  
  pdryq <- raster("EVs/Raw NZ cropped/pdryq.grd")
  pdryq.c <- crop(pdryq, nz.s)
  plot(pdryq.c)
  
  pdryq.pro <- projectRaster(pdryq.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(pdryq.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(pdryq.pro, "EVs/1-km cropped/pdryq.grd", prj = T, overwrite = T)  
# PWARMQ --------------------------------------------------------------------  
  pwarmq <- raster("EVs/Raw NZ cropped/pwarmq.grd")
  pwarmq.c <- crop(pwarmq, nz.s)
  plot(pwarmq.c)
  
  pwarmq.pro <- projectRaster(pwarmq.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(pwarmq.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(pwarmq.pro, "EVs/1-km cropped/pwarmq.grd", prj = T, overwrite = T)  
# PCOLDQ ---------------------------------------------------------------------  
  pcoldq <- raster("EVs/Raw NZ cropped/pcoldq.grd")
  pcoldq.c <- crop(pcoldq, nz.s)
  plot(pcoldq.c)
  
  pcoldq.pro <- projectRaster(pcoldq.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(pcoldq.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(pcoldq.pro, "EVs/1-km cropped/pcoldq.grd", prj = T, overwrite = T)  
  
# ARIDITY --------------------------------------------------------------------------  
  arid <- raster("EVs/Raw NZ cropped/arid.grd")
  arid.c <- crop(arid, nz.s)
  plot(arid.c)
  
  arid.pro <- projectRaster(arid.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(arid.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(arid.pro, "EVs/1-km cropped/arid.grd", prj = T, overwrite = T)  
  
  
# -------------------------------  
  
# 21-25 ------------------------- 
# Set up -------------------------------------------------------------  
# Template: 1-km resolution nztralia raster
  rm(list = ls())
  nz.s <- readOGR("New Zealand/nz-coastlines-and-islands-polygons-topo-150k.shp")
  
  projection(nz.s) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"    
  
  nz <- raster("New Zealand/NZ 1-km.grd")
  
  # Projection
  pro.crs <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
# ELEVATION -------------------------------------------------------------------   
  elev <- raster("EVs/Raw NZ cropped/elev.grd")
  elev.c <- crop(elev, nz.s)
  plot(elev.c)
  
  elev.pro <- projectRaster(elev.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(elev.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(elev.pro, "EVs/1-km cropped/elev.grd", prj = T, overwrite = T)  
  
# PET -------------------------------------------------------------------------------
  pet <- raster("EVs/Raw NZ cropped/pet.grd")
  pet.c <- crop(pet, nz.s)
  plot(pet.c)
  
  pet.pro <- projectRaster(pet.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(pet.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(pet.pro, "EVs/1-km cropped/pet.grd", prj = T, overwrite = T)  
# HII ------------------------------------------------------------------------------
  hii <- raster("EVs/Raw NZ cropped/hii.grd")
  hii.c <- crop(hii, nz.s)
  plot(hii.c)
  
  hii.pro <- projectRaster(hii.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(hii.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(hii.pro, "EVs/1-km cropped/hii.grd", prj = T, overwrite = T)  
  
# RZ -------------------------------------------------------------------------------  
  rz <- raster("EVs/Raw NZ cropped/rz.grd")
  rz.c <- crop(rz, nz.s)
  plot(rz.c)
  
  rz.pro <- projectRaster(rz.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(rz.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(rz.pro, "EVs/1-km cropped/rz.grd", prj = T, overwrite = T)  
  
# SP ----------------------------------------------------------------------------  
  sp <- raster("EVs/Raw NZ cropped/sp.grd")
  sp.c <- crop(sp, nz.s)
  plot(sp.c)
  
  sp.pro <- projectRaster(sp.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(sp.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(sp.pro, "EVs/1-km cropped/sp.grd", prj = T, overwrite = T)  
  
  
# -------------------------------
  
# 26-29 ------------------------- 
# Set up -------------------------------------------------------------  
# Template: 1-km resolution nztralia raster
  rm(list = ls())
  nz.s <- readOGR("New Zealand/nz-coastlines-and-islands-polygons-topo-150k.shp")
  
  projection(nz.s) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"    
  
  nz <- raster("New Zealand/NZ 1-km.grd")
  
  # Projection
  pro.crs <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
# ST -------------------------------------------------------------------------------  
  st <- raster("EVs/Raw NZ cropped/st.grd")
  st.c <- crop(st, nz.s)
  plot(st.c)
  
  st.pro <- projectRaster(st.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(st.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(st.pro, "EVs/1-km cropped/st.grd", prj = T, overwrite = T)  
  
# PAWC -----------------------------------------------------------------------------  
  pawc <- raster("EVs/Raw NZ cropped/pawc.grd")
  pawc.c <- crop(pawc, nz.s)
  plot(pawc.c)
  
  pawc.pro <- projectRaster(pawc.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(pawc.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(pawc.pro, "EVs/1-km cropped/pawc.grd", prj = T, overwrite = T)  

# PEWC ----------------------------------------------------------------------------  
  pewc <- raster("EVs/Raw NZ cropped/pewc.grd")
  pewc.c <- crop(pewc, nz.s)
  plot(pewc.c)
  
  pewc.pro <- projectRaster(pewc.c, nz, res = 0.01, crs = pro.crs, method="bilinear")
  plot(pewc.pro)
  
  amt.pro <- mask(amt.pro, nz)
  plot(atm.pro)
  
  writeRaster(pewc.pro, "EVs/1-km cropped/pewc.grd", prj = T, overwrite = T)  
  
# ---------------------------------------------------------------------------------