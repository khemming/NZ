
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

# shapefile
  aus.shp <- readOGR("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/Australia/australia_shapefile.shp")
  
# Aggregate to 100-km and crop to size rasters from 2. ------------------------
  arid <- raster("EFs/Rasters 1-km Aus-cropped/arid.grd")
  arid.a <- aggregate(arid, fact = 100, fun = mean) 
  arid.a.m <- mask(arid.a, aus.shp)
  plot(arid.a.m)
  writeRaster(arid.a.m, "EFs/Rasters 100-km/arid.grd")
  
  rz <- raster("EFs/Rasters 1-km Aus-cropped/rz.grd")
  rz.a <- aggregate(rz, fact = 100, fun = mean) 
  rz.a.m <- mask(rz.a, aus.shp)
  plot(rz.a.m)
  writeRaster(rz.a.m, "EFs/Rasters 100-km/rz.grd")
  
  sp <- raster("EFs/Rasters 1-km Aus-cropped/sp.grd")
  sp.a <- aggregate(sp, fact = 100, fun = mean) 
  sp.a.m <- mask(sp.a, aus.shp)
  plot(sp.a.m)
  writeRaster(sp.a.m, "EFs/Rasters 100-km/sp.grd")
  
  st <- raster("EFs/Rasters 1-km Aus-cropped/st.grd")
  st.a <- aggregate(st, fact = 100, fun = mean) 
  st.a.m <- mask(st.a, aus.shp)
  plot(st.a.m)
  writeRaster(st.a.m, "EFs/Rasters 100-km/st.grd")
  
  elev <- raster("EFs/Rasters 1-km Aus-cropped/elev.grd")
  elev.a <- aggregate(elev, fact = 100, fun = mean) 
  elev.a.m <- mask(elev.a, aus.shp)
  plot(elev.a.m)
  writeRaster(elev.a.m, "EFs/Rasters 100-km/elev.grd")
  
  th <- aggregate(elev, fact = 100, fun = sd)
  th.m <- mask(th, aus.shp)
  plot(th.m)
  writeRaster(th.m, "EFs/Rasters 100-km/th.grd")
  
  pet <- raster("EFs/Rasters 1-km Aus-cropped/pet.grd")
  pet.a <- aggregate(pet, fact = 100, fun = mean) 
  pet.a.m <- mask(pet.a, aus.shp)
  plot(pet.a.m)
  writeRaster(pet.a.m, "EFs/Rasters 100-km/pet.grd")
  
  pawc <- raster("EFs/Rasters 1-km Aus-cropped/pawc.grd")
  pawc.a <- aggregate(pawc, fact = 100, fun = mean) 
  pawc.a.m <- mask(pawc.a, aus.shp)
  plot(pawc.a.m)
  writeRaster(pawc.a.m, "EFs/Rasters 100-km/pawc.grd")
  
  pewc <- raster("EFs/Rasters 1-km Aus-cropped/pewc.grd")
  pewc.a <- aggregate(pewc, fact = 100, fun = mean) 
  pewc.a.m <- mask(pewc.a, aus.shp)
  plot(pewc.a.m)
  writeRaster(pewc.a.m, "EFs/Rasters 100-km/pewc.grd")
  
  amt <- raster("EFs/Rasters 1-km Aus-cropped/amt.grd")
  amt.a <- aggregate(amt, fact = 100, fun = mean) 
  amt.a.m <- mask(amt.a, aus.shp)
  plot(amt.a.m)
  writeRaster(amt.a.m, "EFs/Rasters 100-km/amt.grd")
  
  mdr <- raster("EFs/Rasters 1-km Aus-cropped/mdr.grd")
  mdr.a <- aggregate(mdr, fact = 100, fun = mean) 
  mdr.a.m <- mask(mdr.a, aus.shp)
  plot(mdr.a.m)
  writeRaster(mdr.a.m, "EFs/Rasters 100-km/mdr.grd")
  
  iso <- raster("EFs/Rasters 1-km Aus-cropped/iso.grd")
  iso.a <- aggregate(iso, fact = 100, fun = mean) 
  iso.a.m <- mask(iso.a, aus.shp)
  plot(iso.a.m)
  writeRaster(iso.a.m, "EFs/Rasters 100-km/iso.grd")
  
  ts <- raster("EFs/Rasters 1-km Aus-cropped/ts.grd")
  ts.a <- aggregate(ts, fact = 100, fun = mean) 
  ts.a.m <- mask(ts.a, aus.shp)
  plot(ts.a.m)
  writeRaster(ts.a.m, "EFs/Rasters 100-km/ts.grd")
  
  twarmm <- raster("EFs/Rasters 1-km Aus-cropped/twarmm.grd")
  twarmm.a <- aggregate(twarmm, fact = 100, fun = mean) 
  twarmm.a.m <- mask(twarmm.a, aus.shp)
  plot(twarmm.a.m)
  writeRaster(twarmm.a.m, "EFs/Rasters 100-km/twarmm.grd")
  
  tcoldm <- raster("EFs/Rasters 1-km Aus-cropped/tcoldm.grd")
  tcoldm.a <- aggregate(tcoldm, fact = 100, fun = mean) 
  tcoldm.a.m <- mask(tcoldm.a, aus.shp)
  plot(tcoldm.a.m)
  writeRaster(tcoldm.a.m, "EFs/Rasters 100-km/tcoldm.grd")
  
  tar <- raster("EFs/Rasters 1-km Aus-cropped/tar.grd")
  tar.a <- aggregate(tar, fact = 100, fun = mean) 
  tar.a.m <- mask(tar.a, aus.shp)
  plot(tar.a.m)
  writeRaster(tar.a.m, "EFs/Rasters 100-km/tar.grd")
  
  hii <- raster("EFs/Rasters 1-km Aus-cropped/hii.grd")
  hii.a <- aggregate(hii, fact = 100, fun = median) 
  hii.a.m <- mask(hii.a, aus.shp)
  plot(hii.a.m)
  writeRaster(hii.a.m, "EFs/Rasters 100-km/hii.grd")
  
  twetq <- raster("EFs/Rasters 1-km Aus-cropped/twetq.grd")
  twetq.a <- aggregate(twetq, fact = 100, fun = mean) 
  twetq.a.m <- mask(twetq.a, aus.shp)
  plot(twetq.a.m)
  writeRaster(twetq.a.m, "EFs/Rasters 100-km/twetq.grd")
  
  tdryq <- raster("EFs/Rasters 1-km Aus-cropped/tdryq.grd")
  tdryq.a <- aggregate(tdryq, fact = 100, fun = mean) 
  tdryq.a.m <- mask(tdryq.a, aus.shp)
  plot(tdryq.a.m)
  writeRaster(tdryq.a.m, "EFs/Rasters 100-km/tdryq.grd")
  
  twarmq <- raster("EFs/Rasters 1-km Aus-cropped/twarmq.grd")
  twarmq.a <- aggregate(twarmq, fact = 100, fun = mean) 
  twarmq.a.m <- mask(twarmq.a, aus.shp)
  plot(twarmq.a.m)
  writeRaster(twarmq.a.m, "EFs/Rasters 100-km/twarmq.grd")
  
  tcoldq <- raster("EFs/Rasters 1-km Aus-cropped/tcoldq.grd")
  tcoldq.a <- aggregate(tcoldq, fact = 100, fun = mean) 
  tcoldq.a.m <- mask(tcoldq.a, aus.shp)
  plot(tcoldq.a.m)
  writeRaster(tcoldq.a.m, "EFs/Rasters 100-km/tcoldq.grd")
  
  ap <- raster("EFs/Rasters 1-km Aus-cropped/ap.grd")
  ap.a <- aggregate(ap, fact = 100, fun = mean) 
  ap.a.m <- mask(ap.a, aus.shp)
  plot(ap.a.m)
  writeRaster(ap.a.m, "EFs/Rasters 100-km/ap.grd")
  
  pwetm <- raster("EFs/Rasters 1-km Aus-cropped/pwetm.grd")
  pwetm.a <- aggregate(pwetm, fact = 100, fun = mean) 
  pwetm.a.m <- mask(pwetm.a, aus.shp)
  plot(pwetm.a.m)
  writeRaster(pwetm.a.m, "EFs/Rasters 100-km/pwetm.grd")
  
  pdrym <- raster("EFs/Rasters 1-km Aus-cropped/pdrym.grd")
  pdrym.a <- aggregate(pdrym, fact = 100, fun = mean) 
  pdrym.a.m <- mask(pdrym.a, aus.shp)
  plot(pdrym.a.m)
  writeRaster(pdrym.a.m, "EFs/Rasters 100-km/pdrym.grd")
  
  ps <- raster("EFs/Rasters 1-km Aus-cropped/ps.grd")
  ps.a <- aggregate(ps, fact = 100, fun = mean) 
  ps.a.m <- mask(ps.a, aus.shp)
  plot(ps.a.m)
  writeRaster(ps.a.m, "EFs/Rasters 100-km/ps.grd")
  
  pwetq <- raster("EFs/Rasters 1-km Aus-cropped/pwetq.grd")
  pwetq.a <- aggregate(pwetq, fact = 100, fun = mean) 
  pwetq.a.m <- mask(pwetq.a, aus.shp)
  plot(pwetq.a.m)
  writeRaster(pwetq.a.m, "EFs/Rasters 100-km/pwetq.grd")
  
  pdryq <- raster("EFs/Rasters 1-km Aus-cropped/pdryq.grd")
  pdryq.a <- aggregate(pdryq, fact = 100, fun = mean) 
  pdryq.a.m <- mask(pdryq.a, aus.shp)
  plot(pdryq.a.m)
  writeRaster(pdryq.a.m, "EFs/Rasters 100-km/pdryq.grd")
  
  pwarmq <- raster("EFs/Rasters 1-km Aus-cropped/pwarmq.grd")
  pwarmq.a <- aggregate(pwarmq, fact = 100, fun = mean) 
  pwarmq.a.m <- mask(pwarmq.a, aus.shp)
  plot(pwarmq.a.m)
  writeRaster(pwarmq.a.m, "EFs/Rasters 100-km/pwarmq.grd")
  
  pcoldq <- raster("EFs/Rasters 1-km Aus-cropped/pcoldq.grd")
  pcoldq.a <- aggregate(pcoldq, fact = 100, fun = mean) 
  pcoldq.a.m <- mask(pcoldq.a, aus.shp)
  plot(pcoldq.a.m)
  writeRaster(pcoldq.a.m, "EFs/Rasters 100-km/pcoldq.grd")
  
  
  
  