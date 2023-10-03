
# date created: 25/2/19
# last updated: 


# Library --------------------------------------------------------------
  library(ggmap)
  library(raster)
  library(rgdal)
  library(maptools)
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/Data files")
  
# NZ shapefile
  nz.s <- readOGR("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/New Zealand/nz-coastlines-and-islands-polygons-topo-150k.shp")
  projection(nz.s) <- "+proj=utm +zone=48 +datum=WGS84"

# these files are big, so I am cropping them to something nearer NZ
  ext <- extent(c(160, 180, -55, 15))
 
  proj <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
# Environnmental variables ---------------------------------------------  
# (1) aridity (note: /10000) -------------------------------------------
  arid <- raster("CGIR CSI Aridity and Evaporation/Global Aridity - Annual/AI_annual/ai_yr/hdr.adf")/10000
  projection(arid) <- proj
  names(arid) <- ("arid")
  
  arid.c <- crop(arid, extent(ext))
  writeRaster(arid.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/arid.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/arid.grd")
  plot(b)
  
# (2) potential evapotranspiration (pet)-------------------------------------------
  pet <- raster("CGIR CSI Aridity and Evaporation/Global PET - Annual/PET_he_annual/pet_he_yr/hdr.adf")
  projection(pet) <- proj
  names(pet) <- ("pet")
  
  pet.c <- crop(pet, extent(ext))
  writeRaster(pet.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/pet.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/pet.grd")
  plot(b)
  
# (3) elevation -------------------------------------------
  elev <- raster("CGIR Elevation/Elevation 30 sec/GloElev_30as.asc")
  projection(elev) <- proj
  names(elev) <- ("elev")
  
  elev.c <- crop(elev, extent(ext))
  writeRaster(elev.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/elev.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/elev.grd")
  plot(b)
  
# (4) Annual Mean Temperature (AMT) -------------------------------------------
  amt <- raster("Worldclim/wc2.0_bio_30s_01.tif") 
  projection(amt) <- proj
  names(amt) <- ("amt")
  
  amt.c <- crop(amt, extent(ext))
  writeRaster(amt.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/amt.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/amt.grd")
  plot(b)   
  
# (5) Mean Diurnal Range (Mean of monthly (max temp - min temp)) (MDR) -----------------------------
  mdr <- raster("Worldclim/wc2.0_bio_30s_02.tif") 
  projection(mdr) <- proj
  names(mdr) <- ("mdr")
  
  mdr.c <- crop(mdr, extent(ext))
  writeRaster(mdr.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/mdr.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/mdr.grd")
  plot(b) 
  
# Free up workspace memory ---------------------------------------------------------
  rm(list = ls())
  ext <- extent(c(-180, 180, -60, 90))
  proj <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  
# (6) Isothermality (BIO2/BIO7) (* 100) (ISO) -------------------------------------------
  iso <- raster("Worldclim/wc2.0_bio_30s_03.tif") 
  projection(iso) <- proj
  names(iso) <- ("iso")
  
  iso.c <- crop(iso, extent(ext))
  writeRaster(iso.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/iso.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/iso.grd")
  plot(b)
  
# (7) Temperature Seasonality (standard deviation *100) (Note I divided by 100) (TS)  ----------------------
  ts <- raster("Worldclim/wc2.0_bio_30s_04.tif")/100
  projection(ts) <- proj
  names(ts) <- ("ts")
  
  ts.c <- crop(ts, extent(ext))
  writeRaster(ts.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/ts.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/ts.grd")
  plot(b)    
  
# (8) Max Temperature of Warmest Month (twarmm) ------------------------------------------
  twarmm <- raster("Worldclim/wc2.0_bio_30s_05.tif") 
  projection(twarmm) <- proj
  names(twarmm) <- ("twarmm")
  
  twarmm.c <- crop(twarmm, extent(ext))
  writeRaster(twarmm.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/twarmm.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/twarmm.grd")
  plot(b)     
  
# (9) Min Temperature of Coldest Month (tcoldm)   ----------------------------------------
  tcoldm <- raster("Worldclim/wc2.0_bio_30s_06.tif") 
  projection(tcoldm) <- proj
  names(tcoldm) <- ("tcoldm")
  
  tcoldm.c <- crop(tcoldm, extent(ext))
  writeRaster(tcoldm.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/tcoldm.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/tcoldm.grd")
  plot(b)
  
# (10) Temperature Annual Range (BIO5-BIO6) (TAR)  ------------------------------------
  tar <- raster("Worldclim/wc2.0_bio_30s_07.tif") 
  projection(tar) <- proj
  names(tar) <- ("tar")
  
  tar.c <- crop(tar, extent(ext))
  writeRaster(tar.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/tar.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/tar.grd")
  plot(b)    
  
# Free up workspace memory ---------------------------------------------------------
  rm(list = ls())
  ext <- extent(c(-180, 180, -60, 90))
  proj <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
# (11) Mean Temperature of Wettest Quarter (TWETQ)   ------------------------------------
  twetq <- raster("Worldclim/wc2.0_bio_30s_08.tif") 
  projection(twetq) <- proj
  names(twetq) <- ("twetq")
  
  twetq.c <- crop(twetq, extent(ext))
  writeRaster(twetq.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/twetq.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/twetq.grd")
  plot(b)     
  
# (12) Mean Temperature of Driest Quarter (TDRYQ)  ------------------------------------------- 
  tdryq <- raster("Worldclim/wc2.0_bio_30s_09.tif") 
  projection(tdryq) <- proj
  names(tdryq) <- ("tdryq")
  
  tdryq.c <- crop(tdryq, extent(ext))
  writeRaster(tdryq.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/tdryq.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/tdryq.grd")
  plot(b)   
  
# (13) Mean Temperature of Warmest Quarter (TWARMQ)  ------------------------------------------- 
  twarmq <- raster("Worldclim/wc2.0_bio_30s_10.tif")
  projection(twarmq) <- proj
  names(twarmq) <- ("twarmq")
  
  twarmq.c <- crop(twarmq, extent(ext))
  writeRaster(twarmq.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/twarmq.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/twarmq.grd")
  plot(b)   
  
# (14) Mean Temperature of Coldest Quarter (TCOLDQ)   ------------------------------------------- 
  tcoldq <- raster("Worldclim/wc2.0_bio_30s_11.tif")
  projection(tcoldq) <- proj
  names(tcoldq) <- ("tcoldq")
  
  tcoldq.c <- crop(tcoldq, extent(ext))
  writeRaster(tcoldq.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/tcoldq.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/tcoldq.grd")
  plot(b)
  
# (15) Annual Precipitation (AP)    -------------------------------------------
  ap <- raster("Worldclim/wc2.0_bio_30s_12.tif")
  projection(ap) <- proj
  names(ap) <- ("ap")
  
  ap.c <- crop(ap, extent(ext))
  writeRaster(ap.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/ap.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/ap.grd")
  plot(b)     
  
# Free up workspace memory ---------------------------------------------------------
  rm(list = ls())
  ext <- extent(c(-180, 180, -60, 90))
  proj <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"  
  
# (16) Precipitation of Wettest Month (PWETM)   -----------------------------------------
  pwetm <- raster("Worldclim/wc2.0_bio_30s_13.tif")
  projection(pwetm) <- proj
  names(pwetm) <- ("pwetm")
  
  pwetm.c <- crop(pwetm, extent(ext))
  writeRaster(pwetm.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/pwetm.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/pwetm.grd")
  plot(b)   
  
# (17) Precipitation of Driest Month (PDRYM)   -------------------------------------------
  pdrym <- raster("Worldclim/wc2.0_bio_30s_14.tif")
  projection(pdrym) <- proj
  names(pdrym) <- ("pdrym")
  
  pdrym.c <- crop(pdrym, extent(ext))
  writeRaster(pdrym.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/pdrym.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/pdrym.grd")
  plot(b)   
  
# (18) Precipitation Seasonality (Coefficient of Variation) (PS) ------------------------
  ps <- raster("Worldclim/wc2.0_bio_30s_15.tif")
  projection(ps) <- proj
  names(ps) <- ("ps")
  
  ps.c <- crop(ps, extent(ext))
  writeRaster(ps.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/ps.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/ps.grd")
  plot(b)        
  
# (19) Precipitation of Wettest Quarter (PWETQ) ------------------------------------------
  pwetq <- raster("Worldclim/wc2.0_bio_30s_16.tif")
  projection(pwetq) <- proj
  names(pwetq) <- ("pwetq")
  
  pwetq.c <- crop(pwetq, extent(ext))
  writeRaster(pwetq.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/pwetq.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/pwetq.grd")
  plot(b)      
  
# (20) Precipitation of Driest Quarter (PDRYQ) -------------------------------------------
  pdryq <- raster("Worldclim/wc2.0_bio_30s_17.tif")
  projection(pwetq) <- proj
  names(pwetq) <- ("pwetq")
  
  pdryq.c <- crop(pdryq, extent(ext))
  writeRaster(pwetq.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/pwetq.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/pwetq.grd")
  plot(b)    
  
# Free up workspace memory ---------------------------------------------------------
  rm(list = ls())
  ext <- extent(c(-180, 180, -60, 90))
  proj <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"  
  
  
# (21) Precipitation of Warmest Quarter (PWARMQ)   -------------------------------------------
  pwarmq <- raster("Worldclim/wc2.0_bio_30s_18.tif")
  projection(pwarmq) <- proj
  names(pwarmq) <- ("pwarmq")
  
  pwarmq.c <- crop(pwarmq, extent(ext))
  writeRaster(pwarmq.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/pwarmq.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/pwarmq.grd")
  plot(b)     
  
# (22) Precipitation of Coldest Quarter (PCOLDQ)   -------------------------------------------
  pcoldq <- raster("Worldclim/wc2.0_bio_30s_19.tif")
  projection(pcoldq) <- proj
  names(pcoldq) <- ("pcoldq")
  
  pcoldq.c <- crop(pcoldq, extent(ext))
  writeRaster(pcoldq.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/pcoldq.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/pcoldq.grd")
  plot(b) 
  
# (23) Human influence index --------------------------------------------------------
  hii <- raster("The Human Influence Index (HII)/hii-global-geo-grid/hii_v2geo/hdr.adf")
  projection(hii) <- proj
  names(hii) <- ("hii")
  
  hii.c <- crop(hii, extent(ext))
  writeRaster(hii.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/hii.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/hii.grd")
  plot(b)    
  
  
# (24) potential storage of water derived from soil texture (mm)) (st) -------------------
  st <- raster("Harmonised World Soil Database/WEBBSOIL_548/WEBBSOIL_548/data/wrtext.asc")
  projection(st) <- proj
  names(st) <- ("st")
  
  st.c <- crop(st, extent(ext))
  writeRaster(st.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/st.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/st.grd")
  plot(b) 
  
# (25) potential storage of water in the root zone (mm) (rz) -------------------------------------------
  rz <- raster("Harmonised World Soil Database/WEBBSOIL_548/WEBBSOIL_548/data/wrroot.asc")
  projection(rz) <- proj
  names(rz) <- ("rz")
  
  rz.c <- crop(rz, extent(ext))
  writeRaster(rz.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/rz.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/rz.grd")
  plot(b) 
  
# Free up workspace memory ---------------------------------------------------------
  rm(list = ls())
  ext <- extent(c(-180, 180, -60, 90))
  proj <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"  
  
# (26) potential storage of water in the soil profile (mm) (sp) -------------------------------------------
  sp <- raster("Harmonised World Soil Database/WEBBSOIL_548/WEBBSOIL_548/data/wrprof.asc")
  projection(sp) <- proj
  names(sp) <- ("sp")
  
  sp.c <- crop(sp, extent(ext))
  writeRaster(sp.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/sp.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/sp.grd")
  plot(b) 
  
# (27) plant available water capacity  -------------------------------------------
  pawc <- raster("C:/Users/s436862/Dropbox/Climate Matching/Data files/Plant water capacity/PAWC_1m/pawc_1m/hdr.adf")
  projection(pawc) <- proj
  names(pawc) <- ("pawc")
  
  pawc.c <- crop(pawc, extent(ext))
  writeRaster(pawc.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/pawc.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/pawc.grd")
  plot(b) 
  
# (28) Plant-extractabel water capacity -------------------------------------------
# again, dunno where I put og rasters (think I did them in ARC GIS?)
# so I am doing it here
  pewc <- raster("C:/Users/s436862/Dropbox/Climate Matching/Data files/Plant water capacity/DUNNESOIL_545/dunne_soil.dat")
  projection(pewc) <- proj
  names(pewc) <- ("pewc")
  
  pewc.c <- crop(pewc, extent(ext))
  writeRaster(pewc.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/pewc.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/pewc.grd")
  plot(b) 
  
# (29) Zobler soil layers  -------------------------------------------
# See what is in this layer
  zobs <- raster("C:/Users/s436862/Dropbox/Climate Matching/Data files/Zobler half degree soil layers/ZOBLERSOILDERIVED_540/data/z_soiltype.dat")
  projection(zobs) <- proj
  names(zobs) <- ("zobs")
  
  zobs.c <- crop(zobs, extent(ext))
  writeRaster(zobs.c, "C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/zobs.grd", prj = T, overwrite = T)
  b <- raster("C:/Users/s436862/Dropbox/Rarefaction New Zealand/Data files/EVs/Raw NZ cropped/zobs.grd")
  plot(b) 
  
# --------------------------------------------------------------------------------
  
  
  