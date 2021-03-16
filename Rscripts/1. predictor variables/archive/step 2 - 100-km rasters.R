
########################################################################################
# step two: reproject New Zealand 1 km raster to 100 km resolution
########################################################################################


# Library ---------------------------------------------------------
  library(raster)
  library(dplyr)
  library(rgdal)
  library(purrr)
  
  rm(list = ls())

# scale up resolution of cells from 1 km to 100 km -----------------
# aridity ----------------------------------------------------------  
  arid <- raster("Data files/EVs/step 1 - 1-km cropped/arid.grd")
  arid
  arid.ag <- aggregate(arid, fact = 100, fun = mean)
  writeRaster(arid.ag, "Data files/EVs/step 2 - 100-km/arid.grd", overwrite = T)
  arid.ag
  plot(arid.ag)
  
# annual mean temperature ------------------------------------------  
  amt <- raster("Data files/EVs/step 1 - 1-km cropped/amt.grd")
  amt
  amt.ag <- aggregate(amt, fact = 100, fun = mean)
  writeRaster(amt.ag, "Data files/EVs/step 2 - 100-km/amt.grd", overwrite = T)
  amt.ag
  plot(amt.ag)
  
# annual precipitation ---------------------------------------------
  ap <- raster("Data files/EVs/step 1 - 1-km cropped/ap.grd")
  ap
  ap.ag <- aggregate(ap, fact = 100, fun = mean)
  writeRaster(ap.ag, "Data files/EVs/step 2 - 100-km/ap.grd", overwrite = T)
  plot(ap.ag)
  ap.ag
    
# elevation --------------------------------------------------------  
  elev <- raster("Data files/EVs/step 1 - 1-km cropped/elev.grd")
  elev
  elev.ag <- aggregate(elev, fact = 100, fun = mean)
  writeRaster(elev.ag, "Data files/EVs/step 2 - 100-km/elev.grd", overwrite = T)
  plot(elev.ag)
  elev.ag

# topographical heterogeneity -------------------------------------  
  th.ag <- aggregate(elev, fact = 100, fun = sd) # note: sd for topographic heterogeneity
  plot(th.ag)
  writeRaster(th.ag, "Data files/EVs/step 2 - 100-km/th.grd", overwrite = T)
  th.ag
  
# human influence index ----------------------------------------------  
  hii <- raster("Data files/EVs/step 1 - 1-km cropped/hii.grd")
  hii.ag <- aggregate(hii, fact = 100, fun = median) # note median
  writeRaster(hii.ag, "Data files/EVs/step 2 - 100-km/hii.grd", overwrite = T)
  plot(hii.ag)
  hii.ag

# isothermality ----------------------------------------------------  
  iso <- raster("Data files/EVs/step 1 - 1-km cropped/iso.grd")
  iso.ag <- aggregate(iso, fact = 100, fun = mean)
  writeRaster(iso.ag, "Data files/EVs/step 2 - 100-km/iso.grd", overwrite = T)
  plot(iso.ag)
  iso.ag
  
# mean diurbnal range ----------------------------------------------
  mdr <- raster("Data files/EVs/step 1 - 1-km cropped/mdr.grd")
  mdr.ag <- aggregate(mdr, fact = 100, fun = mean)
  writeRaster(mdr.ag, "Data files/EVs/step 2 - 100-km/mdr.grd", overwrite = T)
  plot(mdr.ag)
  mdr.ag 
  
# plant available [soil] water capacity ------------------------
# Australian extent -- ignore
  
  # pawc <- raster("Data files/EVs/step 1 - 1-km cropped/pawc.grd")
  # pawc.ag <- aggregate(pawc, fact = 100, fun = mean)
  # writeRaster(pawc.ag, "Data files/EVs/step 2 - 100-km/pawc.grd", overwrite = T)
  # plot(pawc.ag)
  # pawc.ag
  
# precipitation of coldest quarter ------------------------  
  pcoldq <- raster("Data files/EVs/step 1 - 1-km cropped/pcoldq.grd")
  pcoldq.ag <- aggregate(pcoldq, fact = 100, fun = mean)
  writeRaster(pcoldq.ag, "Data files/EVs/step 2 - 100-km/pcoldq.grd", overwrite = T)
  plot(pcoldq.ag)
  pcoldq.ag
  
# precipitation of driest month ---------------------------    
  pdrym <- raster("Data files/EVs/step 1 - 1-km cropped/pdrym.grd")
  pdrym.ag <- aggregate(pdrym, fact = 100, fun = mean)
  writeRaster(pdrym.ag, "Data files/EVs/step 2 - 100-km/pdrym.grd", overwrite = T)
  plot(pdrym.ag)
  pdrym.ag
  
# potential evapo-transiration ------------------------  
  pet <- raster("Data files/EVs/step 1 - 1-km cropped/pet.grd")
  pet.ag <- aggregate(pet, fact = 100, fun = mean)
  writeRaster(pet.ag, "Data files/EVs/step 2 - 100-km/pet.grd", overwrite = T)
  plot(pet.ag)
  pet.ag
  
# plant extractable [soil] water capacity ------------------------    
  pewc <- raster("Data files/EVs/step 1 - 1-km cropped/pewc.grd")
  pewc.ag <- aggregate(pewc, fact = 100, fun = mean)
  writeRaster(pewc.ag, "Data files/EVs/step 2 - 100-km/pewc.grd", overwrite = T)
  plot(pewc.ag)
  
# precipitation seasonality -------------------------------------  
  ps <- raster("Data files/EVs/step 1 - 1-km cropped/ps.grd")
  ps.ag <- aggregate(ps, fact = 100, fun = mean)
  writeRaster(ps.ag, "Data files/EVs/step 2 - 100-km/ps.grd", overwrite = T)
  plot(ps.ag)
  
# precipitation of the warmest quarter -------------------------
  pwarmq <- raster("Data files/EVs/step 1 - 1-km cropped/pwarmq.grd")
  pwarmq.ag <- aggregate(pwarmq, fact = 100, fun = mean)
  writeRaster(pwarmq.ag, "Data files/EVs/step 2 - 100-km/pwarmq.grd", overwrite = T)
  plot(pwarmq.ag)
  
# precipitation of the wettest month -----------------------------  
  pwetm <- raster("Data files/EVs/step 1 - 1-km cropped/pwetm.grd")
  pwetm.ag <- aggregate(pwetm, fact = 100, fun = mean)
  writeRaster(pwetm.ag, "Data files/EVs/step 2 - 100-km/pwetm.grd", overwrite = T)
  plot(pwetm.ag)
  
# precipitation of the wettest quarter -----------------------------  
  pwetq <- raster("Data files/EVs/step 1 - 1-km cropped/pwetq.grd")
  pwetq.ag <- aggregate(pwetq, fact = 100, fun = mean)
  writeRaster(pwetq.ag, "Data files/EVs/step 2 - 100-km/pwetq.grd", overwrite = T)
  plot(pwetq.ag)
  
# [soil] water holding capcity of the root zone --------------------  
  rz <- raster("Data files/EVs/step 1 - 1-km cropped/rz.grd")
  rz.ag <- aggregate(rz, fact = 100, fun = mean)
  writeRaster(rz.ag, "Data files/EVs/step 2 - 100-km/rz.grd", overwrite = T)
  plot(rz.ag)

# [soil] water holding capcity of the soil particles --------------------    
  sp <- raster("Data files/EVs/step 1 - 1-km cropped/sp.grd")
  sp.ag <- aggregate(sp, fact = 100, fun = mean)
  writeRaster(sp.ag, "Data files/EVs/step 2 - 100-km/sp.grd", overwrite = T)
  plot(sp.ag)
  
# [soil] water holding capcity of the soil texture --------------------  
  st <- raster("Data files/EVs/step 1 - 1-km cropped/st.grd")
  st.ag <- aggregate(st, fact = 100, fun = mean)
  writeRaster(st.ag, "Data files/EVs/step 2 - 100-km/st.grd", overwrite = T)
  plot(st.ag)
  
# temperature annual range---------------------- --------------------  
  tar <- raster("Data files/EVs/step 1 - 1-km cropped/tar.grd")
  tar.ag <- aggregate(tar, fact = 100, fun = mean)
  writeRaster(tar.ag, "Data files/EVs/step 2 - 100-km/tar.grd", overwrite = T)
  plot(tar.ag)
  
# minimum tempature of the coldest month ---------------------------  
  tcoldm <- raster("Data files/EVs/step 1 - 1-km cropped/tcoldm.grd")
  tcoldm.ag <- aggregate(tcoldm, fact = 100, fun = mean)
  writeRaster(tcoldm.ag, "Data files/EVs/step 2 - 100-km/tcoldm.grd", overwrite = T)
  plot(tcoldm.ag)
  
# mean temperature of the coldest quarter --------------------------  
  tcoldq <- raster("Data files/EVs/step 1 - 1-km cropped/tcoldq.grd")
  tcoldq.ag <- aggregate(tcoldq, fact = 100, fun = mean)
  writeRaster(tcoldq.ag, "Data files/EVs/step 2 - 100-km/tcoldq.grd", overwrite = T)
  plot(tcoldq.ag)
  
# mean temperature of the driest quarter ------------------------
  tdryq <- raster("Data files/EVs/step 1 - 1-km cropped/tdryq.grd")
  tdryq.ag <- aggregate(tdryq, fact = 100, fun = mean)
  writeRaster(tdryq.ag, "Data files/EVs/step 2 - 100-km/tdryq.grd", overwrite = T)
  plot(tdryq.ag)
  
# temperature seasonality ----------------------------------------  
  ts <- raster("Data files/EVs/step 1 - 1-km cropped/ts.grd")
  ts.ag <- aggregate(ts, fact = 100, fun = mean)
  writeRaster(ts.ag, "Data files/EVs/step 2 - 100-km/ts.grd", overwrite = T)
  plot(ts.ag)
  
# maximum temperature of the warmest month ------------------------ 
  twarmm <- raster("Data files/EVs/step 1 - 1-km cropped/twarmm.grd")
  twarmm.ag <- aggregate(twarmm, fact = 100, fun = mean)
  writeRaster(twarmm.ag, "Data files/EVs/step 2 - 100-km/twarmm.grd", overwrite = T)
  plot(twarmm.ag)
  
# mean temperature of the warmest quarter -----------------------
  twarmq <- raster("Data files/EVs/step 1 - 1-km cropped/twarmq.grd")
  twarmq.ag <- aggregate(twarmq, fact = 100, fun = mean)
  writeRaster(twarmq.ag, "Data files/EVs/step 2 - 100-km/twarmq.grd", overwrite = T)
  plot(twarmq.ag)
  
# mean temperature of the wettest quarter -----------------------  
  twetq <- raster("Data files/EVs/step 1 - 1-km cropped/twetq.grd")
  twetq.ag <- aggregate(twetq, fact = 100, fun = mean)
  writeRaster(twetq.ag, "Data files/EVs/step 2 - 100-km/twetq.grd", overwrite = T)
  plot(twetq.ag)
  
# ----------------------------------------------------------------- 
  
  
  
  
  