
########################################################################################
# step one: crop and reproject to New Zealand at 1 km scale
########################################################################################
#

# scope -------------------------------------------------
# transorming the projection (CRS) to even-sized cells
# and from the original, global extents to something that encompasses New Zealand

# library --------------------------------------------------------------
  library(ggmap)
  library(raster)
  library(rgdal)
 
  rm(list = ls())
  
# 1-km raster functions -------------------------------------------------
# notes: function requirements -------------------------------------------------------------
# raw.raster  = sourced raster layer, somewhere from the deep dark depths of the internet, loaded as a '.grd' R-raster file
# mask.layer  = this layer outlines the general extent by which we want to plot our region, andm ore importantly, the cells of our region of interest (i.e. masks islands, other countries)
# raster.name = short name for raster that will be with it for life
# save        = save file path and raster name, relative to wd()
  
# New Zealand 1 km function  -----------------------------------------------------------------------
   nz_1km <- function(raw.raster, raster.name, mask.layer, save) 
    {
    # set things up
     projection(raw.raster) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
     crop.extent <- extent(mask.layer)
     names(raw.raster) <- raster.name
     
    # crop larger extent
     crop.raster <- crop(raw.raster, crop.extent)
    
    # mask offshore values
     masked.raster <- mask(crop.raster, mask.layer)
   
    # for some reason extents are slightly different
     extent(masked.raster) <- crop.extent
     
     plot(masked.raster)
     
    # save
     writeRaster(masked.raster, save, overwrite = T)
     
     return(masked.raster)
      
     } # fun end
# test 1km function -------------------------------------------------------------------------  
  raw.raster <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/CGIR CSI Aridity and Evaporation/Global Aridity - Annual/AI_annual/ai_yr/hdr.adf")/10000
  raster.name <- "arid"
  mask.layer <- raster("Data files/New Zealand/nz 1-km.grd")
  save <- "Data files/EVs/step 1 - 1-km cropped/arid.grd"

  nz_1km(raw.raster, raster.name, mask.layer, save)
 
  raster(save)
# --------------------------------------------------------------------------------------
# New Zealand 1 km function  for HII -----------------------------------------------------------------------
  nz_1km_hii <- function(raw.raster, raster.name, mask.layer, save) 
  {
    # set things up
    projection(raw.raster) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    crop.extent <- extent(mask.layer)
    names(raw.raster) <- raster.name
    
    # crop larger extent
    crop.raster <- crop(raw.raster, crop.extent)
    
    # for some reason extents are slightly different
    extent(crop.raster) <- crop.extent
    
    # mask offshore values
    masked.raster <- mask(crop.raster, mask.layer)
    
    # for some reason extents are still slightly different
    extent(masked.raster) <- crop.extent
    
    plot(masked.raster)
    
    # save
    writeRaster(masked.raster, save, overwrite = T)
    
    return(masked.raster)
    
  } # fun end
  
# tested with hii --------------------  
# ----------------------------------------------------------------------------------------  
# New Zealand 1 km function for >1 km res rasters --------------------------------------------------
# for rasters which will be the opposite of aggregated (disaggregated?) back to 1 km for cropping purposes
  nz_1km_disag <- function(raw.raster, raster.name, mask.layer, save) 
  {
  # set things up
    projection(raw.raster) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    crop.extent <- extent(mask.layer)
    names(raw.raster) <- raster.name
    
  # crop larger extent
    crop.raster <- crop(raw.raster, crop.extent)
    crop.raster
    plot(crop.raster)
    
  # reproject to 1 km res
    repro.raster <- projectRaster(crop.raster, mask.layer, res = 0.008333334, crs = "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", method="ngb")
    repro.raster
    plot(repro.raster)
    
  # mask offshore values
    masked.raster <- mask(repro.raster, mask.layer)
   
  # for some reason extents are slightly different
    extent(masked.raster) <- crop.extent
    
    plot(masked.raster)
    
  # save
    writeRaster(masked.raster, save, overwrite = T)
    
    return(masked.raster)
    
  } # fun end
  
# test 1km disag --------------------------------------------------------------------
#   st <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Harmonised World Soil Database/WEBBSOIL_548/WEBBSOIL_548/data/wrtext.asc")
#   save <- "Data files/EVs/step 1 - 1-km cropped/st.grd"  
# 
#   raw.raster <- st
#   
# # set things up
#   projection(raw.raster) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#   crop.extent <- extent(mask.layer)
#   names(raw.raster) <- raster.name
#   
#   # crop larger extent
#   crop.raster <- crop(raw.raster, crop.extent)
#   crop.raster
#   plot(crop.raster)
#   
#   # reproject to 1 km res
#   repro.raster <- projectRaster(crop.raster, mask.layer, res = 0.008333334, crs = "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", method="ngb")
#   repro.raster
#   plot(repro.raster)
#   
#   
#   # mask offshore values
#   masked.raster <- mask(repro.raster, mask.layer)
#   masked.raster
#   plot(masked.raster)
#   
#   # for some reason extents are slightly different
#   extent(masked.raster) <- crop.extent
  
  
  
#########################################################################
# run functions
#########################################################################  
# requirements --------------------------------------------------------
  mask.layer <- raster("Data files/New Zealand/nz 1-km.grd")
  
  
# (1) aridity  --------------------------------------------------------
# note: dividied by 10,000 from original raster values
  arid <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/CGIR CSI Aridity and Evaporation/Global Aridity - Annual/AI_annual/ai_yr/hdr.adf")/10000
  save <- "Data files/EVs/step 1 - 1-km cropped/arid.grd"
  
  nz_1km(arid, "arid", mask.layer, save)
  
# (2) potential evapo-transpiration (pet)-------------------------------------------
  pet <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/CGIR CSI Aridity and Evaporation/Global PET - Annual/PET_he_annual/pet_he_yr/hdr.adf")
  save <- "Data files/EVs/step 1 - 1-km cropped/pet.grd"
  
  nz_1km(pet, "pet", mask.layer, save)
 
# (3) elevation -------------------------------------------
  elev <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/CGIR Elevation/Elevation 30 sec/GloElev_30as.asc")
  save <- "Data files/EVs/step 1 - 1-km cropped/elev.grd"
  
  nz_1km(elev, "elev", mask.layer, save)
  
# (4) Annual Mean Temperature (AMT) -------------------------------------------
  amt <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Worldclim/wc2.0_bio_30s_01.tif") 
  save <- "Data files/EVs/step 1 - 1-km cropped/amt.grd"
  
  nz_1km(amt,"amt", mask.layer, save)

# (5) Mean Diurnal Range (Mean of monthly (max temp - min temp)) (MDR) -----------------------------
  mdr <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Worldclim/wc2.0_bio_30s_02.tif") 
  save <- "Data files/EVs/step 1 - 1-km cropped/mdr.grd"
  
  nz_1km(mdr, "mdr", mask.layer, save)

# (6) Isothermality (BIO2/BIO7) (* 1) (ISO) -------------------------------------------
  iso <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Worldclim/wc2.0_bio_30s_03.tif") 
  save <- "Data files/EVs/step 1 - 1-km cropped/iso.grd"
  
  nz_1km(iso, "iso", mask.layer, save)
  
# (7) Temperature Seasonality (standard deviation *100) (Note I divided by 100) (TS)  ----------------------
  ts <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Worldclim/wc2.0_bio_30s_04.tif")/100
  save <- "Data files/EVs/step 1 - 1-km cropped/ts.grd"
  
  nz_1km(ts, "ts", mask.layer, save)
  
# (8) Max Temperature of Warmest Month (twarmm) ------------------------------------------
  twarmm <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Worldclim/wc2.0_bio_30s_05.tif") 
  save <- "Data files/EVs/step 1 - 1-km cropped/twarmm.grd"
  
  nz_1km(twarmm, "twarmm", mask.layer, save)
  
# (9) Min Temperature of Coldest Month (tcoldm)   ----------------------------------------
  tcoldm <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Worldclim/wc2.0_bio_30s_06.tif") 
  save <- "Data files/EVs/step 1 - 1-km cropped/tcoldm.grd"
  
  nz_1km(tcoldm, "tcoldm", mask.layer, save)
  
# (10) Temperature Annual Range (BIO5-BIO6) (TAR)  ------------------------------------
  tar <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Worldclim/wc2.0_bio_30s_07.tif") 
  save <- "Data files/EVs/step 1 - 1-km cropped/tar.grd"
  
  nz_1km(tar, "tar", mask.layer, save)
  
# (11) Mean Temperature of Wettest Quarter (TWETQ)   ------------------------------------
  twetq <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Worldclim/wc2.0_bio_30s_08.tif") 
  save <- "Data files/EVs/step 1 - 1-km cropped/twetq.grd"
  
  nz_1km(twetq, "twetq", mask.layer, save)
  
# (12) Mean Temperature of Driest Quarter (TDRYQ)  ------------------------------------------- 
  tdryq <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Worldclim/wc2.0_bio_30s_09.tif") 
  save <- "Data files/EVs/step 1 - 1-km cropped/tdryq.grd"
  
  nz_1km(tdryq, "tdryq", mask.layer, save) 
  
# (13) Mean Temperature of Warmest Quarter (TWARMQ)  ------------------------------------------- 
  twarmq <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Worldclim/wc2.0_bio_30s_10.tif")
  save <- "Data files/EVs/step 1 - 1-km cropped/twarmq.grd"
  
  nz_1km(twarmq, "twarmq", mask.layer, save)
  
# (14) Mean Temperature of Coldest Quarter (TCOLDQ)   ------------------------------------------- 
  tcoldq <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Worldclim/wc2.0_bio_30s_11.tif")
  save <- "Data files/EVs/step 1 - 1-km cropped/tcoldq.grd"
  
  nz_1km(tcoldq, "tcoldq", mask.layer, save)
  
# (15) Annual Precipitation (AP)    -------------------------------------------
  ap <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Worldclim/wc2.0_bio_30s_12.tif")
  save <- "Data files/EVs/step 1 - 1-km cropped/ap.grd"
  
  nz_1km(ap, "ap", mask.layer, save)  
  
# (16) Precipitation of Wettest Month (PWETM)   -----------------------------------------
  pwetm <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Worldclim/wc2.0_bio_30s_13.tif")
  save <- "Data files/EVs/step 1 - 1-km cropped/pwetm.grd"
  
  nz_1km(pwetm, "pwetm", mask.layer, save)  
  
# (17) Precipitation of Driest Month (PDRYM)   -------------------------------------------
  pdrym <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Worldclim/wc2.0_bio_30s_14.tif")
  save <- "Data files/EVs/step 1 - 1-km cropped/pdrym.grd"
  
  nz_1km(pdrym, "pdrym", mask.layer, save)
  
# (18) Precipitation Seasonality (Coefficient of Variation) (PS) ------------------------
  ps <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Worldclim/wc2.0_bio_30s_15.tif")
  save <- "Data files/EVs/step 1 - 1-km cropped/ps.grd"
  
  nz_1km(ps, "ps", mask.layer, save)     
  
# (19) Precipitation of Wettest Quarter (PWETQ) ------------------------------------------
  pwetq <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Worldclim/wc2.0_bio_30s_16.tif")
  save <- "Data files/EVs/step 1 - 1-km cropped/pwetq.grd"
  
  nz_1km(pwetq, "pwetq", mask.layer, save)
  
# (20) Precipitation of Driest Quarter (PDRYQ) -------------------------------------------
  pdryq <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Worldclim/wc2.0_bio_30s_17.tif")
  save <- "Data files/EVs/step 1 - 1-km cropped/pdrym.grd"
  
  nz_1km(pdrym, "pdrym", mask.layer, save)
  
# (21) Precipitation of Warmest Quarter (PWARMQ)   -------------------------------------------
  pwarmq <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Worldclim/wc2.0_bio_30s_18.tif")
  save <- "Data files/EVs/step 1 - 1-km cropped/pwarmq.grd"
  
  nz_1km(pwarmq, "pwarmq", mask.layer, save)

# (22) Precipitation of Coldest Quarter (PCOLDQ)   -------------------------------------------
  pcoldq <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Worldclim/wc2.0_bio_30s_19.tif")
  save <- "Data files/EVs/step 1 - 1-km cropped/pcoldq.grd"
  
  nz_1km(pcoldq, "pcoldq", mask.layer, save)
  
# (23) Human influence index --------------------------------------------------------
  hii <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/The Human Influence Index (HII)/hii-global-geo-grid/hii_v2geo/hdr.adf")
  save <- "Data files/EVs/step 1 - 1-km cropped/hii.grd"
  
  nz_1km_hii(hii, "hii", mask.layer, save)

# (24) potential storage of water derived from soil texture (mm)) (st) -------------------
  st <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Harmonised World Soil Database/WEBBSOIL_548/WEBBSOIL_548/data/wrtext.asc")
  save <- "Data files/EVs/step 1 - 1-km cropped/st.grd"
  
  nz_1km_disag(st, "st", mask.layer, save)
 
  
# (25) potential storage of water in the root zone (mm) (rz) -------------------------------------------
  rz <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Harmonised World Soil Database/WEBBSOIL_548/WEBBSOIL_548/data/wrroot.asc")
  save <- "Data files/EVs/step 1 - 1-km cropped/rz.grd"
  
  nz_1km_disag(rz, "rz", mask.layer, save)
  
# (26) potential storage of water in the soil profile (mm) (sp) -------------------------------------------
  sp <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Harmonised World Soil Database/WEBBSOIL_548/WEBBSOIL_548/data/wrprof.asc")
  save <- "Data files/EVs/step 1 - 1-km cropped/sp.grd"
  
  nz_1km_disag(sp, "sp", mask.layer, save)

# (27) plant available [soil] water capacity  -------------------------------------------
# Australian extent -- ignore
  
  # pawc <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Plant water capacity/PAWC_1m/pawc_1m/hdr.adf")
  # save <- "Data files/EVs/step 1 - 1-km cropped/pawc.grd"
  # 
  # nz_1km(pawc, "pawc", mask.layer, save)
 
# (28) Plant extractable [soil] water capacity -------------------------------------------
  # again, dunno where I put og rasters (think I did them in ARC GIS?)
  # so I am doing it here
  pewc <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/EVs/Plant water capacity/DUNNESOIL_545/dunne_soil.dat")
  save <- "Data files/EVs/step 1 - 1-km cropped/pewc.grd"
  
  nz_1km_disag(pewc, "pewc", mask.layer, save)
  
# --------------------------------------------------------------------------------
  

