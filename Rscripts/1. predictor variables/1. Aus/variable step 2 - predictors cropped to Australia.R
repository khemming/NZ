

# library ---------------------------------------------------------------
  library(raster)
  library(rgdal)
 
  rm(list = ls())
  
# data ------------------------------------------------------------------
# australia template
  temp <- raster("Data files/Australia/Australia 1 km.grd")
  
# predictor variables from nonnative families chapter
  setwd("C:/Users/s436862/Dropbox/NNF")
  amt <- raster("Data files/predictor variables/raw files/wrldclim/wc2.0_bio_30s_01.tif")
  arid <- raster("Data files/predictor variables/raw files/aridity/hdr.adf")
  elev <- raster("Data files/predictor variables/raw files/elevation/GloElev_30as.asc")
  hii <- raster("Data files/predictor variables/raw files/human influence index/hdr.adf")
  pcoldq <- raster("Data files/predictor variables/raw files/wrldclim/wc2.0_bio_30s_19.tif")
  pwarmq <- raster("Data files/predictor variables/raw files/wrldclim/wc2.0_bio_30s_18.tif")
  ts <- raster("Data files/predictor variables/raw files/wrldclim/wc2.0_bio_30s_04.tif")
  
  setwd("C:/Users/s436862/Dropbox/NZ")
  
# crop to Australia -----------------------------------------------------
# files with 1 km resolution --------------------------------------------
  crop_1km_fun <- function(raw_raster, raster_name) {
    # set things up
      mask <- temp
      projection(raw_raster) <- "+init=epsg:3577 +proj=aea +lat_1=-7 +lat_2=-45 +lat_0=112 +lon_0=155 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
      crop_extent <- extent(mask)
      names(raw_raster) <- raster_name
      
    # crop larger extent
      crop_raster <- crop(raw_raster, crop_extent)
      
    # mask offshore values
      masked_raster <- mask(crop_raster, mask)
      
    # for some reason extents are slightly different
      extent(masked_raster) <- crop_extent
      
    # save
      save <- paste0("Data files/predictor variables/Australia 1 km/", raster_name)
      writeRaster(masked_raster, save, overwrite = T)
      
  }
  
# run function  
  crop_1km_fun(amt,    "amt")
  crop_1km_fun(arid,   "arid")
  crop_1km_fun(elev,   "elev")
  crop_1km_fun(pcoldq, "pcoldq")
  crop_1km_fun(pwarmq, "pwarmq")
  crop_1km_fun(ts,     "ts")
  
# crop to 1 km for HII ---------------------------------------------------------------------------
# extent's not the same after initial cropping
  crop_hii_fun <- function(raw_raster, raster_name) {
  # set things up
    mask <- temp
    projection(raw_raster) <- "+init=epsg:3577 +proj=aea +lat_1=-7 +lat_2=-45 +lat_0=112 +lon_0=155 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
    crop_extent <- extent(mask)
    
  # crop larger extent
    crop_raster <- crop(raw_raster, crop_extent)
    
  # for some reason extents are slightly different
    extent(crop_raster) <- crop_extent   
    
  # mask offshore values
    masked_raster <- mask(crop_raster, mask)
    
  # for some reason extents are still slightly different
    extent(masked_raster) <- crop_extent
    names(masked_raster) <- raster_name
    
  # save
    save <- paste0("Data files/predictor variables/Australia 1 km/", raster_name)
    writeRaster(masked_raster, save, overwrite = T)
    
  }  
  
# run function
  crop_hii_fun(hii, "hii")
  
# -------------------------------------------------------------