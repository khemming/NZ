

# library ---------------------------------------------------------------
  library(sf)
  library(geodata)
  
  rm(list = ls()) 

  source("Rscripts/1. predictor variables/2. NZ/variable step 1 - NZ template.R")
  
# data ------------------------------------------------------------------
# New Zealand template
  temp <- nz_1km
  
### Start here
# read in these files as reproducibly as possible - at least do it for HII as that has an update
# do it for HII to begin with
# read in as rast not raster
# fiugre out CRS
# do like richrd does and join with next one ro two scripts about sacling it and making it a single raster/tif image and saving it there

  
  
# predictor variables from nonnative families chapter 
  amt <- rast("Data files/predictor variables/raw files/wrldclim/wc2.0_bio_30s_01.tif")
  arid <- rast("Data files/predictor variables/raw files/aridity/hdr.adf")
  elev <- rast("Data files/predictor variables/raw files/elevation/GloElev_30as.asc")
  hii <- rast("Data files/predictor variables/raw files/human influence index/hdr.adf")
  pcoldq <- rast("Data files/predictor variables/raw files/wrldclim/wc2.0_bio_30s_19.tif")
  pwarmq <- rast("Data files/predictor variables/raw files/wrldclim/wc2.0_bio_30s_18.tif")
  ts <- rast("Data files/predictor variables/raw files/wrldclim/wc2.0_bio_30s_04.tif")
 
# crop to New Zealand -----------------------------------------------------
# files with 1 km resolution
  
  cv <- 0.9
  
  # make this work!!
  
  # function to interpolate from gam model to fill any missing land cells
  int <- function(inrast) {
    xy <- crds(setValues(inrast, 1:ncell(inrast)))
    y <- values(inrast)[, 1]
    y[is.nan(y)] <- NA
    dat1 <- data.frame(y = y, long = xy[, 1], lat = xy[, 2])
    m1 <- gam(y ~ s(long, lat), data = dat1)
    dat1$pred <- predict(m1, newdata = data.frame(long = xy[, 1], lat = xy[, 2]))
    dat1$y[is.na(dat1$y)] <- dat1$pred[is.na(dat1$y)]
    outrast <- setValues(inrast, dat1$y)
    return(outrast)
  }
  
  c_nz <- function(ras, name, fn) {
    # crop larger extent
      cropd <- terra::crop(ras, temp, mask = T, snap = "near")
      vald <- as.numeric(values(cropd, dataframe = F))
      temp2 <- temp
      values(temp2) <- vald
      names(temp2) <- "layer"
      temp3 <- terra::aggregate(temp2, fact = 100, fun = fn, na.rm = T)
      temp4 <- crop(temp3, nz_sh)
      
    # remove coastal cells with value less than cv  
      
      rem <- values(temp4$layer)[, 1] == "NaN" | values(temp4$layer)[, 1] < cv
    # set values with less than cv land cover to NA
      nlay <- ncell(temp4)
      for(i in 1:nlay) {
        v <- as.matrix(values(temp4, dataframe = T))
        v[rem] <- NA
        values(temp4) <- v
        q <- temp4
        return(q)
      }
      
     
  }
  
# run function  
  amt <- c_nz(amt, "amt", "mean")
  plot(amt)
  arid <- c_nz(arid,   "arid", "mean")
  plot(arid)
  elev <- c_nz(elev,   "elev", "sd")
  plot(elev)
  pcoldq <- c_nz(pcoldq, "pcoldq")
  plot(pcoldq)
  pwarmq <- c_nz(pwarmq, "pwarmq")
  plot(pwarmq)
  ts <- c_nz(ts,     "ts")
  plot(ts)
  hii <- c_nz(hii, "hii")
  plot(hii)  

