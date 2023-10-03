

# library ----------------------------------------------------------------------
  library(sf)
  library(geodata)
  library(mgcv)
  library(rgdal)
  library(terra)
  library(dplyr)
  library(rmapshaper)
  library(geodata)

  rm(list = ls()) 

# 1km template -----------------------------------------------------------------
temp <- rast(res = 0.0083333334,
             crs = "+init=epsg:2193 +proj=merc 
                        +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 
                        +units=m +no_defs +lat_0=0 +lon_0=173 +k=0.9996",
             xmin = 166, xmax = 179.3333,
             ymin = -48, ymax = -33)
values(temp) <- 1:ncell(temp)
plot(temp)

# outline
nz <- gadm(country = "NZ", res = 1, level = 0, path = "Data files/NZ/")
crs(nz) <-"+init=epsg:2193 +proj=merc 
              +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 
              +units=m +no_defs +lat_0=0 +lon_0=173 +k=0.9996"
plot(nz)

# simplify outline
nz2 <- terra::simplifyGeom(nz, tolerance = .05, preserveTopology = T)
nz3 <- terra::crop(nz2, temp)
plot(nz3)
nz_sh <- nz3

# NZ 1 km template
nz_1km <- mask(temp, nz3)
plot(nz_1km)

# data ------------------------------------------------------------------
# New Zealand template
  temp <- nz_1km
  
# predictor variables from nonnative families chapter 
  amt <- rast("Data files/predictor variables/raw files/wrldclim/wc2.0_bio_30s_01.tif")
  arid <- rast("Data files/predictor variables/raw files/aridity/hdr.adf")
  elev <- rast("Data files/predictor variables/raw files/elevation/GloElev_30as.asc")
  hii <- rast("Data files/predictor variables/raw files/wildareas-v3-2009-human-footprint_geo.tif")
  hii[is.na(hii)] <- 0
  #footprint(year = 2009, path = "Data files/predictor variables/raw files/human footprint") 
  # from: https://geodata.ucdavis.edu/geodata/footprint/wildareas-v3-2009-human-footprint_geo.tif
  pcoldq <- rast("Data files/predictor variables/raw files/wrldclim/wc2.0_bio_30s_19.tif")
  pwarmq <- rast("Data files/predictor variables/raw files/wrldclim/wc2.0_bio_30s_18.tif")
  ts <- rast("Data files/predictor variables/raw files/wrldclim/wc2.0_bio_30s_04.tif")
 
# crop to New Zealand 100 km ---------------------------------------------------
# land coverage of each cell
  cov <- rast(nrows = 17, ncols = 15,
               crs = "+init=epsg:2193 +proj=merc 
                        +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 
                        +units=m +no_defs +lat_0=0 +lon_0=173 +k=0.9996",
               xmin = 166.83333334, xmax = 179.33333344,
               ymin = -48, ymax = -33.83333322)
  values(cov) <- 1:ncell(cov)
  cov <- terra::rasterize(nz_sh, cov, cover = T)
  plot(cov)
  
# coverage cutoff  
  cv <- 0.25
  
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
  
  c_nz <- function(ras, fn) {
    # crop larger extent
      cropd <- terra::crop(ras, temp, mask = T, snap = "near")
      vald <- as.numeric(values(cropd, dataframe = F))
      temp2 <- temp
      values(temp2) <- vald
      names(temp2) <- "layer"
      temp3 <- terra::aggregate(temp2, fact = 100, fun = fn, na.rm = T)
      temp4 <- crop(temp3, nz_sh)
      
    # remove coastal cells with value less than cv  
      temp5 <- int(temp4)
      rem <- values(temp5$layer)[, 1] == "NaN" | values(temp5$layer)[, 1] < cv
    # set values with less than cv land cover to NA
      nlay <- ncell(temp5)
      for(i in 1:nlay) {
        v <- as.matrix(values(temp5, dataframe = T))
        v[rem] <- NA
        values(temp5) <- v
        temp6 <- crop(temp5, nz_sh)
        return(temp5)
      }
      
     
  }
  
# run function  
  amt <- c_nz(amt, "mean")
  plot(amt)
  arid <- c_nz(arid,"mean")
  plot(arid)
  th <- c_nz(elev, "sd")
  plot(elev)
  pcoldq <- c_nz(pcoldq, "mean")
  plot(pcoldq)
  pwarmq <- c_nz(pwarmq, "mean")
  plot(pwarmq)
  ts <- c_nz(ts, "mean")
  plot(ts)
  hii <- c_nz(hii, "mean")
  plot(hii)  
  hii[is.na(hii)] <- 0
  plot(hii)

# calculate land area
## remove cells with proportion land cover less than cv
  rem <- values(cov$layer)[, 1] == "NaN" | values(cov$layer)[, 1] < cv
  v <- values(cov)[, 1]
  v[rem] <- NA
  cov <- setValues(cov, v)
  
  size <- cellSize(cov, unit = "km")
  land_area <- size * cov
  land_area
  land_area[is.nan(land_area)] <- NA
  
#-------------------------------------------------------------------------------
# merge all the env variables
  env_rast <- c(amt, arid, pcoldq, pwarmq, ts, th, hii, land_area)
  env_rast
  names(env_rast) <- c("amt", "arid", "pcoldq", "pwarmq", "ts", "th", "hii", "area")

# transform and scale env variables
  ev <- values(env_rast)
  head(ev)
  
# histograms
  par(mfrow = c(3, 3))
  for(i in 1:8) {
    hist(ev[, i], main = names(env_rast)[i])
  }
  
# function to scale and/or log values
  sl <- function(x, logv = FALSE, add_to_log = 0) {
    y <- values(x)[, 1]
    if(logv) { y <- log(y + add_to_log) }
  # scale to mean zero
    y <- (y - mean(y, na.rm = T)) / sd(y, na.rm = T)
    return(setValues(x, y))
  }
  
# transform and scale
  env_rast$pwarmq <- sl(env_rast$pwarmq, logv = TRUE)
  env_rast$pcoldq <- sl(env_rast$pcoldq, logv = TRUE)
  env_rast$arid <- sl(env_rast$arid, logv = TRUE)
  env_rast$hii <- sl(env_rast$hii, logv = TRUE, add_to_log = 1)
  env_rast$th <- sl(env_rast$th, logv = TRUE)
  
# scale  
  env_rast$amt <- sl(env_rast$amt)
  env_rast$ts <- sl(env_rast$ts)
  env_rast$area <- sl(env_rast$area)
  
  ev <- values(env_rast)
  head(ev)
  
# histograms
  par(mfrow = c(3, 3))
  for(i in 1:8) {
    hist(ev[, i], main = names(env_rast)[i])
  }
  
# write environmental raster to a data file
  writeRaster(env_rast, "Data files/predictor variables/Environment/Environment raster NZ.tif", overwrite = T)
  
  
  
  
  
  