
  library(raster)
  library(terra)
  library(sf)
  library(mgcv)
  
#-------------------------------------------------------------------------------
# read in the Australia 2021 digital boundary shapefile downloaded from:
# https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files
  
  aus <- st_read("Data files/Australia/AUS_2021_AUST_SHP_GDA2020/AUS_2021_AUST_GDA2020.shp")
  
# recast the multipolygon as individual polygons
  sep <- st_cast(aus, "POLYGON")
  head(sep)
  
# extract the polygon for mainland Australia, excluding all the island polygons
  main <- sep[c(6412, 6374), ] 
  main
  
# reset CRS to match raster
  main <- st_transform(main, crs = 4326)
  plot(main$geometry)  
  
# convert to spatial vector
  mainvec <- vect(main)

#-------------------------------------------------------------------------------
# land coverage of each cell
  cov <- rasterize(mainvec, temp_rast, cover = T)
  
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
  
# function to take a raster, aggregate to larger scale and clip to mainland Australia
  cv <- 0.5

  aggr <- function(x) {
    # number of layers
    nlay <- length(names(x))
    # crop to Australia
    x <- crop(x, mainvec)
    # resample to get aggregated raster values
    newx <- resample(x, temp_rast, method = "bilinear")
    # interpolate to fill in any missing values
    newx <- int(newx)
    # remove cells with proportion land cover less than cv
    rem <- values(cov$layer)[, 1] == "NaN" | values(cov$layer)[, 1] < cv
    # set values with less than cv land cover to NA
    for(i in 1:nlay) {
      v <- values(newx[[i]])[, 1]
      v[rem] <- NA
      newx[[i]] <- setValues(newx[[i]], v)
    }
    return(newx)
  }

#-------------------------------------------------------------------------------
# World clim data  
  wc1 <- aggr(rast("Data files/predictor variables/wc2-5/bio1.bil"))
  names(wc1) <- "amt"
  
  wc2 <- aggr(rast("wc2-5/bio4.bil"))
  names(wc2) <- "ts"
  
  wc3 <- aggr(rast("wc2-5/bio18.bil"))
  names(wc3) <- "pwarmq"
  
  wc4 <- aggr(rast("wc2-5/bio19.bil"))
  names(wc4) <- "pcoldq"
  
# aridity
  arid <- aggr(rast("Data files/Environment/aridity/hdr.adf"))
  names(arid) <- "arid"
  
# potential water capacity
  pewc <- aggr(rast("Data files/Environment/potential water capacity/dunne_soil.dat"))
  names(pewc) <- "pewc"

# human influence index  
  hii <- aggr(rast("Data files/Environment/human influence index/hdr.adf"))
  names(hii) <- "hii"

# elevation  
  alt_raw <- getData("alt", country = "AUS")

# aggregate to get topographic heterogeneity
  th_raw <- raster::aggregate(alt_raw, fact = 50, fun = sd)
  th <- aggr(rast(th_raw))
  names(th) <- "th"
  
# calculate land area
  # remove cells with proportion land cover less than cv
  rem <- values(cov$layer)[, 1] == "NaN" | values(cov$layer)[, 1] < cv
  v <- values(cov)[, 1]
  v[rem] <- NA
  cov <- setValues(cov, v)
  
  size <- cellSize(cov, unit = "km")
  land_area <- size * cov
  land_area
  land_area[is.nan(land_area)] <- NA
  
#-------------------------------------------------------------------------------------
# merge all the env variables
  env_rast <- c(wc1, wc2, wc3, wc4, arid, pewc, hii, th, land_area)
  env_rast
  names(env_rast)
  
#-------------------------------------------------------------------------------------
# transform and scale env variables
  ev <- values(env_rast)
  head(ev)
  
# histograms
  par(mfrow = c(3, 3))
  for(i in 1:9) {
    hist(ev[, i], main = names(env_rast)[i])
  }
  
# function to scale and/or log values
  sl <- function(x, logv = FALSE, add_to_log = 0) {
    y <- values(x)[, 1]
    if(logv) { y <- log(y + add_to_log) }
    #scale to mean zero
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
  for(i in 1:9) {
    hist(ev[, i], main = names(env_rast)[i])
  }
  
# write environmental raster to a data file
  writeRaster(env_rast, "Data files/Environment/Environment raster.tif", overwrite = T)
  
  
  
  
  