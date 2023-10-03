
  library(raster)
  library(terra)
  library(sf)
  library(mgcv)
  library(sp)
  library(geodata)
  
# run this once, then use next section to load NZ shapefile --------------------
# NZ 
# nz <- raster::getData('GADM', country='NZ', level = 0)
  
# or, download NZ shapefile from source:
# https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_NZL_0_sp.rds
  # nz <- readRDS("Data files/NZ/gadm36_NZL_0_sp.rds")  
  # head(nz)
# plot(nz)
 
# simplify outline
  # nz2 <- ms_simplify(nz, keep = .01,      # proportion of points
  #                        weighting = 0.7) # smoothing index
  # plot(nz2)
  # 
  # writeOGR(nz2, ".", "Data files/NZ/NZ shapefile", driver = "ESRI Shapefile")
  
# load NZ ----------------------------------------------------------------------
  nz_s <- st_read("Data files/NZ/NZ shapefile.shp")
  
# recast the multipolygon as individual polygons; ignore warning
  sep <- st_cast(nz_s, "MULTIPOLYGON")
  head(sep)

# extract the polygon for Nrth Isl, Sht Isl, Stuart Isl
names(sep)

# https://r-spatial.github.io/sf/articles/sf1.html

# reset CRS to match raster
  main <- st_transform(main, crs = 4326) # same as Aus
  plot(main)  
  
# convert to spatial vector
  mainvec <- vect(main)

#-------------------------------------------------------------------------------
# temp raster with required characteristics
  temp_rast <- rast(res = 5/6,
                 xmin = 166, xmax = 179.3333,
                 ymin = -48, ymax = -33)
  
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
  
# function to take a raster, aggregate to larger scale and clip to mainland NZ
  cv <- 0.25 # try a lower threshold for NZ

  aggr <- function(x) {
    # number of layers
    nlay <- length(names(x))
    # crop to NZ
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
  
  wc2 <- aggr(rast("Data files/predictor variables/wc2-5/bio4.bil"))
  names(wc2) <- "ts"
  
  wc3 <- aggr(rast("Data files/predictor variables/wc2-5/bio18.bil"))
  names(wc3) <- "pwarmq"
  
  wc4 <- aggr(rast("Data files/predictor variables/wc2-5/bio19.bil"))
  names(wc4) <- "pcoldq"
  
# aridity
  arid <- aggr(rast("Data files/predictor variables/Australia 1 km/arid.grd")) 
  # ignore warning
  names(arid) <- "arid" 

# human influence index  
  hii <- aggr(rast("Data files/predictor variables/Australia 1 km/hii.grd")) # ignore warning
  plot(hii)
  names(hii) <- "hii"

  
# elevation  
  #alt_raw <- elevation_3s(lat = c-45, lon = 110, path = "./Data files/predictor variables/wc2-5")
  ## use this when raster depreciated
  ## but figure out how to get appropriate lat long
    alt_raw <- raster::getData("alt", country = "AUS", path = "./Data files/predictor variables/wc2-5")
  
# aggregate to get topographic heterogeneity
  th_raw <- raster::aggregate(alt_raw, fact = 50, fun = sd)
  th <- aggr(rast(th_raw))
  names(th) <- "th"
  
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
  env_rast <- c(wc1, wc2, wc3, wc4, arid, hii, th, land_area)
  env_rast
  names(env_rast)
  
#-------------------------------------------------------------------------------------
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
  for(i in 1:8) {
    hist(ev[, i], main = names(env_rast)[i])
  }
  
# write environmental raster to a data file
  writeRaster(env_rast, "Data files/predictor variables/Environment/Environment raster NZ.tif", overwrite = T)
  
  
  
  
  