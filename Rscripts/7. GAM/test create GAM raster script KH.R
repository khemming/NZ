  library(raster)
  library(terra)
  library(sf)
  library(mgcv)
  library(sp)
  
#-------------------------------------------------------------------------------
# read in the Australia 2021 digital boundary shapefile downloaded from:
# https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files
  
  aus <- st_read("Data files/Australia/AUS_2021_AUST_SHP_GDA2020/AUS_2021_AUST_GDA2020.shp")
  
# recast the multipolygon as individual polygons
  sep <- st_cast(aus, "POLYGON") # ignore warning
  head(sep)
  
# extract the polygon for mainland Australia, excluding all the island polygons
  main <- sep[c(6412, 6374), ] # how does this work?
  main
  
# reset CRS to match raster
  main <- st_transform(main, crs = 4326)
  #plot(main$geometry)  
  
# convert to spatial vector
  mainvec <- vect(main)

#-------------------------------------------------------------------------------
  # template raster of desired size
  temp_rast <- rast(xmin = 110, xmax = 155, ymin = -45, ymax = -10, resolution = 5/6)
  
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
  
  # start here
  wc1 <- aggr(rast("predictor variables/wc2-5/bio1.bil"))
  names(wc1) <- "amt"
  
  wc2 <- aggr(rast("predictor variables/wc2-5/bio4.bil"))
  names(wc2) <- "ts"