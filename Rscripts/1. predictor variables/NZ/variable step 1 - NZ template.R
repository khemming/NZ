

# library --------------------------------------------------
  library(raster)
  library(rgdal)
  library(dplyr)
  library(rmapshaper)

  rm(list = ls())

# 1km template template----------------------------------------------------
  temp <- raster(res = 0.0083333334,
                 crs = "+init=epsg:2193 +proj=merc 
                        +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 
                        +units=m +no_defs +lat_0=0 +lon_0=173 +k=0.9996",
                 xmn = 166, xmx = 179.3333,
                 ymn = -48, ymx = -33)
  values(temp) <- 1:ncell(temp)
  plot(temp)
 
# outline
  nz <- getData('GADM', country='NZ', level = 0)
  crs(nz) <-"+init=epsg:2193 +proj=merc 
              +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 
              +units=m +no_defs +lat_0=0 +lon_0=173 +k=0.9996"
  plot(nz)
 
# simplify outline
  nz2 <- ms_simplify(nz, keep = .01,      # proportion of points
                         weighting = 0.7) # smoothing index
  plot(nz2)
  
# NZ 1 km template
  nz_1km <- mask(temp, nz2)
  plot(nz_1km)
  
# save
  writeRaster(nz_1km, "Data files/NZ/NZ 1 km.grd", overwrite = T)
  
  writeOGR(nz2, ".", "Data files/NZ/NZ shapefile", driver = "ESRI Shapefile")
  
# --------------------------------------------------------