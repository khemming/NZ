

# library --------------------------------------------------
library(raster)
library(rgdal)
library(dplyr)
library(rmapshaper)
library(geodata)

rm(list = ls())

# 1 km template raster------------------------------------
temp <- raster(res = 0.008333333,
               crs = "+init=epsg:3577 +proj=aea +lat_1=-7 +lat_2=-45 +lat_0=112 +lon_0=155 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
               xmn = 110.8, xmx = 155.8,
               ymn = -46.26667, ymx = -7.1)
values(temp) <- 1:ncell(temp)
plot(temp)

# outline
aus <- gadm(country = "AUS", res = 1, level = 0, path = "Data files/Australia/")
crs(aus) <- "+init=epsg:3577 +proj=aea +lat_1=-7 +lat_2=-45 +lat_0=112 +lon_0=155 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
plot(aus)

# simplify outline
aus2 <- ms_simplify(aus, keep = .01,      # proportion of points
                    weighting = 0.7) # smoothing index
plot(aus2)


# aus 1 km template
temp2 <- mask(temp, aus2)
plot(temp2)

# save
writeRaster(temp2, "Data files/Australia/Australia 1 km.grd", overwrite = T)

writeOGR(aus2, ".", "Data files/Australia/Australia shapefile", driver = "ESRI Shapefile")