
########################################################
# New Zealand 100-km raster and dataframe
########################################################

# data frame: including cell_id, ocean/land category, proportion of cell covered (i.e. land %)

# library
  library(raster)
  library(rgdal)
  
rm(list = ls())
 
# data
  nz.shp <- readOGR("Data files/New Zealand/nz-coastlines-and-islands-polygons-topo-150k.shp")
  projection(nz.shp) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
# 1-km resolution ----------------------------------------------------------------
  arid <- raster("Data files/EVs/step 1 - 1-km cropped/arid.grd") # raster already cropped to right size
  plot(arid)
  
  nz <- arid
  nz.val <- setValues(nz, 1:ncell(nz))
  
  nz.masked <- mask(nz.val, nz)
  plot(nz.masked)
  
# save
  writeRaster(nz.masked, "Data files/New Zealand/nz 1-km.grd", overwrite = T)

# 100-km resolution raster and data frame ----------------------------------------
  nz.masked <- raster("Data files/New Zealand/nz 1-km.grd") 
  plot(nz.masked)
  plot(nz.shp, add = T)
  nz.100 <- aggregate(nz.masked, fact = 100, fun = mean)
  plot(nz.100)
  plot(nz.shp, add = T)
  
# proportion of cell covered by shapefile
  prop.cover <- rasterize(nz.shp, nz.100, getCover = T)
  prop.cover <- data.frame(getValues(prop.cover))
  prop.cover <- prop.cover/100
  colnames(prop.cover) <- "prop.cover"
  
  table(prop.cover)  

# cell_id
  cell.id <- 1:nrow(prop.cover)
  
# cell category: ocean or land
  cell.category <- data.frame(ifelse(prop.cover == 0, "ocean", "land"))
  names(cell.category) <- "cell.category"
  head(cell.category)  

# df
  nz.cells.100km <- data.frame(cbind(cell.id, cell.category, prop.cover))

# save
  write.csv(nz.cells.100km, "Data files/New Zealand/nz 100-km.csv", row.names = F)
  
# raster template
  nz.100.template <- setValues(nz.100, 1:ncell(nz.100))
  plot(nz.100.template)
  nz.100.masked <- mask(nz.100.template, nz.100)
  plot(nz.100.masked)
  writeRaster(nz.100.masked, "Data files/New Zealand/nz 100-km.grd", overwrite = T)
  