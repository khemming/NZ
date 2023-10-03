  
  
# library ---------------------------------------------------------
  library(raster)
  library(rgdal)
  library(tidyverse)
  
  rm(list = ls())
  
# data files ------------------------------------------------------
# NZ outline
  nz_shp <- readOGR("Data files/NZ/NZ shapefile.shp")
  
# predictor variable rasters 
  setwd("Results/rasters/predictor variables/NZ 100 km")
  files <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", files)
  stack <- stack(files)
  names(stack) <- names
  
  setwd("C:/Users/s436862/Dropbox/NZ")  
  
# hii as template 
  nz_100 <- stack[["hii"]]
  
# 100 km NZ --------------------------------------------------------------------
# cut out NZ and provide cell values
  plot(nz_100)
  plot(nz_shp, add = T)
  temp <- setValues(nz_100, 1:ncell(nz_100))
  plot(temp)
  mask <- mask(temp, nz_100)
  nz_cells <- length(na.omit(getValues(nz_100)))
  nz_cells 
  plot(mask)
  writeRaster(mask, "Data files/NZ/NZ 83.grd", overwrite = T)
  
# lat/long centroid coordinates of raster cells (x/y)  
  cell_list <- rasterToPoints(mask)
  cell_list <- data.frame(cell_list)
  head(cell_list)
  names(cell_list) <- c("long", "lat", "cell_id")
  
# proportion of cell covered by shapefile
  pc1 <- rasterize(nz_shp, nz_100, getCover = T)
  pc2 <- data.frame(getValues(pc1))
  
# cell_id
  cell_id <- 1:length(nz_100)
  
# cell category for all cells: ocean or land
  cell_category <- ifelse(!is.na(getValues(nz_100)), "land", "ocean")
  table(cell_category, exclude = NULL)
  
# df
  nz_df <- data.frame(cbind(cell_id, cell_category, pc2))
  colnames(nz_df) <- c("cell_id", "cell_category", "proportion_cover")
  
# add in coordinates of land-cells
  nz_df2 <- left_join(nz_df, cell_list, by = "cell_id")
  slice(nz_df2)
  
# save NZ meta data  
  write.csv(nz_df2, "Data files/NZ/NZ 288.csv", row.names = F)
  
# add predictor variables
  stack_df <- data.frame(getValues(stack))
  nz_df3 <- cbind(nz_df2, stack_df)
  slice(nz_df3, 50:60)
  
# save NZ meta data  
  write.csv(nz_df2, "Data files/NZ/NZ 288.csv", row.names = F)
  
# save with predictor varaibles
  write.csv(nz_df3, "Data files/NZ/NZ predictor varaibles 288.csv", row.names = F)
  
# ----------------------------------------------------------------------------------