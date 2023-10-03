
# library ---------------------------------------------------------
  library(raster)
  library(rgdal)
  library(tidyverse)

  rm(list = ls())

# data files ------------------------------------------------------
# Australia outline
  aus_shp <- readOGR("Data files/Australia/Australia shapefile.shp")
  
# predictor variable rasters 
  setwd("Results/rasters/predictor variables/Australia 100 km")
  files <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", files)
  stack <- stack(files)
  names(stack) <- names
  
  setwd("C:/Users/s436862/Dropbox/NZ")  
  
# hii as template 
  aus_100 <- stack[["hii"]]
  
# 100 km Australia --------------------------------------------------------------------
# cut out Australia and provide cell values
  plot(aus_100)
  plot(aus_shp, add = T)
  temp <- setValues(aus_100, 1:ncell(aus_100))
  plot(temp)
  mask <- mask(temp, aus_100)
  aus_cells <- length(na.omit(getValues(aus_100)))
  aus_cells 
  plot(mask)
  writeRaster(mask, "Data files/Australia/Australia 1141.grd", overwrite = T)
  
# lat/long centroid coordinates of raster cells (x/y)  
  cell_list <- rasterToPoints(mask)
  cell_list <- data.frame(cell_list)
  head(cell_list)
  names(cell_list) <- c("long", "lat", "cell_id")
  
# proportion of cell covered by shapefile
  pc1 <- rasterize(aus_shp, aus_100, getCover = T)
  pc2 <- data.frame(getValues(pc1))
  
# cell_id
  cell_id <- 1:length(aus_100)
  
# cell category for all cells: ocean or land
  cell_category <- ifelse(!is.na(getValues(aus_100)), "land", "ocean")
  table(cell_category, exclude = NULL)
  
# df
  aus_df <- data.frame(cbind(cell_id, cell_category, pc2))
  colnames(aus_df) <- c("cell_id", "cell_category", "proportion_cover")
  names(aus_df)

# add in coordinates of land-cells
  aus_df2 <- left_join(aus_df, cell_list, by = "cell_id")
  slice(aus_df3, 400:410)
  
# save Australian meta data  
  write.csv(aus_df2, "Data files/Australia/Australia 2538.csv", row.names = F)
  
# add predictor variables
  stack_df <- data.frame(getValues(stack))
  aus_df3 <- cbind(aus_df2, stack_df)
  slice(aus_df3, 400:410)
  
# save with predictor varaibles
  write.csv(aus_df3, "Data files/Australia/Australia predictor varaibles 2538.csv", row.names = F)
  
# ----------------------------------------------------------------------------------