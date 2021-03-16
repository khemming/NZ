
# library --------------------------------------------------------------
  library(ggsn)
  library(ggThemeAssist)
  library(raster)
  library(gplots)
  library(RColorBrewer)
  library(ggmap)
  library(rgdal)
  library(rasterVis)
  library(maptools)
  library(ggthemes)
  library(forcats)
  library(maps)
  library(tidyverse)
  
  rm(list = ls())
  
# data ------------------------------------------------
# country outlines
  setwd("C:/Users/s436862/Dropbox/NZ")
  nz <- readOGR("Data files/NZ/NZ shapefile.shp")
  plot(nz)
  
# NZ coordinates
  nz_x <- c(166, 179) 
  nz_y <- c(-48, -33)
  
# rasters
  setwd("Results/rasters/predictor variables/NZ 100 km")
  files <- list.files(pattern = ".grd")
  short_names <- gsub(pattern = ".grd", "", files)
  stack <- stack(files)
# change aridity units  
  stack[[2]] <- stack[[2]]/10000
  names(stack) <- short_names
  setwd("C:/Users/s436862/Dropbox/NZ")
  
  short_names_df <- data.frame(short_names)
  
  title_names <- c("Annual mean temperature",
                   "Aridity",
                   "Human influence index",
                   "Precipitation of the coldest quarter",
                   "Precipitation of the warmest quarter",
                   "Topographic heterogeneity",
                   "Temperature seasonality")
  
# insert units here
  unit_ls <- c("Degrees\nCelcius",
               "Index",
               "Index",
               "mm",
               "mm",
               "sd",
               "sd")
  
  names_df <- cbind(short_names, title_names)
  names_df <- data.frame(names_df)

# plot function ------------------------------------------------------------------------
# raster = raster
# save = save file name
# title = plot title
  ras_v10 <- function(raster, title, unit){
    
  # raster to spatial points dataframe 
    raster_spdf <- as(raster, "SpatialPixelsDataFrame")
    raster_df <- as.data.frame(raster_spdf)
    colnames(raster_df) <- c("value", "x", "y")
  # pretty colours
    colr <- rev(brewer.pal(11, "Spectral"))
  # save 
    save_txt <- paste0("Results/maps/predictor variables/", title, ".jpeg")
  # plot
    q <- ggplot() +  
      ggtitle(title) +
      geom_polygon(data = nz, aes(x = long, y = lat, group = group),
                   fill = "grey60") +
      geom_tile(data = raster_df, aes(x = x, y = y, fill = value)) +             
      geom_polygon(data = nz, colour = "grey1", 
                   aes(x = long, y = lat, group = group), fill = NA, size = 0.7) + 
      scale_fill_gradientn(colours = colr, 
                           #limits = c(0, 1),                             
                           #breaks = c(0, 0.5, 1),
                           space = "Lab",
                           name = unit) +
      coord_fixed(ratio = diff(nz_y)/diff(nz_x), xlim = nz_x, ylim = nz_y) +
      theme_map() +
      theme(legend.direction = "vertical",
            legend.justification = "right",
            legend.position = "right",
            legend.key.size = unit(18, "cm"),
            legend.key.width = unit(1,"cm"),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            legend.box.spacing = unit(0.01, "cm"),
            plot.title = element_text(size = 20, face = "bold", hjust = 0.1, vjust = -0.5)) +
      guides(fill = guide_colorbar(barheight = unit(5, "cm")))
    plot(q)
    
    ggsave(save_txt, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  } # finish function
  
# maps ------------------------------------------------------------------
# raster = raster
# title = plot title
  
  for (i in 1:nrow(names_df)) {
    
    ras_v10(stack[[i]],
           title_names[i],
           unit_ls[i])
    
  } 
  