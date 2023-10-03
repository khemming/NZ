#########################################################
# absolute richness maps
#########################################################

# library --------------------------------------------------------------
  library(gplots)
  library(RColorBrewer)
  library(raster)
  library(ggmap)
  library(rgdal)
  library(rasterVis)
  library(maptools)
  library(ggthemes)
  library(ggThemeAssist)
  library(tidyverse)
  library(forcats)
  library(maps)
  library(scales)

  rm(list = ls())

# data ------------------------------------------------
# shape file
  nz <- readOGR("Data files/New Zealand/New Zealand shapefile.shp")
  plot(nz)
  
# rounding function
  mround <- function(x,base){
    base*round(x/base)
  }
  
# NZ
  setwd("Results/rasters/NZ species richness")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)

# Aus    
  setwd("C:/Users/s436862/Dropbox/NZ-biogeography/Results/rasters/Aus species richness")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)

  setwd("C:/Users/s436862/Dropbox/NZ-biogeography")

# scale legend to: nat C3 C3, breaks: 20, 40 60 
# notes --------------------------------------------------------
# highest species richness
  cellStats(nz_c3_native, stat = 'max', na.rm = T) # 69
  cellStats(nz_c4_native, stat = 'max', na.rm = T) # 34
  
  cellStats(nz_c3_nonnative, stat = 'max', na.rm = T) # 60
  cellStats(nz_c4_nonnative, stat = 'max', na.rm = T) # 1 
  
  cellStats(predicted_nz_C3, stat = 'max', na.rm = T) # 63
  cellStats(predicted_nz_C4, stat = 'max', na.rm = T) # 36
  
  cellStats(predicted_aus_C3, stat = 'max', na.rm = T) # 18
  cellStats(predicted_aus_C4, stat = 'max', na.rm = T) # 64
  
# pretty breaks = add in pretty_breaks(n = 4)
  ras_v7(nz_c3_native,
         "Native NZ C3",
         c(0, cellStats(nz_c3_native, stat = 'max', na.rm = T)))
  
  
  
# raw maps (v7) ---------------------------------------------------------
  ras_v7 <- function(raster, title, limit_set){
    
  # raster to spatial points dataframe 
    raster_spdf <- as(raster, "SpatialPixelsDataFrame")
    raster_df <- as.data.frame(raster_spdf)
    colnames(raster_df) <- c("value", "x", "y")
  # pretty colours
    colr <- rev(brewer.pal(11, "Spectral"))
  # save 
    save_txt <- paste0("Results/maps/iNEXT", title, ".jpeg")
  # plot
    q <- ggplot() +  
      ggtitle(title) +
      geom_polygon(data = nz, aes(x = long, y = lat, group = group), 
                   fill = "grey60") +                                               geom_tile(data = raster_df, aes(x = x, y = y, fill = value)) +             
      geom_polygon(data = nz, colour = "grey1", 
                   aes(x = long, y = lat, group = group), fill = NA, size = 1) + 
      scale_fill_gradientn(colours = colr, 
                           limits = limit_set,                             
                           breaks = c(20, 40, 60),
                           space = "Lab",
                           name = "Species\nrichness") +
      coord_fixed(xlim = c(166, 179), ylim = c(-48, -33), expand = F) +
      theme_map() +
      theme(legend.direction = "vertical",
            legend.justification = "right",
            legend.position = "right",
            legend.key.size = unit(12, "cm"),
            legend.key.width = unit(1,"cm"),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.box.spacing = unit(0.1, "cm"),
            plot.title = element_text(size = 20, face = "bold", hjust = 0.1, vjust = -0.5)) +
      guides(fill = guide_colorbar(barheight = unit(5, "cm"))) 
    plot(q)
    ggsave(save_txt, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
} # finish function
  
# maps -----------------------------------------------------------------  
# raster, title, limit_set

# NZ C3 native
  ras_v7(nz_c3_native,
         "Native NZ C3",
         c(0, cellStats(nz_c3_native, stat = 'max', na.rm = T)))
  
# NZ C4 native
  ras_v7(nz_c4_native,
         "Native NZ C4",
         c(0, cellStats(nz_c3_native, stat = 'max', na.rm = T)))
  
# NZ C3 non-native
  ras_v7(nz_c3_nonnative,
         "Non-native NZ C3",
         c(0, cellStats(nz_c3_native, stat = 'max', na.rm = T)))
  
# NZ C4 non-native C4: can't plot one point
  ras_v7(nz_c4_nonnative,
         "Non-native NZ C4",
         c(0, cellStats(nz_c3_native, stat = 'max', na.rm = T)))

# predicted NZ native C3
  ras_v7(predicted_nz_C3,
         "Predicted NZ C3",
         c(0, cellStats(nz_c3_native, stat = 'max', na.rm = T)))

# predicted NZ native C4
  ras_v7(predicted_nz_C4,
         "Predicted NZ C4",
         c(0, cellStats(nz_c3_native, stat = 'max', na.rm = T)))
  

# predicted Aus native C3
  ras_v7(predicted_aus_C3,
         "Predicted Aus C3",
         c(0, cellStats(nz_c3_native, stat = 'max', na.rm = T)))
  
# predicted Aus native C4
  ras_v7(predicted_aus_C4,
         "Predicted Aus C4",
         c(0, cellStats(nz_c3_native, stat = 'max', na.rm = T)))
  
# -----------------------------------------------------------------------------      