

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
  aus <- readOGR("Data files/Australia/Australia shapefile.shp")
  plot(aus)

# predicted richness
  setwd("C:/Users/s436862/Dropbox/NZ/Results/rasters/predicted")
  files_ls <- list.files(pattern = ".grd")
  
  aus_ls <- Filter(function(x) grepl("_to_Aus", x), files_ls)
  aus_names <- gsub(pattern = "\\.grd$", "", aus_ls)
  aus_pred <- stack(aus_ls)
  names(aus_pred) <- aus_names
  
  setwd("C:/Users/s436862/Dropbox/NZ")
  
# Aus coordinates
  aus_x <- c(112, 155) 
  aus_y <- c(-45, -7)
 
# species richness scale bars -----------------------------------------------------------
# in raw richness
  j <- vector()
  
  for (i in 1:nlayers(aus_pred)){
    j[i] <- ceiling(exp(cellStats(aus_pred[[i]], "max", na.rm = T)))
  }
  j 
  aus_leg_lab <- list(c(65, 30, 10, 5, 1),   # 61
                      c(80, 40, 20, 5, 1),   # 78
                      c(30, 20, 10, 5, 1),   # 30
                      c(40, 20, 10, 5, 1))   # 40
  
# maps - log richness ---------------------------------------------
  ras_v10_l <- function(raster, title, sr_breaks, legend_title){
    
  # scale breaks and labels
    log_sr <- log(sr_breaks)
    
  # spatial points dataframe 
    raster_spdf <- as(raster, "SpatialPixelsDataFrame")
    raster_df <- as.data.frame(raster_spdf)
    colnames(raster_df) <- c("value", "x", "y")
    
  # km scale bar  
    km_scale <- raster_df %>% rename(lat = y, long = x)
    km_pos <- as.vector(data.frame(x = 150, 
                                   y = -30))
    
  # colours
    colr <- rev(brewer.pal(11, "Spectral"))
    
  # plot
    q <- ggplot() +  
      ggtitle(title) +
      geom_polygon(data = aus, aes(x = long, y = lat, group = group),
                   fill = "grey60") +
      geom_tile(data = raster_df, aes(x = x, y = y, fill = value)) +             
      geom_polygon(data = aus, colour = "grey1", 
                   aes(x = long, y = lat, group = group), fill = NA, size = 0.7) + 
      scale_fill_gradientn(colours = colr, 
                           limits = c(min(log_sr), max(log_sr)),            
                           breaks = log_sr, 
                           labels = sr_breaks,
                           space = "Lab",
                           name = legend_title) +
      coord_fixed(ratio = diff(aus_x)/diff(aus_y), xlim = aus_x, ylim = aus_y) +
      theme_map() +
      theme(legend.direction = "vertical",
            legend.justification = "right",
            legend.position = "right",
            legend.key.size = unit(18, "cm"),
            legend.key.width = unit(1,"cm"),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 12),
            legend.box.spacing = unit(0.01, "cm"),
            plot.title = element_text(size = 20, face = "bold", hjust = 0.1, vjust = -0.5)) +
      guides(fill = guide_colorbar(barheight = unit(5, "cm")))
    
    plot(q)  
    
  # save 
    save_txt <- paste0("Results/maps/species/", title, ".jpeg")
    ggsave(save_txt, plot = last_plot(), height = 15, width = 15, units = "cm", dpi = 500, device = "jpeg")
    
  } # finish map fun
  
# requires ----------------------------------------------
# raster = raster
# title = plot and save title
# sr_breaks = the raw species richness legend labels (i.e. leg_lab)
# legend_title = legend units
# -------------------------------------------------------
  
# observed   
  for (i in 1:length(names(aus_pred))) {
    raster <- aus_pred[[i]]
    title <- paste0(names(aus_pred)[i])
    sr_breaks <- aus_leg_lab[[i]]
    legend_title <- "Species\nrichness"
    
    ras_v10_l(raster, title, sr_breaks, legend_title)
  }
  
# ----------------------------------------------------  
  
