

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

# raw richness
  setwd("C:/Users/s436862/Dropbox/NZ/Results/rasters/iNEXT")
  files_ls <- list.files(pattern = ".grd")
  
  nz_ls <- Filter(function(x) grepl("NZ_", x), files_ls)
  nz_names <- gsub(pattern = "\\.grd$", "", nz_ls)
  nz_scale <- stack(nz_ls)
  names(nz_scale) <- nz_names
  
  aus_ls <- Filter(function(x) grepl("Aus_", x), files_ls)
  aus_names <- gsub(pattern = "\\.grd$", "", aus_ls)
  aus_scale <- stack(aus_ls)
  names(aus_scale) <- aus_names
  
# observed richness
  setwd("C:/Users/s436862/Dropbox/NZ/Results/rasters/log")
  files_ls <- list.files(pattern = ".grd")
  
  nz_ls <- Filter(function(x) grepl("NZ_", x), files_ls)
  nz_names <- gsub(pattern = "\\.grd$", "", nz_ls)
  nz_obs <- stack(nz_ls)
  names(nz_obs) <- nz_names
  
  aus_ls <- Filter(function(x) grepl("Aus_", x), files_ls)
  aus_names <- gsub(pattern = "\\.grd$", "", aus_ls)
  aus_obs <- stack(aus_ls)
  names(aus_obs) <- aus_names
  
# predicted richness
  setwd("C:/Users/s436862/Dropbox/NZ/Results/rasters/predicted")
  files_ls <- list.files(pattern = "predicted.grd")
  
  nz_ls <- Filter(function(x) grepl("NZ_", x), files_ls)
  nz_names <- gsub(pattern = "\\.grd$", "", nz_ls)
  nz_pred <- stack(nz_ls)
  names(nz_pred) <- nz_names
  
  aus_ls <- Filter(function(x) grepl("Aus_", x), files_ls)
  aus_names <- gsub(pattern = "\\.grd$", "", aus_ls)
  aus_pred <- stack(aus_ls)
  names(aus_pred) <- aus_names
 
# potential richness
  setwd("C:/Users/s436862/Dropbox/NZ/Results/rasters/potential")
  files_ls <- list.files(pattern = ".grd")
  
  nz_ls <- Filter(function(x) grepl("NZ_", x), files_ls)
  nz_names <- gsub(pattern = "\\.grd$", "", nz_ls)
  nz_pot <- stack(nz_ls)
  names(nz_pot) <- nz_names
  
  aus_ls <- Filter(function(x) grepl("Aus_", x), files_ls)
  aus_names <- gsub(pattern = "\\.grd$", "", aus_ls)
  aus_pot <- stack(aus_ls)
  names(aus_pot) <- aus_names

  setwd("C:/Users/s436862/Dropbox/NZ")
  
# NZ coordinates
  nz_x <- c(166, 179) 
  nz_y <- c(-48, -33)
 
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
    km_pos <- as.vector(data.frame(x = 178, 
                                   y = -47))
    
  # colours
    colr <- rev(brewer.pal(11, "Spectral"))
    
  # plot
    q <- ggplot() +  
      ggtitle(title) +
      geom_polygon(data = nz, aes(x = long, y = lat, group = group),
                   fill = "grey60") +
      geom_tile(data = raster_df, aes(x = x, y = y, fill = value)) +             
      geom_polygon(data = nz, colour = "grey1", 
                   aes(x = long, y = lat, group = group), fill = NA, size = 0.7) + 
      scale_fill_gradientn(colours = colr, 
                           limits = c(min(log_sr), max(log_sr)),            
                           breaks = log_sr, 
                           labels = sr_breaks,
                           space = "Lab",
                           name = legend_title) +
      coord_fixed(ratio = diff(nz_y)/diff(nz_x), xlim = nz_x, ylim = nz_y) +
      # ggsn::scalebar(km_scale, dist = 125, dist_unit = "km",  st.size = 3, 
      #                st.dist = 0.05, st.bottom = T, height = 0.05, transform = T, 
      #                model = 'WGS84', anchor = km_pos) +
      # north(data = km_scale, symbol = 1, anchor = c(y = -43, x = 177),
      #       x.min = 178, x.max = 175, y.min = -45, y.max = -47, scale = 0.15) +
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
    
    plot(q, )  
    
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
  
# NZ observed  
# calculate species richness breaks  
  nz_scale
  nz_obs_lab <- list(c(70,  45,  25, 10, 5, 1), # 65
                     c(1.5,  1),                # 2
                     c(70,  45,  25, 10, 5, 1), # 70
                     c(35,  20,  10, 5, 1))     # 34
# run  
  for (i in 1:length(names(nz_obs))) {
    raster <- nz_obs[[i]]
    title <- paste0(names(nz_obs)[i], "_observed")
    sr_breaks <- nz_obs_lab[[i]]
    legend_title <- "Species\nrichness"
    
    ras_v10_l(raster, title, sr_breaks, legend_title)
  }
  
# NZ predicted
  exp(nz_pred)
  nz_pred_lab <- list(c(65,  40,  25, 10, 5, 1), # 64
                      c(75,  50,  25, 10, 5, 1), # 72
                      c(35,  20,  10, 5, 1))     # 33
  
  for (i in 1:length(names(nz_pred))) {
    raster <- nz_pred[[i]]
    title <- paste0(names(nz_pred)[i])
    sr_breaks <- nz_pred_lab[[i]]
    legend_title <- "Species\nrichness"
    
    ras_v10_l(raster, title, sr_breaks, legend_title)
  }
  
# Aus
  exp(aus_pred)
  aus_pred_lab <- list(c(80, 50, 25, 10, 5, 1), # 79
                       c(10, 5, 0.1),             # 9
                       c(35,  20,  10, 5, 0.1),   # 33
                       c(20, 10, 5, 1))         # 19
  
  for (i in 1:length(names(aus_pred))) {
    raster <- aus_pred[[i]]
    title <- paste0(names(aus_pred)[i])
    sr_breaks <- aus_pred_lab[[i]]
    legend_title <- "Species\nrichness"
    
    ras_v10_l(raster, title, sr_breaks, legend_title)
  }

# maps - invasion potentital -------------------------------------------------------  
# note: version 10 maintains scaled legend to 0 and 1 and has a location scale bar
  ras_v10_po <- function(raster, title, sr_breaks, legend_title){
    
  # spatial points dataframe
    raster_spdf <- as(raster, "SpatialPixelsDataFrame")
    raster_df <- as.data.frame(raster_spdf)
    colnames(raster_df) <- c("value", "x", "y")
    
  # km scale bar
    km_scale <- raster_df %>% rename(lat = y, long = x)
    km_pos <- as.vector(data.frame(x = 178, 
                                   y = -47))
    
  # colours
    colr <- rev(brewer.pal(11, "Spectral"))
    
  # plot
    q <- ggplot() +
      ggtitle(title) +
      geom_polygon(data = nz, aes(x = long, y = lat, group = group),
                   fill = "grey60") +
      geom_tile(data = raster_df, aes(x = x, y = y, fill = value)) +
      geom_polygon(data = nz, colour = "grey1",
                   aes(x = long, y = lat, group = group), fill = NA, size = 0.7) +
      scale_fill_gradientn(colours = colr,
                           limits = c(0, 1),
                           breaks = sr_breaks,
                           labels = sr_breaks,
                           space = "Lab",
                           name = legend_title) +
      coord_fixed(ratio = diff(nz_y)/diff(nz_x), xlim = nz_x, ylim = nz_y) +
      ggsn::scalebar(km_scale, dist = 125, dist_unit = "km",  st.size = 3, st.dist = 0.05, st.bottom = T, height = 0.05, transform = T, model = 'WGS84', anchor = km_pos) +
      north(data = km_scale, symbol = 1, anchor = c(y = -43, x = 177),
            x.min = 178, x.max = 175, y.min = -45, y.max = -47, scale = 0.15) +
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
    
  } # finish function
  
  
# invasion potential ------------------------------------------------------  
# IP by NZ template 
  for (i in 1:length(names(nz_pot))) {
    
    raster <- nz_pot[[i]]
    title <- names(nz_pot)[i]
    sr_breaks <- c(0, 0.5, 1)
    legend_title <- "Scaled\nlog-\nspecies\nrichness"
    
    ras_v10_po(raster, title, sr_breaks, legend_title)
    
  }
  
# IP by Aus template 
  for (i in 1:length(names(aus_pot))) {
    
    raster <- aus_pot[[i]]
    title <- names(aus_pot)[i]
    sr_breaks <- c(0, 0.5, 1)
    legend_title <- "Scaled\nlog-\nspecies\nrichness"
    
    ras_v10_po(raster, title, sr_breaks, legend_title)
    
  }
  
# ----------------------------------------------------  
  
