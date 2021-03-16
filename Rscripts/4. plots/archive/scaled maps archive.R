

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
# country outlines
  setwd("C:/Users/s436862/Dropbox/NZ")
  nz <- readOGR("Data files/NZ/NZ shapefile.shp")
  plot(nz)
  oz <- readOGR("Data files/Australia/Australia shapefile.shp")
  plot(oz)
  
# raw richness for scale
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
  setwd("C:/Users/s436862/Dropbox/NZ/Results/rasters/log scaled")
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
  setwd("C:/Users/s436862/Dropbox/NZ/Results/rasters/log scaled")
  files_ls <- list.files(pattern = ".grd")
  
  nz_ls <- Filter(function(x) grepl("NZ_", x), files_ls)
  nz_names <- gsub(pattern = "\\.grd$", "", nz_ls)
  nz_pred <- stack(nz_ls)
  names(nz_pred) <- nz_names
  
  names <- gsub(pattern = "\\.grd$", "", files_ls)
  pred <- stack(files_ls)
  names(pred) <- names
 
# potential richness
  setwd("C:/Users/s436862/Dropbox/NZ/Results/rasters/potential")
  files_ls <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", files_ls)
  poten <- stack(files_ls)
  names(poten) <- names

  setwd("C:/Users/s436862/Dropbox/NZ")
  
# coordinates for Aus/NZ
  nz_x <- c(166, 179) 
  nz_y <- c(-48, -33)
  aus_x <- c(112, 155) 
  aus_y <- c(-45, -7)
  
# maps (v10) ---------------------------------------------------------
# scale bar --------------------------------------------------------------
# raw species richness to scale map legends
  nz_s <- vector()
  
  for (i in 1:nlayers(nz_scale)){
    
    nz_s[i] <- ceiling(cellStats(nz_scale[[i]], "max", na.rm = T))
    
  }
  nz_s 
  ras_v8 <- function(country, x_lim, y_lim, raster, title, sr_breaks, legend_title){
    
  # scale breaks and labels to give rounded and highest value
    log_sr <- log(sr_breaks)
    scale_sr <- log_sr/max(log_sr)
    
  # raster to spatial points dataframe 
    raster_spdf <- as(raster, "SpatialPixelsDataFrame")
    raster_df <- as.data.frame(raster_spdf)=
    colnames(raster_df) <- c("value", "x", "y")
  # pretty colours
    colr <- rev(brewer.pal(11, "Spectral"))
  # save 
    save_txt <- paste0("Results/maps/scaled/", title, ".jpeg")
    
  # plot
    q <- ggplot() +  
      ggtitle(title) +
      geom_polygon(data = country, aes(x = long, y = lat, group = group), 
                   fill = "grey60") +                                               
      geom_tile(data = raster_df, aes(x = x, y = y, fill = value)) +             
      geom_polygon(data = country, colour = "grey1", 
                   aes(x = long, y = lat, group = group), fill = NA, size = 0.7) + # changed from 1
      scale_fill_gradientn(colours = colr, 
                           limits = c(min(scale_sr), max(scale_sr)),            
                           breaks = scale_sr,
                           labels = sr_breaks,
                           space = "Lab",
                           name = legend_title) +
      coord_fixed(xlim = x_lim,
                  ylim = y_lim, 
                  expand = F) +
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
    
    ggsave(save_txt, plot = last_plot(), width = 15, height = 15, units = "cm", dpi = 500, device = "jpeg")
} # finish function
# scaled maps (v9) ---------------------------------------------------  
  ras_v9 <- function(country, x_lim, y_lim, raster, title, sr_breaks, legend_title){
    
  # raster to spatial points dataframe 
    raster_spdf <- as(raster, "SpatialPixelsDataFrame")
    raster_df <- as.data.frame(raster_spdf)
    colnames(raster_df) <- c("value", "x", "y")
  # colours
    colr <- rev(brewer.pal(11, "Spectral"))
  # save 
    save_txt <- paste0("Results/maps/scaled/", title, ".jpeg")
    
  # plot
    q <- ggplot() +  
      ggtitle(title) +
      geom_polygon(data = country, aes(x = long, y = lat, group = group), 
                   fill = "grey60") +                                               
      geom_tile(data = raster_df, aes(x = x, y = y, fill = value)) +             
      geom_polygon(data = country, colour = "grey1", 
                   aes(x = long, y = lat, group = group), fill = NA, size = 0.7) + # changed from 1
      scale_fill_gradientn(colours = colr, 
                           limits = c(0, 1),            
                           breaks = sr_breaks,
                           labels = sr_breaks,
                           space = "Lab",
                           name = legend_title) +
      coord_fixed(xlim = x_lim,
                  ylim = y_lim, 
                  expand = F) +
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
    
    ggsave(save_txt, plot = last_plot(), width = 15, height = 15, units = "cm", dpi = 500, device = "jpeg")
  } # finish function
# function requirements ----------------------------------------------------------------- 
# country      = outline
# raster       = spp
# title        = identifiable via save
# sr_breaks    = logical raw spp richness (iNEXT) breaks for scale bar
# legend_title = raw richness or scaled log-richness
# x_lim, y_lim = country plot limits
  
# NZ observed ---------------------------------------------------------
  plot(nz_scale[[1]])
  plot(nz_scale[[2]])
  plot(nz_scale[[3]])
  plot(nz_scale[[4]])
  
  obs_scale <- list(c(70, 40,  20, 5,  1),  # native C3
                    c(30, 20,  10, 5,  1),  # native C4
                    c(70, 40,  10, 5,  1),  # nonnative C3
                    c(          2, 1,  0))  # nonnative C4
  
 for (i in 1:length(names(nz_obs))) {
    country <- nz
    x_lim <- nz_x
    y_lim <- nz_y
    
    raster <- nz_obs[[i]]
    title <- paste0(names(nz_obs)[i], "_observed")
    sr_breaks <- obs_scale[[i]]
    legend_title <- "Species\nrichness"
   
    ras_v8(country, x_lim, y_lim, raster, title, sr_breaks, legend_title)
  }
  
# observed Aus -------------------------------------------
  plot(aus_scale[[1]])
  plot(aus_scale[[2]])
  plot(aus_scale[[3]])
  plot(aus_scale[[4]])
 
  obs_scale <- list(c(60,  40, 20,  5, 1),  # Aus native C3
                    c(160, 80, 40, 10, 1),  # Aus native C4
                    c(40,  20, 10,  5, 1),  # Aus nonnative C3
                    c(40,  20, 10,  5, 1))  # Aus nonnative C4
                   
  for (i in 1:length(names(aus_obs))) {
    country <- oz
    x_lim <- aus_x
    y_lim <- aus_y
    
    raster <- aus_obs[[i]]
    title <- paste0(names(aus_obs)[i], "_observed")
    sr_breaks <- obs_scale[[i]]
    legend_title <- "Species\nrichness"
    
    ras_v8(country, x_lim, y_lim, raster, title, sr_breaks, legend_title)
  }

# predicted -------------------------------------------------------
  names(pred) # note: no predicted nonnative C4
  pred_scale <- list(c(60, 40,  20, 5,  1),  # Aus native C3
                     c(160, 80, 40, 10, 1),  # Aus native C4
                     c(70, 40,  20, 5,  1),  # NZ native C3
                     c(30, 20,  10, 5,  1),  # NZ native C4
                     c(70, 40,  10, 5,  1))  # nonnative C3
  
  for (i in 1:length(names(pred))) {
    country <- nz
    x_lim <- nz_x
    y_lim <- nz_y
    
    raster <- pred[[i]]
    title <- paste0(names(pred)[i])
    sr_breaks <- pred_scale[[i]]
    legend_title <- "Species\nrichness"
    
    ras_v8(country, x_lim, y_lim, raster, title, sr_breaks, legend_title)
  }
  
# potential -------------------------------------------------------
  for (i in 1:length(names(poten))) {
    country <- nz
    x_lim <- nz_x
    y_lim <- nz_y
    
    raster <- poten[[i]]
    title <- paste0(names(poten)[i])
    sr_breaks <- c(0, 0.5, 1)
    legend_title <- "Scaled\nlog-\nspecies\nrichness"
    
    ras_v9(country, x_lim, y_lim, raster, title, sr_breaks, legend_title)
  }
  