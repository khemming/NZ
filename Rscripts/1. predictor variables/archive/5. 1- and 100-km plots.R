
# date created: 4/6/18
# last updated: 27/6

# # Might help : 
# library(gplot2)
# 

# ggThemeAssistGadget(plot1) 
# makes an easy-to-use interface and then returns the code that configures your plot

# EF 4. plots
# single-EF plots, and maybe correlations withs rarefaction
# though I might end up sticking that in a new script

  library(RColorBrewer)
  library(raster)
  library(ggmap)
  library(tidyr)
  library(rgdal)
  library(ggplot2)
  library(dplyr)
  library(rasterVis)
  library(maptools)
  library(ggthemes)
  
  rm(list = ls())
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")

# EFs at 1-km scale ---------------------------------------------------- 
# Note: th is missing because its only in 100-km scale
  hii <- raster("EFs/Rasters 1-km Aus-cropped/hii.grd") 
  arid <- raster("EFs/Rasters 1-km Aus-cropped/arid.grd") 
  rz <- raster("EFs/Rasters 1-km Aus-cropped/rz.grd")
  sp <- raster("EFs/Rasters 1-km Aus-cropped/sp.grd")
  st <- raster("EFs/Rasters 1-km Aus-cropped/st.grd")
  elev <- raster("EFs/Rasters 1-km Aus-cropped/elev.grd")
  pet <- raster("EFs/Rasters 1-km Aus-cropped/pet.grd")
  pawc <- raster("EFs/Rasters 1-km Aus-cropped/pawc.grd")
  pewc <- raster("EFs/Rasters 1-km Aus-cropped/pewc.grd")
  amt <- raster("EFs/Rasters 1-km Aus-cropped/amt.grd")
  mdr <- raster("EFs/Rasters 1-km Aus-cropped/mdr.grd")
  iso <- raster("EFs/Rasters 1-km Aus-cropped/iso.grd")
  ts <- raster("EFs/Rasters 1-km Aus-cropped/ts.grd")
  twarmm <- raster("EFs/Rasters 1-km Aus-cropped/twarmm.grd")
  tcoldm <- raster("EFs/Rasters 1-km Aus-cropped/tcoldm.grd")
  tar <- raster("EFs/Rasters 1-km Aus-cropped/tar.grd")
  twetq <- raster("EFs/Rasters 1-km Aus-cropped/twetq.grd")
  tdryq <- raster("EFs/Rasters 1-km Aus-cropped/tdryq.grd")
  twarmq <- raster("EFs/Rasters 1-km Aus-cropped/twarmq.grd")
  tcoldq <- raster("EFs/Rasters 1-km Aus-cropped/tcoldq.grd")
  ap <- raster("EFs/Rasters 1-km Aus-cropped/ap.grd")
  pwetm <- raster("EFs/Rasters 1-km Aus-cropped/pwetm.grd")
  pdrym <- raster("EFs/Rasters 1-km Aus-cropped/pdrym.grd")
  ps <- raster("EFs/Rasters 1-km Aus-cropped/ps.grd")
  pwetq <- raster("EFs/Rasters 1-km Aus-cropped/pwetq.grd")
  pdryq <- raster("EFs/Rasters 1-km Aus-cropped/pdryq.grd")
  pwarmq <- raster("EFs/Rasters 1-km Aus-cropped/pwarmq.grd")
  pcoldq <- raster("EFs/Rasters 1-km Aus-cropped/pcoldq.grd")

# Plot function --------------------------------------------------------
#  Requires:
# (1) raster
# (2) legend [title] (e.g. record number or species richness)
# (3) file save name (e.g. Graphs/Native SR 50-km.jpeg)  [needs full extension]

# function   
  ef_plot <- function (raster, legend, save)  
  {
  # AUS border + NA fill
  # oz1 <- borders("world", region = "Australia", fill = "grey50")
    oz2 <- borders("world", region = "Australia", colour = "black")
    
  # Colour palette for legend
    colour <- rev(brewer.pal(11, "Spectral"))
  # Plot
    q <- gplot(raster) + 
      theme_map()+
      geom_raster(aes(fill = value)) +
      scale_fill_gradientn(colours = colour, 
                           space = "Lab",
                           na.value = "white",
                           guide = "colourbar",
                           name = legend
      ) + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 0.88) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
  } # finish function
  
# Seems legit  
  
  
# EF plots -----------------------------------------------------  
# Requires: Raster: EF raster 
#           legend: what it's long name is 
#           save: complete save location (C/Users etc.)

# hii
  raster <- hii
  legend <- "Human influence index"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/hii.jpeg"
  ef_plot(raster, legend, save)
  
# arid
  raster <- arid
  legend <- "Aridity"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/aridity.jpeg"
  ef_plot(raster, legend, save)
  
# rz    
  raster <- rz
  legend <- "Water availability of root zone"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/rz.jpeg"
  ef_plot(raster, legend, save)
  
# st  
  raster <- st
  legend <- "Water availability of soil texture"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/st.jpeg"
  ef_plot(raster, legend, save)
  
# elev    
  raster <- elev
  legend <- "Elevation"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/elev.jpeg"
  ef_plot(raster, legend, save)
  
# pet  
  raster <- pet
  legend <- "Potential evapo-transpiration"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/pet.jpeg"
  ef_plot(raster, legend, save)
  
# pawc    
  raster <- pawc
  legend <- "Plant available water capacity"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/pawc.jpeg"
  ef_plot(raster, legend, save)
  
# pewc    
  raster <- pewc
  legend <- "Plant extractable water capacity"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/pewc.jpeg"
  ef_plot(raster, legend, save)  
  
# amt  
  raster <- amt
  legend <- "Annual mean temperature"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/mat.jpeg"
  ef_plot(raster, legend, save)
  
# mdr   
  raster <- mdr
  legend <- "Mean diurnal range"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/mdr.jpeg"
  ef_plot(raster, legend, save)
  
# iso  
  raster <- iso
  legend <- "Isothermality"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/iso.jpeg"
  ef_plot(raster, legend, save)
  
# ts
  raster <- ts
  legend <- "Temperature seasonality"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/ts.jpeg"
  ef_plot(raster, legend, save)
  
# twarmm 
  raster <- twarmm
  legend <- "Mx temp. warmest month"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/twarmm.jpeg"
  ef_plot(raster, legend, save)
  
# tcoldm
  raster <- tcoldm
  legend <- "Mn temp. coldest month"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/tcoldm.jpeg"
  ef_plot(raster, legend, save)
  
# tar
  raster <- tar
  legend <- "Annual temperature range"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/tar.jpeg"
  ef_plot(raster, legend, save)
  
# twetq
  raster <- twetq
  legend <- "Mean temp. wettest qtr."
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/twetq.jpeg"
  ef_plot(raster, legend, save)
  
# tdryq
  raster <- tdryq
  legend <- "Mean temp. driest qtr."
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/tdryq.jpeg"
  ef_plot(raster, legend, save)
  
# twarmq
  raster <- twarmq
  legend <- "Mean temp. warmest qtr."
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/twarmq.jpeg"
  ef_plot(raster, legend, save)
  
# tcoldq
  raster <- tcoldq
  legend <- "Mean temp. coldest qtr."
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/tcoldq.jpeg"
  ef_plot(raster, legend, save)
  
# ap
  raster <- ap
  legend <- "Annual precipitation"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/ap.jpeg"
  ef_plot(raster, legend, save)
  
# pwetm
  raster <- pwetm
  legend <- "Precip. wettest month"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/pwetm.jpeg"
  ef_plot(raster, legend, save)
  
# pdrym
  raster <- pdrym
  legend <- "Precip. driest month"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/pdrym.jpeg"
  ef_plot(raster, legend, save)
  
# ps
  raster <- ps
  legend <- "Precipitation seasonality"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/ps.jpeg"
  ef_plot(raster, legend, save)
  
# pwetq
  raster <- ap
  legend <- "Precip. wettest qtr."
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/pwetq.jpeg"
  ef_plot(raster, legend, save)
  
# pdryq
  raster <- pdryq
  legend <- "Precip. driest qtr."
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/pdryq.jpeg"
  ef_plot(raster, legend, save)
  
# pwarmq
  raster <- pwarmq
  legend <- "Precip. warmest qtr."
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/pwarmq.jpeg"
  ef_plot(raster, legend, save)
  
# pcoldq
  raster <- pcoldq
  legend <- "Precip. coldest qtr."
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 1-km/pcoldq.jpeg"
  ef_plot(raster, legend, save)
  
# ----------------------------------------------------------------------  
  
# EFs at 100-km scale -------------------------------------------------- 
# Plot function --------------------------------------------------------
#  Requires:
# (1) raster
# (2) legend [title] (e.g. record number or species richness)
# (3) file save name (e.g. Graphs/Native SR 50-km.jpeg)  [needs full extension]

# function   
  ef_plot <- function (raster, legend, save)  
  {
  # AUS border + NA fill
  # oz1 <- borders("world", region = "Australia", fill = "grey50")
    oz2 <- borders("world", region = "Australia", colour = "black")
    
  # Colour palette for legend
    colour <- rev(brewer.pal(11, "Spectral"))
  # Plot
    q <- gplot(raster) + 
      theme_map()+
      geom_raster(aes(fill = value)) +
      scale_fill_gradientn(colours = colour, 
                           space = "Lab",
                           na.value = "white",
                           guide = "colourbar",
                           name = legend
      ) + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 0.88) +
      oz2
    print(q)
    
      ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
      
    } # finish function
    
  # Seems legit  
  
  
# Data ----------------------------------------------------------------------
  ef <- read.csv("EFs/CSV/100-km raw.csv", header = T)
  
# Australia raster template
  aus <- raster("Australia/aus_100km_cell_id.grd")

# Run script ----------------------------------------------------------------
# Requires: Raster: EF raster 
#           legend: what it's long name is 
#           save: complete save location (C/Users etc.)
  
# hii
  raster <- setValues(aus, ef$hii)
  legend <- "Human influence index"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/hii.jpeg"
  ef_plot(raster, legend, save)
  
# arid
  raster <- setValues(aus, ef$arid)
  legend <- "Aridity"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/arid.jpeg"
  ef_plot(raster, legend, save)
  
# rz    
  raster <- setValues(aus, ef$rz)
  legend <- "water avai. root zone"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/rz.jpeg"
  ef_plot(raster, legend, save)
  
# st  
  raster <- setValues(aus, ef$st)
  legend <- "Water avai. soil texture"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/st.jpeg"
  ef_plot(raster, legend, save)
  
# elev    
  raster <- setValues(aus, ef$elev)
  legend <- "Elevation"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/elev.jpeg"
  ef_plot(raster, legend, save)
  
# pet  
  raster <- setValues(aus, ef$pet)
  legend <- "Potential evapo-transpiration"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/pet.jpeg"
  ef_plot(raster, legend, save)
  
# pawc    
  raster <- setValues(aus, ef$pawc)
  legend <- "Plant available water capacity"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/pawc.jpeg"
  ef_plot(raster, legend, save)
  
# pewc    
  raster <- setValues(aus, ef$pewc)
  legend <- "Plant extractable water capacity"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/pewc.jpeg"
  ef_plot(raster, legend, save)  
  
# amt  
  raster <- setValues(aus, ef$amt)
  legend <- "Annual mean temperature"
  save <- "C:/Users/s436862/Dropbox/Cliamte Matching/4. Results/Graphs/EFs 100-km/amt.jpeg"
  ef_plot(raster, legend, save)
  
# mdr   
  raster <- setValues(aus, ef$mdr)
  legend <- "Mean diurnal range"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/mdr.jpeg"
  ef_plot(raster, legend, save)
  
# iso  
  raster <- setValues(aus, ef$iso)
  legend <- "Isothermality"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/iso.jpeg"
  ef_plot(raster, legend, save)
  
# ts
  raster <- setValues(aus, ef$ts)
  legend <- "Temperature seasonality"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/ts.jpeg"
  ef_plot(raster, legend, save)
  
# twarmm 
  raster <- setValues(aus, ef$twarmm)
  legend <- "Mx temp. warmest month"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/twarmm.jpeg"
  ef_plot(raster, legend, save)
  
# tcoldm
  raster <- setValues(aus, ef$tcoldm)
  legend <- "Mn temp. coldest month"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/tcoldm.jpeg"
  ef_plot(raster, legend, save)
  
# tar
  raster <- setValues(aus, ef$tar)
  legend <- "Annual temperature range"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/tar.jpeg"
  ef_plot(raster, legend, save)
  
# twetq
  raster <- setValues(aus, ef$twetq)
  legend <- "Mean temp. wettest qtr."
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/twetq.jpeg"
  ef_plot(raster, legend, save)
  
# tdryq
  raster <- setValues(aus, ef$tdryq)
  legend <- "Mean temp. driest qtr."
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/tdryq.jpeg"
  ef_plot(raster, legend, save)
  
# twarmq
  raster <- setValues(aus, ef$twarmq)
  legend <- "Mean temp. warmest qtr."
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/twarmq.jpeg"
  ef_plot(raster, legend, save)
  
# tcoldq
  raster <- setValues(aus, ef$tcoldq)
  legend <- "Mean temp. coldest qtr."
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/tcoldq.jpeg"
  ef_plot(raster, legend, save)
  
# ap
  raster <- setValues(aus, ef$ap)
  legend <- "Annual precipitation"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/ap.jpeg"
  ef_plot(raster, legend, save)
  
# pwetm
  raster <- setValues(aus, ef$pwetm)
  legend <- "Precip. wettest month"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/pwetm.jpeg"
  ef_plot(raster, legend, save)
  
# pdrym
  raster <- setValues(aus, ef$pdrym)
  legend <- "Precip. driest month"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/pdrym.jpeg"
  ef_plot(raster, legend, save)
  
# ps
  raster <- setValues(aus, ef$ps)
  legend <- "Precipitation seasonality"
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/ps.jpeg"
  ef_plot(raster, legend, save)
  
# pwetq
  raster <- setValues(aus, ef$pwetq)
  legend <- "Precip. wettest qtr."
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/pwetq.jpeg"
  ef_plot(raster, legend, save)
  
# pdryq
  raster <- setValues(aus, ef$pdryq)
  legend <- "Precip. driest qtr."
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/pdryq.jpeg"
  ef_plot(raster, legend, save)
  
# pwarmq
  raster <- setValues(aus, ef$pwarmq)
  legend <- "Precip. warmest qtr."
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/pwarmq.jpeg"
  ef_plot(raster, legend, save)
  
# pcoldq
  raster <- setValues(aus, ef$pcoldq)
  legend <- "Precip. coldest qtr."
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/pcoldq.jpeg"
  ef_plot(raster, legend, save)
  
# th
  raster <- setValues(aus, ef$th)
  legend <- "Topo. hetero."
  save <- "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EFs 100-km/th.jpeg"
  ef_plot(raster, legend, save)  
  
  
  
  
  
  
  
  
  
  
  
    