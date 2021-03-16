
########################################################################################
# step four: plot 100 km rasters (and 1 km at end)
########################################################################################

# library ---------------------------------------------------------------------
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
  
# data ------------------------------------------------------------------  
  setwd("C:/Users/s436862/Dropbox/NZ-biogeography/Data files/EVs/step 2 - 100-km")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)

  setwd("C:/Users/s436862/Dropbox/NZ-biogeography")
  
# Plot function --------------------------------------------------------
#  Requires:
# (1) raster
# (2) legend [title] (e.g. record number or species richness)
# (3) file save name (e.g. Graphs/Native SR 50-km.jpeg)  [needs full extension]

# NZ at 100 km
  nz <- raster("Data files/New Zealand/nz 100-km.grd")
  plot(nz)
  
# function   
  ef_plot <- function (raster, legend, save)  
  {
  # AUS border + NA fill
  # oz1 <- borders("world", region = "New Zealand", fill = "grey50")
    oz2 <- borders("world", region = "New Zealand", colour = "black")
    
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
      coord_cartesian(xlim = c(165, 181), ylim = c(-50, -31), expand = F) +
      theme(legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 1.21) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), device = "jpeg", scale = 1, dpi = 500)
    
  } # finish function
  
# EF plots -----------------------------------------------------  
# Requires: Raster: EF raster 
#           legend: what it's long name is 
#           save: complete save location (C/Users etc.)
  setwd("C:/Users/s436862/Dropbox/NZ-biogeography/Results/EVs/100 km plots")
# ---------------------------------------------------------------  
# hii
  raster <- hii
  legend <- "Human \ninfluence \nindex"
  save <- "hii.jpeg"
  ef_plot(raster, legend, save)
  
# arid
  raster <- arid
  legend <- "Aridity"
  save <- "aridity.jpeg"
  ef_plot(raster, legend, save)
  
# rz    
  raster <- rz
  legend <- "water \navailability \nroot zone"
  save <- "rz.jpeg"
  ef_plot(raster, legend, save)
  
# st  
  raster <- st
  legend <- "Water \navailability \nsoil texture"
  save <- "st.jpeg"
  ef_plot(raster, legend, save)
  
# elev    
  raster <- elev
  legend <- "Elevation"
  save <- "elev.jpeg"
  ef_plot(raster, legend, save)
  
# evap  
  raster <- pet
  legend <- "Potential \nevapo-transpiration"
  save <- "pet.jpeg"
  ef_plot(raster, legend, save)
  
# pawc    
  # raster <- pawc
  # legend <- "Plant \navailable \nwater \ncapacity"
  # save <- "pawc.jpeg"
  # ef_plot(raster, legend, save)

# pewc    
  raster <- pewc
  legend <- "Plant \nextractable \nwater \ncapacity"
  save <- "pewc.jpeg"
  ef_plot(raster, legend, save)  
  
# amt  
  raster <- amt
  legend <- "Annual \nmean \ntemperature"
  save <- "amt.jpeg"
  ef_plot(raster, legend, save)
  
# mdr   
  raster <- mdr
  legend <- "Mean \ndiurnal \nrange"
  save <- "mdr.jpeg"
  ef_plot(raster, legend, save)
  
# iso  
  raster <- iso
  legend <- "Isothermality"
  save <- "iso.jpeg"
  ef_plot(raster, legend, save)
  
# ts
  raster <- ts
  legend <- "Temperature \nseasonality"
  save <- "ts.jpeg"
  ef_plot(raster, legend, save)
  
# twarmm 
  raster <- twarmm
  legend <- "Temperature \nwarmest month"
  save <- "twarmm.jpeg"
  ef_plot(raster, legend, save)
  
# tcoldm
  raster <- tcoldm
  legend <- "Temperature \ncoldest month"
  save <- "tcoldm.jpeg"
  ef_plot(raster, legend, save)
  
# tar
  raster <- tar
  legend <- "Annual \ntemperature \nrange"
  save <- "tar.jpeg"
  ef_plot(raster, legend, save)
  
# twetq
  raster <- twetq
  legend <- "Temperature \nwettest qtr."
  save <- "twetq.jpeg"
  ef_plot(raster, legend, save)
  
# tdryq
  raster <- tdryq
  legend <- "Temperature \ndriest qtr."
  save <- "tdryq.jpeg"
  ef_plot(raster, legend, save)
  
# twarmq
  raster <- twarmq
  legend <- "Temperature \nwarmest qtr."
  save <- "twarmq.jpeg"
  ef_plot(raster, legend, save)
  
# tcoldq
  raster <- tcoldq
  legend <- "Temperature \ncoldest qtr."
  save <- "tcoldq.jpeg"
  ef_plot(raster, legend, save)
  
# ap
  raster <- ap
  legend <- "Annual \nprecipitation"
  save <- "ap.jpeg"
  ef_plot(raster, legend, save)
  
# pwetm
  raster <- pwetm
  legend <- "Precipitation \nwettest \nmonth"
  save <- "pwetm.jpeg"
  ef_plot(raster, legend, save)
  
# pdrym
  raster <- pdrym
  legend <- "Precipitation \ndriest \nmonth"
  save <- "pdrym.jpeg"
  ef_plot(raster, legend, save)
  
# ps
  raster <- ps
  legend <- "Precipitation \nseasonality"
  save <- "ps.jpeg"
  ef_plot(raster, legend, save)
  
# pwetq
  raster <- ap
  legend <- "Precipitation \nwettest qtr."
  save <- "pwetq.jpeg"
  ef_plot(raster, legend, save)
  
# pdryq
  raster <- pdryq
  legend <- "Precipitation \ndriest qtr."
  save <- "pdryq.jpeg"
  ef_plot(raster, legend, save)
  
# pwarmq
  raster <- pwarmq
  legend <- "Precipitation \nwarmest qtr."
  save <- "pwarmq.jpeg"
  ef_plot(raster, legend, save)
  
# pcoldq
  raster <- pcoldq
  legend <- "Precipitation \ncoldest qtr."
  save <- "pcoldq.jpeg"
  ef_plot(raster, legend, save)
  
# th
  raster <- th
  legend <- "Topographic \nheterogeniety"
  save <- "th.jpeg"
  ef_plot(raster, legend, save)  
  

  
