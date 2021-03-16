#########################################################
# scatter plots
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
  library(gplots)
  library(tidyverse)
  library(forcats)
  library(maps)
  
  rm(list = ls())

# data -------------------------------------------------------
# NZ
  setwd("Results/rasters/scaled/NZ")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
  setwd("C:/Users/s436862/Dropbox/NZ-biogeography") 
  
# Plot 1: C3 Aus(aus pred) x Aus(aus nonnat obs) ------------------------------------------------  
# ---------------------------------------------------------------------------------
# Plot 2: C3 Aus(nz pred) x NZ(nonnat obs)
# setting up plot
  x <- getValues(Aus_predicted_C3)
  xlab <- "Australia predicted C3"
  
  y <- getValues(NZ_nonnative_C3)
  ylab <- "NZ non-native C3" 
  
  dat <- data.frame(cbind(x, y))
  file_name <- paste(xlab, "x", ylab)
  save <- paste0("Results/correlation plots/", file_name, ".jpeg")
  
# correlation
  corr <- cor(x, y, method = "pearson", use = "complete.obs")
  oc <- format(round(corr, 2), nsmall = 2)
  cor_lab <- paste0("r = ", oc)
# plot  
  ggplot(aes(x = x, y = y), data = dat) +
    geom_point(shape = "circle", size = 4) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 2),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
          # axis.text.x = element_text(colour = "black", size = 18),
          # axis.text.y = element_text(colour = "black", size = 18),
          # axis.ticks = element_line(size = 2, colour = "black"),
          # axis.ticks.length = unit(0.25, "cm")) +
    scale_x_continuous(breaks = c(0, 0.5, 1),
                       limits = c(0, 1)) +
    scale_y_continuous(breaks = c(0, 0.5, 1),
                       limits = c(0, 1)) +
    geom_abline(intercept = 0, slope = 1, size = 1.5, linetype = "dashed") +
    annotate("text", x = 0.75, y = 0.9, label = cor_lab, size = 7) +
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 18))
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")

# ------------------------------------------------------------------------------  
# Plot 3: C3 NZ(pred) x NZ(nonnat obs)
# setting up plot
  x <- getValues(NZ_predicted_C3)
  xlab <- "NZ predicted C3"
  
  y <- getValues(NZ_nonnative_C3)
  ylab <- "NZ non-native C3" 
  
  dat <- data.frame(cbind(x, y))
  file_name <- paste(xlab, "x", ylab)
  save <- paste0("Results/correlation plots/", file_name, ".jpeg")
  
# correlation
  corr <- cor(x, y, method = "pearson", use = "complete.obs")
  oc <- format(round(corr, 2), nsmall = 2)
  cor_lab <- paste0("r = ", oc)
# plot  
  ggplot(aes(x = x, y = y), data = dat) +
    geom_point(shape = "circle", size = 4) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 2),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
        # axis.text.x = element_text(colour = "black", size = 18),
        # axis.text.y = element_text(colour = "black", size = 18),
        # axis.ticks = element_line(size = 2, colour = "black"),
        # axis.ticks.length = unit(0.25, "cm")) +
    scale_x_continuous(breaks = c(0, 0.5, 1),
                       limits = c(0, 1)) +
    scale_y_continuous(breaks = c(0, 0.5, 1),
                       limits = c(0, 1)) +
    geom_abline(intercept = 0, slope = 1, size = 1.5, linetype = "dashed") +
    annotate("text", x = 0.75, y = 0.9, label = cor_lab, size = 7) +
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 18))
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
# -------------------------------------------------------------------  