


# library -------------------------------------------------------------
  library(raster)
  library(tidyverse)
  library(ggrepel)
  
  rm(list = ls())

# data -----------------------------------------------------------------------
# NZ observed richness
  setwd("C:/Users/s436862/Dropbox/NZ/Results/rasters/log")
  files_ls <- list.files(pattern = ".grd")
  nz_ls <- Filter(function(x) grepl("NZ_", x), files_ls)
  nz_names <- gsub(pattern = "\\.grd$", "", nz_ls)
  nz_obs <- stack(nz_ls)
  names(nz_obs) <- nz_names
  list2env(setNames(unstack(nz_obs), nz_names), .GlobalEnv)

# predicted richness
  setwd("C:/Users/s436862/Dropbox/NZ/Results/rasters/predicted")
  files_ls <- list.files(pattern = "predicted.grd")
# NZ  
  nz_ls <- Filter(function(x) grepl("NZ_", x), files_ls)
  nz_names <- gsub(pattern = "\\.grd$", "", nz_ls)
  nz_pred <- stack(nz_ls)
  names(nz_pred) <- nz_names
  list2env(setNames(unstack(nz_pred), names(nz_pred)), .GlobalEnv)
# Aus  
  aus_ls <- Filter(function(x) grepl("Aus_", x), files_ls)
  aus_names <- gsub(pattern = "\\.grd$", "", aus_ls)
  aus_pred <- stack(aus_ls)
  names(aus_pred) <- aus_names
  list2env(setNames(unstack(aus_pred), names(aus_pred)), .GlobalEnv)
  
# invasion potential 
  setwd("C:/Users/s436862/Dropbox/NZ/Results/rasters/potential")
  files_ls <- list.files(pattern = "potential.grd")
# NZ  
  nz_ls <- Filter(function(x) grepl("NZ_", x), files_ls)
  nz_names <- gsub(pattern = "\\.grd$", "", nz_ls)
  nz_pot <- stack(nz_ls)
  names(nz_pot) <- nz_names
  list2env(setNames(unstack(nz_pot), names(nz_pot)), .GlobalEnv)
# Aus  
  aus_ls <- Filter(function(x) grepl("Aus_", x), files_ls)
  aus_names <- gsub(pattern = "\\.grd$", "", aus_ls)
  aus_pot <- stack(aus_ls)
  names(aus_pot) <- aus_names
  list2env(setNames(unstack(aus_pot), names(aus_pot)), .GlobalEnv)
  
  setwd("C:/Users/s436862/Dropbox/NZ")
  
# plots ---------------------------------------------------------------------
# log richness   
# note addition of x and y breaks
  log_corr <- function(dat, title, xlab, ylab, cor, label, x_brks, y_brks, x_pos, y_pos, save) {
    q <- ggplot(aes(x = x, y = y), data = dat) +
      geom_point(shape = "circle", size = 2) +
      geom_abline(intercept = 0, slope = 1/(max(log(y_brks))/max(log(x_brks))), size = 1, linetype = "dashed") +
      theme_bw() + 
      labs(x = xlab, y = ylab) +
      theme(panel.border = element_blank(),
            plot.title = element_text(size = 22),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black", size = 1),
            axis.text.x = element_text(colour = "black", size = 16),
            axis.text.y = element_text(colour = "black", size = 16),
            axis.ticks.length = unit(0.25, "cm"),
            axis.ticks = element_line(colour = "black", size = 1)) +
      scale_x_continuous(breaks = log(x_brks), 
                         labels = x_brks,
                         limits = c(min(log(x_brks)), max(log(x_brks))), 
                         expand = c(0, 0.05)) +
      scale_y_continuous(breaks = log(y_brks), 
                         labels = y_brks,
                         limits = c(min(log(y_brks)), max(log(y_brks))), 
                         expand = c(0, 0.05)) +
      annotate("text", x = x_pos, y = y_pos, label = label, size = 7) 
    q
    ggsave(save, plot = last_plot(), dpi = 500, width = 10, height = 9, units = "cm", device = "jpeg")  
    
    return(q)
  }
  
# scaled richness
  scaled_corr <- function(dat, title, xlab, ylab, cor, label, x_pos, y_pos, save) {
    q <- ggplot(aes(x = x, y = y), data = dat) +
      geom_point(shape = "circle", size = 2) +
      geom_abline(intercept = 0, slope = 1, size = 1, linetype = "dashed") +
      theme_bw() + 
      labs(x = xlab, y = ylab) +
      theme(panel.border = element_blank(),
            plot.title = element_text(size = 22),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black", size = 1),
            axis.text.x = element_text(colour = "black", size = 16),
            axis.text.y = element_text(colour = "black", size = 16),
            axis.ticks.length = unit(0.25, "cm"),
            axis.ticks = element_line(colour = "black", size = 1)) +
      scale_x_continuous(breaks = c(0, 0.5, 1), 
                         limits = c(0, 1), 
                         expand = c(0, 0.05)) +
      scale_y_continuous(breaks = c(0, 0.5,  1), 
                         limits = c(0, 1), 
                         expand = c(0, 0.05)) +
      annotate("text", x = x_pos, y = y_pos, label = label, size = 7) 
    q
    ggsave(save, plot = last_plot(), dpi = 500, width = 10, height = 9, units = "cm", device = "jpeg")   
    return(q)
  }
# -----------------------------------------------------------------------  

# C3 Australia native predicted x -------------------------------------------------
# NZ Exotic observed
# data
  x <- getValues(Aus_native_C3_predicted)/cellStats(Aus_native_C3_predicted, "max", na.rm = T)
  y <- getValues(NZ_nonnative_C3)/cellStats(NZ_nonnative_C3, "max", na.rm = T)
  dat <- data.frame(cbind(x, y))
# labels
  title <- "Aus native-NZ exotic C3 correlation"
  xlab <- "Predicted native AU\nrichness (scaled)"
  ylab <- "Observed exotic NZ\nrichness (scaled)" 
# correlation
  cor <- sprintf("%.2f", cor(x, y, use = "complete.obs", method = "spearman"), 2)
  label <- paste0("r = ", cor)
# text position
  x_pos <- 0.2
  y_pos <- 0.95
# save
  save <- paste0("Results/correlation plots/", title, ".jpeg")
# run  
  scaled_corr(dat, title, xlab, ylab, cor, label, x_pos, y_pos, save)

# n = 288 - 239 = 49    
  
# C4 Australia native predicted x -------------------------------------------------
# NZ nonnative observed
# data
  x <- getValues(Aus_native_C4_predicted)/cellStats(Aus_native_C4_predicted, "max", na.rm = T)
  y <- getValues(NZ_nonnative_C4)/cellStats(NZ_nonnative_C4, "max", na.rm = T)
  dat <- data.frame(cbind(x, y))
# labels
  title <- "Native AU C4-exotic NZ C4"
  xlab <- "Predicted native AU\nrichness (scaled)"
  ylab <- "Observed exotic NZ\nrichness (scaled)" 
# correlation
  cor <- sprintf("%.2f", cor(x, y, use = "complete.obs", method = "spearman"), 2)
  label <- paste0("r = ", cor)
# text position
  x_pos <- 0.2
  y_pos <- 0.95
# save
  save <- paste0("Results/correlation plots/", title, ".jpeg")
# run  
  scaled_corr(dat, title, xlab, ylab, cor, label, x_pos, y_pos, save)
  
  # n = 288 - 265 = 23    
  
# C3 NZ native observed x -------------------------------------------------
# NZ nonnative observed
# data
  x <- getValues(NZ_native_C3)/cellStats(NZ_native_C3, "max", na.rm = T)
  y <- getValues(NZ_nonnative_C3)/cellStats(NZ_nonnative_C3, "max", na.rm = T)
  dat <- data.frame(cbind(x, y))
# labels
  title <- "NZ native observed-NZ exotic C3 correlation"
  xlab <- "Observed native\nNZ richness (scaled)"
  ylab <- "Observed exotic NZ\nrichness (scaled)" 
# correlation
  cor <- sprintf("%.2f", cor(x, y, use = "complete.obs", method = "spearman"), 2)
  label <- paste0("r = ", cor)
# text position
  x_pos <- 0.2
  y_pos <- 0.95
# save
  save <- paste0("Results/correlation plots/", title, ".jpeg")
# run  
  scaled_corr(dat, title, xlab, ylab, cor, label, x_pos, y_pos, save)
  
# n = 288 - 253 = 35
  
# C3 NZ native predicted x  -------------------------------------------------
# NZ nonnative observed
# data
  x <- getValues(NZ_native_C3_predicted)/cellStats(NZ_native_C3_predicted, "max", na.rm = T)
  y <- getValues(NZ_nonnative_C3)/cellStats(NZ_nonnative_C3, "max", na.rm = T)
  dat <- data.frame(cbind(x, y))
# labels
  title <- "NZ native predicted-NZ exotic C3 correlation"
  xlab <- "Predicted native NZ\nrichness (scaled)"
  ylab <- "Observed exotic NZ\nrichness (scaled)" 
# correlation
  cor <- sprintf("%.2f", cor(x, y, use = "complete.obs", method = "spearman"), 2)
  label <- paste0("r = ", cor)
# text position
  x_pos <- 0.2
  y_pos <- 0.95
# save
  save <- paste0("Results/correlation plots/", title, ".jpeg")
# run  
  scaled_corr(dat, title, xlab, ylab, cor, label, x_pos, y_pos, save)
  
# C3 NZ native predicted x -------------------------------------------------
# C3 Aus native predicted
# data
  x <- getValues(NZ_native_C3_predicted)
  y <- getValues(Aus_native_C3_predicted)
  dat <- data.frame(cbind(x, y))
# labels
  title <- "Native-Aus native C3 predicted correlation"
  xlab <- "Predicted native\nNZ richness"
  ylab <- "Predicted native\nAU richness" 
# correlation
  cor <- sprintf("%.2f", cor(x, y, use = "complete.obs", method = "spearman"), 2)
  label <- paste0("r = ", cor)
# breaks: scale by max
  exp(NZ_native_C3_predicted)
  x_brks <- c(65,  40,  20, 10, 5, 1)
  exp(Aus_native_C3_predicted)
  y_brks <- c(80,  60,  40, 20, 5, 1)
# text position
  x_pos <- 0.9
  y_pos <- log(60)
# save
  save <- paste0("Results/correlation plots/", title, ".jpeg")
# run  
  log_corr(dat, title, xlab, ylab, cor, label, x_brks, y_brks, x_pos, y_pos, save)    
  
# n = 288 - 205 = 83
  
# C3 NZ native observed x -------------------------------------------------
# C3 Aus native predicted
# data
  x <- getValues(NZ_native_C3)
  y <- getValues(Aus_native_C3_predicted)
  dat <- data.frame(cbind(x, y))
# labels
  title <- "Native-Aus native C3 observed correlation"
  xlab <- "Observed native\nNZ richness"
  ylab <- "Predicted native\nAU richness" 
# correlation
  cor <- sprintf("%.2f", cor(x, y, use = "complete.obs", method = "spearman"), 2)
  label <- paste0("r = ", cor)
# breaks: scale by max
  exp(NZ_native_C3_predicted)
  x_brks <- c(65, 40, 20, 10, 5, 1)
  exp(Aus_native_C3_predicted)
  y_brks <- c(80,  60,  40, 20, 5, 1)
# text position
  x_pos <- 0.9
  y_pos <- log(60)
# save
  save <- paste0("Results/correlation plots/", title, ".jpeg")
# run  
  log_corr(dat, title, xlab, ylab, cor, label, x_brks, y_brks, x_pos, y_pos, save)  
# n = 288 - 233 = 55
  
# C3 NZ native observed x -----------------------------------------------------
# C3 NZ native predicted
# data
  x <- getValues(NZ_native_C3)
  y <- getValues(NZ_native_C3_predicted)
  dat <- data.frame(cbind(x, y))
# labels
  title <- "NZ native observed-predicted C3 correlation"
  xlab <- "Observed native\nNZ richness"
  ylab <- "Predicted native\nNZ richness" 
# correlation
  cor <- sprintf("%.2f", cor(x, y, use = "complete.obs", method = "spearman"), 2)
  label <- paste0("r = ", cor)
# breaks: scale by max
  exp(NZ_native_C3)
  x_brks <- c(65,  40,  20, 10, 5, 1)
  exp(NZ_native_C3_predicted)
  y_brks <- c(65,  40,  20, 10, 5, 1)
# text position
  x_pos <- 0.9
  y_pos <- log(50)
# save
  save <- paste0("Results/correlation plots/", title, ".jpeg")
# run  
  log_corr(dat, title, xlab, ylab, cor, label, x_brks, y_brks, x_pos, y_pos, save)  
  
# n = 288 - 233 = 55 
  
# C3 NZ nonnative observed x -----------------------------------------------------
# C4 NZ nonnative observed
# data
  x <- getValues(NZ_nonnative_C3)
  y <- getValues(NZ_nonnative_C4)
  dat <- data.frame(cbind(x, y))
# labels
  title <- "NZ C3-C4 exotic correlation"
  xlab <- "Observed exotic\nC3 NZ richness"
  ylab <- "Observed exotic\nC4 NZ richness"
# correlation
  cor <- sprintf("%.2f", cor(x, y, use = "complete.obs", method = "spearman"), 2)
  label <- paste0("r = ", cor)
# breaks: scale by max
  exp(NZ_nonnative_C3)
  x_brks <- c(70,  45,  25, 10, 5, 1)
  exp(NZ_nonnative_C4)
  x_brks <- c(35, 20, 10, 5, 1)
# text position
  x_pos <- 0.9
  y_pos <- log(50)
# save
  save <- paste0("Results/correlation plots/", title, ".jpeg")
# run  
  log_corr(dat, title, xlab, ylab, cor, label, x_brks, y_brks, x_pos, y_pos, save)  

# n = 288 - 283 = 5   
  
# C3 NZ nonnative invasion potential  x ------------------------------------
# C4 Aus nonnative nvasion potential
# data
  x <- getValues(NZ_N_nonnative_C3_potential)/cellStats(NZ_N_nonnative_C3_potential, "max", na.rm = T)
  y <- getValues(Aus_nonnative_C4_potential)/cellStats(Aus_nonnative_C4_potential, "max", na.rm = T)
  dat <- data.frame(cbind(x, y))
# labels
  title <- "C3-C4 exotic invasion potential"
  xlab <- "Potential exotic C3\nrichness (scaled)"
  ylab <- "Potential exotic C4\nrichness (scaled)" 
# correlation
  cor <- sprintf("%.2f", cor(x, y, use = "complete.obs", method = "spearman"), 2)
  label <- paste0("r = ", cor)
# text position
  x_pos <- 0.47
  y_pos <- 0.99
# save
  save <- paste0("Results/correlation plots/", title, ".jpeg")
# run  
  scaled_corr(dat, title, xlab, ylab, cor, label, x_pos, y_pos, save)
  
  # n = 288 - 283 = 83  

# -----------------------------------------------------------------------    
  
  
  