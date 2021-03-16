
# library --------------------------------------------------------------
  library(tidyverse)
 
  rm(list = ls())
  
# data -------------------------------------
# parameter estimates 
  nzc3 <- read.csv("Results/csv/models/NZ C3 mean estimates.csv", stringsAsFactors = T)
  nzc4 <- read.csv("Results/csv/models/NZ C4 mean estimates.csv", stringsAsFactors = T)
  ausc3 <- read.csv("Results/csv/models/Aus C3 mean estimates.csv", stringsAsFactors = T)
  ausc4 <- read.csv("Results/csv/models/Aus C4 mean estimates.csv", stringsAsFactors = T) 
  
# envrionmental varaible order
  plot_names <- c("Land cover",
                  "Human\nimpact",
                  "Topographic\nheterogeneity",
                  "Winter\nrainfall",          
                  "Summer\nrainfall",
                  "Temperature\nseasonality",  
                  "Aridity",
                  "Annual mean\ntemperature")
 
# start here ----------------------------------
   
# build data frames for taxa we want to compare  
# C3 Aus native-NZ nonnative (a)
  a <- rbind(nzc3[9:16,], ausc3[1:8,]) %>%
       mutate(plot_names = factor(rep(plot_names, 2), levels = plot_names),
              group = paste(country, status, sep = ""))
# C3 NZ native-NZ nonnative (b)
  b <- nzc3 %>%
       mutate(plot_names = factor(rep(plot_names, 2), levels = plot_names),
              group = paste(country, status, sep = ""))
 
# C3 Aus native-NZ native (c)
  c <- rbind(nzc3[1:8,], ausc3[1:8,]) %>%
       mutate(plot_names = factor(rep(plot_names, 2), levels = plot_names),
              group = paste(country, status, sep = ""))
  
# C3 NZ, Aus and nonnative (d)
  d <- rbind(nzc3, ausc3[1:8,]) %>%
    mutate(plot_names = factor(rep(plot_names, 3), levels = plot_names),
           group = paste(country, status, sep = ""))  
  
# C4 Aus native-NZ nonnative (e)
  e <- rbind(nzc4, ausc4[1:8,])  %>%
       mutate(plot_names = factor(rep(plot_names, 2), levels = plot_names),
              group = paste(country, status, sep = ""))
  

# plot function ----------------------------------------------------------------------
  coef_v2 <- function(dat, title, leg_lab, leg_col, leg_shp, x_mn, x_mx, brks){ 
    q <- ggplot(dat, aes(y = plot_names, shape = group, colour = group)) +
          geom_point(aes(x = estimate), size = 3, position = position_dodge(width = 0.7)) +
          theme_classic() +
          scale_colour_manual(labels = leg_lab,
                              values = leg_col) + 
          scale_shape_manual(labels = leg_lab,
                             values = leg_shp) +
          geom_vline(aes(xintercept = 0),
                     colour = "black", 
                     size = 0.9, 
                     linetype = "dashed") +
          labs(colour = "Origin",
               shape = "Origin",  
               x = "Mean estimate",
               y = "") +
          geom_errorbarh(aes(xmin = lower, xmax = upper),
                         size = 1, position = position_dodge(width = 0.7), height = 0) +
          scale_x_continuous(limits = c(x_mn, x_mx),
                             breaks = brks,
                             labels = brks) +
      theme(legend.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.position = "bottom", 
            axis.title = element_text(size = 16),
            axis.line = element_line(colour = "black", size = 1),
            axis.text.x = element_text(colour = "black", size = 16),
            axis.text.y = element_text(colour = "black", size = 14),
            axis.ticks.length = unit(0.25, "cm"),
            axis.ticks = element_line(colour = "black", size = 1),
            plot.caption = element_text(size = 14))
    
    save <- paste0("Results/coefficient plots/", title, ".jpeg")
    ggsave(save, plot = last_plot(), height = 13, width = 14, units = "cm", dpi = 500, device = "jpeg")
    return(q) 
    
  } # coef_v1 end
  
# plots --------------------------------------------------------------------  
# required: 
# dat       = model plot dataframe
# title     = for saving only
# leg_lab   = legend labels
# leg_col   = colour
# leg_shp   = shape
# x_mn/x_mx = x axis margins 
# brks      = equal distances between mn/mx

# C3 Aus native-NZ nonnative (a)
  coef_v2(a,
          "C3 AU native NZ nonnative",
          c("Native AU", "Nonnative NZ"),
          c("grey35", "red"),
          c(17, 19),
          x_mn <- -1.5,
          x_mx <- 1.5,
          c(-1.5, -0.75, 0, 0.75, 1.5))
  
# C3 NZ native-NZ nonnative (b)
  coef_v2(b,
          "C3 NZ native NZ nonnative",
          c("Native NZ", "Nonnative NZ"),
          c("grey35", "red"),
          c(19, 19),
          x_mn <- -1.5,
          x_mx <- 1.5,
          c(-1.5, -0.75, 0, 0.75, 1.5))
  
# C3 native AU, native NZ (c)
  coef_v2(c,
          "C3 AU native NZ native",
          c("Native AU", "Native NZ"),
          c("grey35", "grey35"),
          c(17, 19),
          x_mn <- -0.8,
          x_mx <- 0.6,
          c(-0.8, -0.4, 0, 0.4, 0.8))
  
# C3 native and nonnative NZ, native AU
  coef_v2(d,
          "C3 native NZ nonnative NZ native AU",
          c("Native AU", "Native NZ", "Nonnative NZ"),
          c("grey35", "grey35","red"),
          leg_shp <- c(17, 19, 19),
          x_mn <- -1.5,
          x_mx <- 1.5,
          c(-1.5, -0.75, 0, 0.75, 1.5))
  
#
  coef_v2(e,
          title <- "C4 nonnative NZ native AU",
          leg_lab <- c("Native AU", "Nonnative NZ"),
          leg_col <- c("grey35", "red"),
          c(17, 19),
          x_mn <- -2.4,
          x_mx <- 2.8,
          c(-2, -1, 0, 1, 2))
  

# ----------------------------------------------------------------------------  
  
  