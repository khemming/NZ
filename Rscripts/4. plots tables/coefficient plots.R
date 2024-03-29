
# library --------------------------------------------------------------
  library(tidyverse)
 
  rm(list = ls())
  
# data -------------------------------------
# parameter estimates 
  nat_nzc3 <- read.csv("Results/csv/models/native NZ C3 mean estimates.csv", stringsAsFactors = T)
  non_nzc3 <- read.csv("Results/csv/models/nonnative NZ C3 mean estimates.csv", stringsAsFactors = T)
  non_nzc4 <- read.csv("Results/csv/models/nonnative NZ C4 mean estimates.csv", stringsAsFactors = T)
  nat_ausc3 <- read.csv("Results/csv/models/native Aus C3 mean estimates.csv", stringsAsFactors = T)
  nat_ausc4 <- read.csv("Results/csv/models/native Aus C4 mean estimates.csv", stringsAsFactors = T) 
  
# environmental variable order
  plot_names <- c("Human impact",
                  "Topographic\nheterogeneity",
                  "Winter rainfall",          
                  "Summer rainfall",
                  "Temperature\nseasonality",  
                  "Aridity",
                  "Annual mean\ntemperature")
  
# data frames to compare different taxa --------------------------------
# C3 Aus native-NZ nonnative (a)
  a <- rbind(nat_ausc3, non_nzc3) %>%
       mutate(plot_names = factor(rep(plot_names, 2), levels = plot_names),
              group = factor(paste(country, status, sep = ""), 
                             levels = c("NZNonnative", "AUNative"), 
                             ordered = is.ordered(c("NZNonnative", "AUNative"))))
  a
# C3 NZ native-NZ nonnative (b)
  b <- rbind(nat_nzc3, non_nzc3) %>%
       mutate(plot_names = factor(rep(plot_names, 2), levels = plot_names),
              group = factor(paste(country, status, sep = ""), 
                             levels = c("NZNonnative", "NZNative"), 
                             ordered = is.ordered(c("NZNonnative", "NZNative"))))
  b 
# C3 Aus native-NZ native (c)
  c <- rbind(nat_ausc3, nat_nzc3) %>%
       mutate(plot_names = factor(rep(plot_names, 2), levels = plot_names),
              group = factor(paste(country, status, sep = ""), 
                             levels = c("NZNative", "AUNative"), 
                             ordered = is.ordered(c("NZNative", "AUNative"))))
  c
# C3 NZ, Aus and nonnative (d)
  d <- rbind(nat_ausc3, non_nzc3, nat_nzc3) %>%
    mutate(plot_names = factor(rep(plot_names, 3), levels = plot_names),
           group = factor(paste(country, status, sep = ""), 
                          levels = c("NZNonnative", "NZNative", "AUNative"), 
                          ordered = is.ordered(c("NZNonnative", "NZNative", "AUNative"))))
  d
# C4 Aus native-NZ nonnative (e)
  e <- rbind(nat_ausc4, non_nzc4) %>%
       mutate(plot_names = factor(rep(plot_names, 2), levels = plot_names),
              group = factor(paste(country, status, sep = ""), 
                             levels = c("NZNonnative", "AUNative"), 
                             ordered = is.ordered(c("NZNonnative", "AUNative"))))
  e
  
  
  f <- rbind(nat_ausc4, nat_ausc3) %>%
       mutate(plot_names = factor(rep(plot_names, 2), levels = plot_names),
              group = c(rep("C3", 7), rep("C4", 7)))

# plot function ----------------------------------------------------------------------
  coef_v2 <- function(dat, title, leg_lab, leg_col, leg_shp, x_mn, x_mx, brks){ 
    q <- ggplot(dat, aes(y = plot_names, shape = group, colour = group)) +
          geom_point(aes(x = estimate), size = 3, position = position_dodge(width = 0.7)) +
          geom_errorbarh(aes(xmin = lower, xmax = upper),
                     size = 1, position = position_dodge(width = 0.7), height = 0) +
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
          scale_x_continuous(limits = c(x_mn, x_mx),
                             breaks = brks,
                             labels = brks) +
      theme(legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.position = "bottom", 
            axis.title = element_text(size = 14),
            axis.line = element_line(colour = "black", size = 1),
            axis.text.x = element_text(colour = "black", size = 14),
            axis.text.y = element_text(colour = "black", size = 14),
            axis.ticks.length = unit(0.25, "cm"),
            axis.ticks = element_line(colour = "black", size = 1),
            plot.caption = element_text(size = 14))
    
    save <- paste0("Results/coefficient plots/", title, ".jpeg")
    ggsave(save, plot = last_plot(), height = 10, width = 12, units = "cm", dpi = 500, device = "jpeg")
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
# -------------------------------------------------------------------
  
# C3 Aus native-NZ nonnative (a)
  coef_v2(a,
          "C3 AU native NZ nonnative",
          c("Exotic NZ", "Native AU"),
          c("red", "grey35"),
          c(19, 17),
          x_mn <- -1.4,
          x_mx <- 2,
          c(-1, 0, 1, 2))
  
# C3 NZ native-NZ nonnative (b)
  coef_v2(b,
          "C3 NZ native NZ nonnative",
          c("Exotic NZ", "Native NZ"),
          c("red", "grey35"),
          c(19, 19),
          x_mn <- -1.4,
          x_mx <- 2,
          c(-1, 0, 1, 2))
  
# C3 native AU, native NZ (c)
  coef_v2(c,
          "C3 AU native NZ native",
          c("Native NZ", "Native AU"),
          c("grey35", "grey35"),
          c(19, 17),
          x_mn <- -0.9,
          x_mx <- 0.45,
          c(-0.9, -0.6, -0.3, 0, 0.3))
  
# C3 native and nonnative NZ, native AU
  coef_v2(d,
          "C3 native NZ nonnative NZ native AU",
          c("Exotic NZ", "Native NZ", "Native AU"),
          c("red", "grey35","grey35"),
          leg_shp <- c(19, 19, 17),
          x_mn <- -1.3,
          x_mx <- 2,
          c(-1, 0, 1, 2))
  
# C4 Aus native-NZ nonnative (e)
  coef_v2(e,
          title <- "C4 nonnative NZ native AU",
          leg_lab <- c("Exotic NZ", "Native AU"),
          leg_col <- c("red", "grey35"),
          c(19, 17),
          x_mn <- -2.5,
          x_mx <- 3.1,
          c(-2.5, -1.25, 0, 1.25, 2.5))
  
  coef_v2(f,
          "C3 and C4 AU native",
          c("C3", "C4"),
          c("red", "blue"),
          c(19, 19),
          x_mn <- -1,
          x_mx <- 1,
          c(-1, -0.5, 0, 0.5, 1))
  

# ----------------------------------------------------------------------------  
  
  