
########################################################################################
# step five: Environmental variable covariation analyses
########################################################################################

# date created: 19/3/19
# last updated: 


# library ---------------------------------------------------------------------
  library(dplyr)
  library(RColorBrewer)
  library(ggthemes)
  library(corrplot)
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/")

# data -----------------------------------------------------------------------
  dat <- read.csv("EVs/CSV/100 km EFs scaled.csv", header = T) %>%
          dplyr::select(-cell.id, -cell.cat, -prop.cover)
  
  dat2 <- dplyr::select(dat, pcoldq, pwarmq, amt, ps, arid, pewc, th, hii)
  
# EDA analyses --------------------------------------------------------------
# Pearson's correlation
  pcor <- cor(dat, method = "pearson", use = "complete.obs")
  write.csv(pcor, "EVs/CSV/100 km correlation matrix.csv")  
 
# reduced EVs on account fo varaible selection 
  pcor.vs <- cor(dat2, method = "pearson", use = "complete.obs")
  write.csv(pcor.vs, "EVs/CSV/100 km correlation matrix redcued EV set.csv")  
  
# PCA #1: all EFs ----------------------------------------------------------
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  
  jpeg("EVs/Graphs/EDA/correlation matrix.jpeg", width = 15, height = 15, units = "cm", res = 300)
  corrplot(pcor, method = "color", col = col(200),
           type = "upper", order = "hclust", number.cex = .5,
           addCoef.col = "black", # Add coefficient of correlation
           tl.col = "black", tl.srt = 90) # Text label color and rotation
  dev.off()           

# PCA #2: reduced set (via EDA) ----------------------------------------------
  jpeg("EVs/Graphs/EDA/correlation matrix variabled selection.jpeg", width = 15, height = 15, units = "cm", res = 300)
  corrplot(pcor.vs, method = "color", col = col(200),
           type = "upper", order = "hclust", number.cex = .7,
           addCoef.col = "black", # Add coefficient of correlation
           tl.col = "black", tl.srt = 90) # Text label color and rotation
  dev.off()           
  
# 
  