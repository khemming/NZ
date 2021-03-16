########################################################
# records by species, pp, orgin, and genus
########################################################
# date created: 17/5
# last updated:

# aim --------------------------------------------------
# 1. summary tables (Table 1 in manuscipt and Appendix of genera data) by records, pp, orgin, etc. 
# 2. correlation of predicted & observed x native & exotic x C3 & C4 distributions
# 3. mean first date of occurrence -- C3 & C4
# library ----------------------------------------------
  library(tidyverse)
  library(data.table)
  library(raster)
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Poaceae/Data files")

# 1. summary tables ---------------------------------
# 1.1 table 1. total, c3, c4 records and species
# what script is this part actually in?
# not sure - have ot do again I think

# 1.2 difference between warning cells removed and retained -- table
# load rasters   
# warning cells removed
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/iNEXT/Rasters/observed 15 rec 0.8 cov warn removed")
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
  warn.rm <- getValues(c.stack)
  
# warning cells retained 
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/iNEXT/Rasters/observed 15 rec 0.8 cov warn retained")
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
  warn.retain <- getValues(c.stack)
  
# 
  
  
# 2. correlation matrix of observed and predicted distributions --------------------------  
# combine native and exotic rasters to data frame
# observed raster data
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/iNEXT/Rasters/observed 15 rec 0.8 cov warn removed")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
# predicted raster data
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/iNEXT/Rasters/predicted 15 rec 0.8 cov warn removed")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv) 
  
# o = observed, p = predicted; n = native, e = exotic; tot, c3 and c4
  o.n.tot <- getValues(n.tot)
  o.n.c3 <- getValues(n.c3)
  o.n.c4 <- getValues(n.c4)
  
  o.e.tot <- getValues(e.tot)
  o.e.c3 <- getValues(e.c3)
  o.e.c4 <- getValues(e.c4)
  
  p.n.tot <- getValues(n.tot.predicted)
  p.n.c3 <- getValues(n.c3.predicted)
  p.n.c4 <- getValues(n.c4.predicted)
  
  p.e.tot <- getValues(e.tot.predicted)
  p.e.c3 <- getValues(e.c3.predicted)
  p.e.c4 <- getValues(e.c4.predicted)
  
  spp.df <- cbind(o.n.tot, o.n.c3, o.n.c4, 
                  o.e.tot, o.e.c3, o.e.c4,
                  p.n.tot, p.n.c3, p.n.c4,
                  p.e.tot, p.e.c3, p.e.c4) 
  
  spp.df <- data.frame(spp.df) 
  
# correlation matrix
  p.o.cor <- round(cor(spp.df, use = "complete.obs"), 2)
  write.csv(p.o.cor, "C:/Users/s436862/Dropbox/Poaceae/Results/iNEXT/CSV/Observed-predicted distribution correlation matrix.csv", row.names = T)    
  
  
  
  
# mean first date of occurrence -----------------------------------------
# who got here first, C3s or C4s? And what's the background like on the natives?
  rm(list = ls())
  setwd("C:/Users/s436862/Dropbox/Poaceae")

# data
  dat <- readRDS("Data files/ALA/ALA master data/master grass data.rds")

# test w native C3
  n.c3 <- filter(dat, status == "native" & pp == "C3")

# unique spp    
  n.c3.d <- distinct(n.c3, species, year)
  
# now I want only the smallest year for a species
  
  
  
  
  
  
  
  
  
    
  
# --------------------------------------------------------------------------------------    