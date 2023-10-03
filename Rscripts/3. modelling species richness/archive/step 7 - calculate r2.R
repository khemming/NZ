


# library ------------------------------------------------------------------
  library(raster)
 
  rm(list = ls())

# data ---------------------------------------------------------------------   
# model data
  load("Data files/rdata/models.RData")
  
# load observed rasters
  setwd("C:/Users/Hemming/Dropbox/Poaceae/Results/rasters/scaled")
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  spp <- as.data.frame(c.stack, na.rm = F)
  
# load predicted rasters
  setwd("C:/Users/Hemming/Dropbox/Poaceae/Results/rasters/predicted")
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  spp <- as.data.frame(c.stack, na.rm = F)
  setwd("C:/Users/Hemming/Dropbox/Poaceae")
  
# r2 ----------------------------------------------------------------------
  adj_r2 <- function(obs_raster, pred_raster){
    y <- getValues(obs_raster)
    yhat <- getValues(pred_raster)
    r_squared = 1 - sum((y - yhat)^2, na.rm = T) / sum((y - mean(y, na.rm = T))^2, na.rm = T)
    r_squared
    
  # adjusted r2
    n <- length(na.omit(y))
    p <- 9
    adj_r_squared = 1 - (1 - r_squared) * ((n - 1)/(n - p - 1))
    return(round(adj_r_squared, 2)) 
    } # ajd_r2 end
    
# run 
  r2 <- data.frame(model = c("n_c3",    "n_c4",    "n_tot",
                             "nn_c3n",  "nn_c4n",  "nn_totn",
                             "nn_c3nn", "nn_c4nn", "nn_totnn"),
                   adj_r2 = NA)

# comparing native species as the predicted raster for nonnatives    
  r2$adj_r2[1] <- adj_r2(native_C3,    native_C3_predicted)
  r2$adj_r2[2] <- adj_r2(native_C4,    native_C4_predicted)
  r2$adj_r2[3] <- adj_r2(native_total, native_total_predicted)  
  
  r2$adj_r2[4] <- adj_r2(nonnative_C3,    native_C3_predicted) 
  r2$adj_r2[5] <- adj_r2(nonnative_C4,    native_C4_predicted)
  r2$adj_r2[6] <- adj_r2(nonnative_total, native_total_predicted)  
  
# comparing nonnative species as the predicted raster for nonnatives    
  r2$adj_r2[7] <- adj_r2(nonnative_C3,    nonnative_C3_predicted) 
  r2$adj_r2[8] <- adj_r2(nonnative_C4,    nonnative_C4_predicted)
  r2$adj_r2[9] <- adj_r2(nonnative_total, nonnative_total_predicted)  
  
  r2
  
  write.csv(r2, "Results/csv/r2.csv", row.names = F)
  
# -------------------------------------------------------------------------
 

  
   