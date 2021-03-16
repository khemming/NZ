
###############################################################################
# New Zealand and Australia predicted richness
###############################################################################


# library ------------------------------------------------------------------
  library(dplyr)
  library(broom)
  library(magrittr)
  library(raster)
  
  rm(list = ls())

# data ---------------------------------------------------------------------
  load("Data files/rdata/model_data.RData") 
  
# NZ and Aus predictor varaibles
  nz_pv_dat <- nz_pv %>% filter(cell_category == "land") %>%
                  dplyr::select(-cell_category, -cell_id)
  aus_pv_dat <- aus_pv %>% filter(cell_category == "land") %>%
    dplyr::select(-cell_category, -cell_id)

# NZ raster
  nz <- raster("Data files/New Zealand/New Zealand 100 km.grd")
  
# cell ID
  nz_cell_id <- nz_pv %>% filter(cell_category == "land") %>%
                   dplyr::select(cell_id)
  aus_cell_id <- aus_pv %>% filter(cell_category == "land") %>%
    dplyr::select(cell_id)
  
# predict distributions ----------------------------------------------------
# NZ C3
  nz_c3_pred <- predict(m_nz_nc3, newdata = nz_pv_dat) 
# NZ C4
  nz_c4_pred <- predict(m_nz_nc4, newdata = nz_pv_dat) 
  
# Aus C3
  aus_c3_pred <- predict(m_aus_nc3, newdata = nz_pv_dat)
# Aus C4
  aus_c4_pred <- predict(m_aus_nc4, newdata = nz_pv_dat) 
  
# predictive model rasters and data frame ------------------------------------
# bind predicted distributions and add cell id
  pred_distr <- cbind(nz_cell_id, nz_c3_pred,  nz_c4_pred,
                                  aus_c3_pred, aus_c4_pred)
  
# generate list of occupied cells
  cell_list <- pred_distr$cell_id

# make a matrix with all missing values
  x <- matrix(NA, nrow = length(getValues(nz)), ncol = 4)
  pred_distr_m <- as.matrix(pred_distr)
  pred_distr_mm <- pred_distr_m[, 2:5]
  class(pred_distr_mm) <- "numeric"
  
# add the occupied cells
  x[cell_list, ] <- pred_distr_mm
  
# producing predictive rasters of NZ ----------------------------------------
# note: we are truncating negative richness values to zero
# NZ C3
  p_nz_c3 <- setValues(nz, x[, 1])
  plot(p_nz_c3)

  p_nz_c3_calc <- calc(p_nz_c3, fun = function(x) {x[x<0] <- 0; return(x)})
  plot(p_nz_c3_calc)
  p_nz_c3_calc
  writeRaster(p_nz_c3_calc, "Results/rasters/scaled/NZ/NZ_predicted_C3.grd", overwrite = T)
  
# NZ C4  
  p_nz_c4 <- setValues(nz, x[, 2])
  plot(p_nz_c4)
  
  p_nz_c4_calc <- calc(p_nz_c4, fun = function(x) {x[x<0] <- 0; return(x)})
  plot(p_nz_c4_calc)
  p_nz_c4_calc
  writeRaster(p_nz_c4_calc, "Results/rasters/scaled/NZ/NZ_predicted_C4.grd", overwrite = T)

# 'Straya C3
  p_aus_c3 <- setValues(nz, x[, 3])
  plot(p_aus_c3)
  
  p_aus_c3_calc <- calc(p_aus_c3, fun = function(x) {x[x<0] <- 0; return(x)})
  plot(p_aus_c3_calc)
  p_aus_c3_calc
  writeRaster(p_aus_c3_calc, "Results/rasters/scaled/NZ/Aus_predicted_C3.grd", overwrite = T)
  
# 'Straya C4
  p_aus_c4 <- setValues(nz, x[, 4])
  plot(p_aus_c4)
  
  p_aus_c4_calc <- calc(p_aus_c4, fun = function(x) {x[x<0] <- 0; return(x)})
  plot(p_aus_c4_calc)
  p_aus_c4_calc
  writeRaster(p_aus_c4_calc, "Results/rasters/scaled/NZ/Aus_predicted_C4.grd", overwrite = T)
    
# -------------------------------------------------------------------------------  
