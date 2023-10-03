

# library ------------------------------------------------------------------
  library(tidyverse)
  library(broom)
  library(magrittr)
  library(MuMIn)
  library(ape)
  library(nlme)  
  library(car)
  library(rgdal)
  library(lmtest)
  library(gvlma)
  
  rm(list = ls())
 
# data ---------------------------------------------------------------------   
# Aus raster names
  setwd("C:/Users/s436862/Dropbox/NZ/Results/rasters/log")
  files_list <- list.files(pattern = ".grd")
  aus_list <- Filter(function(x) grepl("Aus_", x), files_list)
  aus_stack <- stack(aus_list)
  spp <- gsub(pattern = "\\.grd$", "", aus_list)
  setwd("C:/Users/s436862/Dropbox/NZ")
  
# predictor variables
  pv <- read.csv("Results/csv/predictor variables/Australia predictor variables 2538.csv")
  
# species richness (log of iNEXT)
  spp_df <- data.frame(getValues(aus_stack))
  names(spp_df) <- spp
  
# species predictor variable data frame (spv)
  spv <- bind_cols(spp_df, pv) %>%
         filter(cell_category == "land")
  head(spv)
  

# outputs --------------------------------------------------  
# store all results for supplementary materials
  moran_l <- list()             # Moran's I spatial autocorrelation test
  gls_l <- list()               # general least squares parameter estimates
  cor_str <- matrix(nrow = length(spp))   # identifies best model from model selection
  model_list <- list()          # stores best models
  ci_list <- list()             # stores gls vonfidence intervals
  
# identify spatial auto-correlation --------------------------------------------------------------
# identify spatial autocorrelation function (returns p-value)
  moran_fun <- function(spp_col, col_no) {
    xy <- spv %>% filter(!is.na(spp_col)) %>%
      dplyr::select(all_of(col_no), long, lat)
    coords = cbind(xy$long, xy$lat)
    w = fields:::rdist(coords)
    m_i <- Moran.I(x = xy[, 1], w = w, na.rm = T)
  return(m_i)
  
  }

# run 
  for (i in 1:length(spp)){
    
    moran_l[[i]] <- moran_fun(spv[, i], i)
    
  }
  
  names(moran_l) <- spp
  moran_l 
  
# Moran's I data frame for saving: 4 x 6
  m_mat <- round(matrix(unlist(moran_l), byrow = T, nrow = length(spp)), 2)
  row.names(m_mat) <- spp
  colnames(m_mat) <- c("observed","expected", "sd", "p.value")
  m_mat # all spp have signif. autocorrelation

# model selection --------------------------------------------------------------------------------
# test different methods for modelling spatial autocorrelation
  model_sel_fun <- function(spp_col) {
  # model methods to account for spatial autocorrelation
    model_e <- gls(spp_col ~ proportion_cover + hii + th + pcoldq + pwarmq + ts + arid + amt, 
                   data = spv, correlation = corExp(form = ~long + lat, nugget=T) , na.action = na.omit, method = "ML")
    model_g <- gls(spp_col ~ proportion_cover + hii + th + pcoldq + pwarmq + ts + arid + amt, 
                   data = spv, correlation = corGaus(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
    model_s <- gls(spp_col ~ proportion_cover + hii + th + pcoldq + pwarmq + ts + arid + amt, 
                   data = spv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
    model_r <- gls(spp_col ~ proportion_cover + hii + th + pcoldq + pwarmq + ts + arid + amt,
                   data = spv, correlation = corRatio(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
    model_lm <- lm(spp_col ~ proportion_cover + hii + th + pcoldq + pwarmq + ts + arid + amt,
                   data = spv, na.action = na.omit)
  # compare models using AICc
    model_sel <- model.sel(model_e, model_g , model_s, model_r, model_lm)
    return(model_sel)
  }
  
# run and choose best model by AICc ------------------------------------------------
  for (i in 1:length(spp)){
  spp_col <- spv[, i]
  gls_l[[i]] <- model_sel_fun(spp_col)
  cor_str[i] <- gls_l[[i]]$correlation[1] # best correlation structure
  }
  cor_str

# save all gls models
  gls_mat <- matrix(nrow = 5, ncol = 72) # ncol = 18 * no. of species
  
  gls_mat[,] <- unlist(gls_l, recursive = T)
  gls_mat2 <- rbind(gls_mat[, 1:18],  
                    gls_mat[, 19:36],
                    gls_mat[, 37:54],  
                    gls_mat[, 55:72])
  
  colnames(gls_mat2) <- colnames(gls_l[[1]])
  rownames(gls_mat2) <- rep(spp, each = 5)

# run lowest-AIC models ------------------------------------------------------------------
# identified in correlation structure
  cor_str
  
# native C3 =  corSpher(form = ~long + lat, nugget=T)
  model_list[[1]] <- gls(Aus_native_C3 ~ proportion_cover + hii + th + pcoldq + pwarmq + ts + arid + amt, 
      data = spv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
  ci_list[[1]] <- data.frame(intervals(model_list[[1]], 0.95, which = "coef")$coef)
  
# native C4 =  corExp(form = ~long + lat, nugget=T)
  model_list[[2]] <- gls(Aus_native_C4 ~ proportion_cover + hii + th + pcoldq + pwarmq + ts + arid + amt, 
                         data = spv, correlation = corExp(form = ~long+lat,T), na.action = na.omit, method = "ML")
  ci_list[[2]] <- data.frame(intervals(model_list[[2]], 0.95, which = "coef")$coef)
  
# nonnative C3 =  corExp(form = ~long + lat, nugget=T)
  model_list[[3]] <- gls(Aus_nonnative_C3 ~ proportion_cover + hii + th + pcoldq + pwarmq + ts + arid + amt, 
                         data = spv, correlation = corExp(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
  ci_list[[3]] <- data.frame(intervals(model_list[[3]], 0.95, which = "coef")$coef)
  
# nonnative C4 =  corExp(form = ~long + lat, nugget=T)
  model_list[[4]] <- gls(Aus_nonnative_C4 ~ proportion_cover + hii + th + pcoldq + pwarmq + ts + arid + amt, 
                         data = spv, correlation = corExp(form = ~long+lat,T), na.action = na.omit, method = "ML")
  ci_list[[4]] <- data.frame(intervals(model_list[[4]], 0.95, which = "coef")$coef)

  names(model_list) <- spp
  names(ci_list) <- spp
  
# wrangle parameter estimates into plot-ready tables ---------------    
  model_df <- function(cis, stat, location){
    df <- cis %>% 
          mutate(pv = rownames(cis),
                  status = stat) %>%
          slice(3:9) %>%
          mutate(country = location,
                 status = as.factor(status),
                 estimate = est.) %>%
          dplyr::select(pv, country, status, lower, estimate, upper)
    return(df)
  }
  
# run   
  natausc3 <- model_df(ci_list[[1]], "Native", "AU")
  natausc3
  
  natausc4 <- model_df(ci_list[[2]], "Native", "AU")
  natausc4
  
  nonausc3 <- model_df(ci_list[[3]], "Nonnative", "AU")
  nonausc3
    
  nonausc4 <- model_df(ci_list[[4]], "Nonnative", "AU")
  nonausc4
  
# save data ------------------------------------------------------------------
  write.csv(m_mat, "Results/csv/models/Aus Morans I.csv", row.names = T)
  write.csv(gls_mat2, "Results/csv/models/Aus GLS model structures.csv", row.names = T)
  
  write.csv(natausc3, "Results/csv/models/native Aus C3 mean estimates.csv", row.names = F)
  write.csv(nonausc3, "Results/csv/models/nonnative Aus C3 mean estimates.csv", row.names = F)
  
  write.csv(natausc4, "Results/csv/models/native Aus C4 mean estimates.csv", row.names = F)
  write.csv(nonausc4, "Results/csv/models/nonnative Aus C4 mean estimates.csv", row.names = F)
  
  save.image("Data files/rdata/Aus models.RData")

# ----------------------------------------------------------------------------
