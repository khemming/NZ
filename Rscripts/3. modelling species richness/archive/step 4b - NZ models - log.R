

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
# NZ raster names
  setwd("C:/Users/s436862/Dropbox/NZ/Results/rasters/iNEXT")
  files_list <- list.files(pattern = ".grd")
  nz_list <- Filter(function(x) grepl("NZ_", x), files_list)[1:3]
  spp <- gsub(pattern = "\\.grd$", "", nz_list)
  setwd("C:/Users/s436862/Dropbox/NZ")

# predictor variables
  pv <- read.csv("Results/csv/predictor variables/NZ predictor variables 288.csv")
  
# species richness
  NZ_native_C3 <- read.csv("Results/csv/species richness/NZ_native_C3.csv") %>% 
                  dplyr::select(inext) %>%
                  rename(NZ_native_C3 = inext)
  NZ_native_C4 <- read.csv("Results/csv/species richness/NZ_native_C4.csv") %>% 
                  dplyr::select(inext) %>%
                  rename(NZ_native_C4 = inext)
  
  NZ_nonnative_C3 <- read.csv("Results/csv/species richness/NZ_nonnative_C3.csv") %>% 
                     dplyr::select(inext) %>%
                     rename(NZ_nonnative_C3 = inext)
 
# species predictor variable data frame (spv)
  spv <- bind_cols(NZ_native_C3, NZ_native_C4, NZ_nonnative_C3, pv)
  head(spv)

# identify spatial auto-correlation --------------------------------------------------------------
# store all results for supplementary materials
  moran_l <- list()             # Moran's I spatial autocorrelation test
  gls_l <- list()               # general least squares parameter estimates
  cor_str <- matrix(nrow = 3)   # identifies best model from model selection
  model_list <- list()          # stores best models

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
    
    moran_l[[i]] <- moran_fun(spv[, i], i) # error for nonnative C4 grasses
    
  }
  
  names(moran_l) <- spp
  moran_l 
  
# Moran's I data frame for saving: 4 x 6
  m_mat <- round(matrix(unlist(moran_l), byrow = T, nrow = length(spp)), 2)
  row.names(m_mat) <- spp
  colnames(m_mat) <- c("observed","expected", "sd", "p.value")
  m_mat # all are autocorrelated

# model selection --------------------------------------------------------------------------------
# test different methods for modelling spatial autocorrelation
  model_sel_fun <- function(spp_col) {
  # model methods to account for spatial autocorrelation
    model_e <- gls(spp_col ~ hii + th  + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                   data = spv, correlation = corExp(form = ~long + lat, nugget=T) , na.action = na.omit, method = "ML")
    model_g <- gls(spp_col ~ hii + th  + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                   data = spv, correlation = corGaus(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
    model_s <- gls(spp_col ~ hii + th  + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                   data = spv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
    model_r <- gls(spp_col ~ hii + th  + pcoldq + pwarmq + ts + arid + amt + proportion_cover,
                   data = spv, correlation = corRatio(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
    model_lm <- lm(spp_col ~ hii + th  + pcoldq + pwarmq + ts + arid + amt + proportion_cover,
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
  cor_str # no data indicates lm were best fits
  
# save all gls models
  gls_mat <- matrix(nrow = 5, ncol = 54) # ncol = 18 * no. of species
  
  gls_mat[,] <- unlist(gls_l, recursive = T)
  gls_mat2 <- rbind(gls_mat[, 1:18],  
                    gls_mat[, 19:36],  
                    gls_mat[, 37:54])
  
  colnames(gls_mat2) <- colnames(gls_l[[1]])
  rownames(gls_mat2) <- rep(spp, each = 5)

# run lowest-AIC models ------------------------------------------------------------------
# linear models (because they were the best in all cases)
  model_list[[1]] <- lm(NZ_native_C3 ~ hii + th  + pcoldq + pwarmq + ts + arid + amt + proportion_cover, data = spv)
  model_list[[2]] <- lm(NZ_nonnative_C3 ~ hii + th  + pcoldq + pwarmq + ts + arid + amt + proportion_cover, data = spv)
  model_list[[3]] <- lm(NZ_native_C4 ~ hii + th  + pcoldq + pwarmq + ts + arid + amt + proportion_cover, data = spv)

# wrangle parameter estimates into plot-ready tables
# parameter data frame for 3 groups of spp. [nonnative C4 nonexistent]
  nzc3_a <- data.frame(pv = rep(c("hii", "th",   "pcoldq", "pwarmq", 
                                  "ts",  "arid", "amt"),   2))
  nzc4_a <- data.frame(pv = rep(c("hii", "th",   "pcoldq", "pwarmq", 
                                  "ts",  "arid", "amt")))
  
# native NZ native and nonnative C3; then native C4  
  nzc3_b <- nzc3_a %>% mutate(country = rep("NZ", 14),
                              status = c(rep("Native", 7), rep("Nonnative", 7)),
                              lower = NA,
                              estimate = NA,
                              upper = NA)
  nzc3_b
  nzc4_b <- nzc4_a %>% mutate(country = rep("NZ", 7),
                              status = rep("Native", 7),
                              lower = NA,
                              estimate = NA,
                              upper = NA)
  nzc4_b

# attain relevant parameter estimates and confidence intervals
# lower CI
  nzc3_b[nzc3_b$status == "Native", "lower"] <- confint(model_list[[1]])[2:8, 1]    # native C3
  nzc3_b[nzc3_b$status == "Nonnative", "lower"] <- confint(model_list[[2]])[2:8, 1] # nonnative C3
  nzc4_b[nzc4_b$status == "Native", "lower"] <- confint(model_list[[3]])[2:8, 1]    # native C4
  
# mean estimate
  nzc3_b[nzc3_b$status == "Native", "estimate"] <- model_list[[1]]$coefficients[2:8]
  nzc3_b[nzc3_b$status == "Nonnative", "estimate"] <- model_list[[2]]$coefficients[2:8]
  nzc4_b[nzc3_b$status == "Native", "estimate"] <- model_list[[3]]$coefficients[2:8]

# upper CI  
  nzc3_b[nzc3_b$status == "Native", "upper"] <- confint(model_list[[1]])[2:8, 2]
  nzc3_b[nzc3_b$status == "Nonnative", "upper"] <- confint(model_list[[2]])[2:8, 2]
  nzc4_b[nzc4_b$status == "Native", "upper"] <- confint(model_list[[3]])[2:8, 2]
 
  nzc3_b
  nzc4_b

# save data ------------------------------------------------------------------
  write.csv(m_mat, "Results/csv/models/iNEXT NZ Morans I.csv", row.names = T)
  write.csv(gls_mat2, "Results/csv/models/iNEXT NZ GLS model structures.csv", row.names = T)
  
  write.csv(nzc3_b, "Results/csv/models/iNEXT NZ C3 mean estimates.csv", row.names = F)
  write.csv(nzc4_b, "Results/csv/models/iNEXT NZ C4 mean estimates.csv", row.names = F)
  
  save.image("data files/rdata/iNEXT NZ models.RData")

# ----------------------------------------------------------------------------
