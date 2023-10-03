
# run these for the most up to date packages
# install.packages('terra', repos='https://rspatial.r-universe.dev')
# install.packages("remotes")
# remotes::install_github("rspatial/geodata")
  library(geodata)
  library(tidyverse)
  library(terra)
  library(sf)
  library(mgcv)
  
  # source("RScripts/7. GAM/01-create_aus_raster.R")
  # source("RScripts/7. GAM/02-create_nz_raster.R")
  # source("RScripts/7. GAM/03-estimate_species_richness.R")

#-------------------------------------------------------------------------------

# read in nz environment raster
  env_nz <- rast("Data files/predictor variables/Environment/Environment raster NZ.tif")
  crs(env_nz) <- crs(env_nz) <- "+init=epsg:2193 +proj=merc 
              +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 
              +units=m +no_defs +lat_0=0 +lon_0=173 +k=0.9996"
  head(env_nz)
  names(env_nz)

# template raster based on env raster
  temp_nz <- rast(extent = ext(env_nz), resolution = res(env_nz))
  crs(temp_nz) <- "+init=epsg:2193 +proj=merc 
              +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 
              +units=m +no_defs +lat_0=0 +lon_0=173 +k=0.9996"
  temp_nz
  
# outline
  nz <- geodata::gadm(country = "NZ", res = 1, level = 0, path = "Data files/NZ/")
  crs(nz) <- "+init=epsg:2193 +proj=merc 
              +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 
              +units=m +no_defs +lat_0=0 +lon_0=173 +k=0.9996"
  plot(nz)
  
# simplify outline
  nz2 <- terra::simplifyGeom(nz, tolerance = .02, preserveTopology = T)
  nz_sh <- terra::crop(nz2, temp_nz)
  terra::plot(nz_sh, lwd = 2)
 

#-------------------------------------------------------------------------------

# read in richness data from iNEXT
  nat_C3 <- read.csv("Results/csv/species richness/nz_nat_c3.csv")
  nat_C4 <- read.csv("Results/csv/species richness/nz_nat_c4.csv")
  exo_C3 <- read.csv("Results/csv/species richness/nz_nonnat_c3.csv")
  exo_C4 <- read.csv("Results/csv/species richness/nz_nonnat_c4.csv")

# total number of records per cell
  tot <- rbind(dplyr::select(nat_C3, cell_id, n.rec),
               dplyr::select(nat_C4, cell_id, n.rec),
               dplyr::select(exo_C3, cell_id, n.rec),
               dplyr::select(exo_C4, cell_id, n.rec))

  tot <- tot %>%
    group_by(cell_id) %>%
    summarise(tot.rec = sum(n.rec))

# range of records per cell
  range(tot$tot.rec)
  
  nat_C3 <- left_join(nat_C3, tot)
  nat_C4 <- left_join(nat_C4, tot)
  exo_C3 <- left_join(exo_C3, tot)
  exo_C4 <- left_join(exo_C4, tot)

#-------------------------------------------------------------------------------------
# function to generate a raster with filtered records
  genras <- function(dat) {
    dat <- dat %>%
      filter(tot.rec >= 10 & warn == 0)
    
    val <- temp_nz
    val[dat$cell_id] <- dat$q0
    val[is.nan(val)] <- NA
    return(val)
  }

# raster with the richness values
  rich <- c(genras(nat_C3),genras(nat_C4), genras(exo_C3), genras(exo_C4))
  names(rich) <- c("nat_C3", "nat_C4", "exo_C3", "exo_C4")
  rich

# clip to nz
  cov <- rasterize(nz_sh, temp_nz, cover = T)
  
# remove cells with proportion land cover less than cv
  cv <- 0.25
  rem <- values(cov$layer)[, 1] == "NaN" | values(cov$layer)[, 1] < cv
# set values with less than cv land cover to NA
for(i in 1:length(names(rich))) {
  v <- values(rich[[i]])[, 1]
  v[rem] <- NA
  rich[[i]] <- setValues(rich[[i]], v)
  }

# full raster
  env_nz <- c(env_nz, rich)
  names(env_nz)

# create data frame
  env <- as.data.frame(env_nz, cells = T, na.rm = F)

# convert NaN to NA
  for(i in 2:ncol(env)) {
    x <- env[, i]
    x[is.nan(x)] <- NA
    env[, i] <- x
  }

# add lats and longs
  ll <- crds(env_nz, na.rm = F)
  env$long <- ll[, 1]
  env$lat <- ll[, 2]
  
  names(env)[1] <- "cell_id"
  head(env)
  
  par(mfrow = c(3, 3))
  for(i in 2:8) {
    hist(env[, i], main = names(env)[i])
  }

# missing environment values
  n.miss <- table(nm <- apply(env, 1, function(x) sum(is.na(x[2:9]))))
  n.miss

# numbers of observations
  table(!is.na(env$nat_C3))
  table(!is.na(env$exo_C3))
  table(!is.na(env$nat_C4))
  table(!is.na(env$exo_C4))

##############################################################################
# correlations split by hii
# quantile to split hii into high and low
  lim <- quantile(env$hii, 0.9, na.rm = T)
  lim
  
  envh1 <- filter(env, hii >= lim)
  
# n
  sum(!is.na(envh1$nat_C3))
  sum(!is.na(envh1$exo_C3))
  sum(!is.na(envh1$nat_C4))
  sum(!is.na(envh1$exo_C4))
  
# mean richness
  mean(envh1$nat_C3, na.rm = T)
  mean(envh1$exo_C3, na.rm = T)
  mean(envh1$nat_C4, na.rm = T)
  mean(envh1$exo_C4, na.rm = T)

cr <- data.frame( 
  var = names(env[2:8]),
  natc3 = as.vector(cor(envh1$nat_C3, envh1[, 2:8], method = "spearman", 
                        use = "pairwise.complete.obs")),
  exoc3 = as.vector(cor(envh1$exo_C3, envh1[, 2:8], method = "spearman", 
                        use = "pairwise.complete.obs")),
  natc4 = as.vector(cor(envh1$nat_C4, envh1[, 2:8], method = "spearman", 
                        use = "pairwise.complete.obs")),
  exoc4 = as.vector(cor(envh1$exo_C4, envh1[, 2:8], method = "spearman", 
                        use = "pairwise.complete.obs")))

cr[, 2:5] <- round(cr[, 2:5], 2)
cr

cor(cr[, 2:3])
cor(cr[, 4:5])

##############################################################################
# richness relationships
  pdf("Results/figures/Figure 1 NZ.pdf")
  
  cl <- ifelse(env$hii >= lim, rgb(0.6, 0, 0, 0.5), rgb(0, 0, 0.6, 0.5))
  par(mfrow = c(3, 2), mar = c(4, 4, 3, 1))
  
  plot(rich$nat_C3, main = "Native C3 richness")
  plot(nz_sh, add = T)
    mtext("A", line = 0.5, adj = 0, cex = 1.2)
  
  plot(rich$nat_C4, main = "Native C4 richness")
  plot(nz_sh, add = T)
  mtext("B", line = 0.5, adj = 0, cex = 1.2)
  
  plot(rich$exo_C3, main = "Exotic C3 richness")
  plot(nz_sh, add = T)
  mtext("C", line = 0.5, adj = 0, cex = 1.2)
  
  plot(rich$exo_C4, main = "Exotic C4 richness")
  plot(nz_sh, add = T)
  mtext("D", line = 0.5, adj = 0, cex = 1.2)

# function to plot correlations
  pc <- function(v1, v2, let, xpos, ps) {
    y1 <- v1[env$hii >= lim]
    x1 <- v2[env$hii >= lim]
    m1 <- lm(log(y1) ~ log(x1))
    d1 <- data.frame(x1 = seq(min(x1, na.rm = T), max(x1, na.rm = T), 0.1))
  
    y2 <- v1[env$hii < lim]
    x2 <- v2[env$hii < lim]
    m2 <- lm(log(y2) ~ log(x2))
    d2 <- data.frame(x2 = seq(min(x2, na.rm = T), max(x2, na.rm = T), 0.1))
  
    pred1 <- predict(m1, newdata = d1)                   
    pred2 <- predict(m2, newdata = d2)                   
    
    cr <- cor(log(v1), log(v2), use = "complete.obs")
      
    plot(v1 ~ v2, pch = 19, col = cl, cex = 0.7, main = paste("r = ", round(cr, 2)),
         ylab = paste0("Exotic C", ps, " richness"), 
         xlab = paste0("Native C", ps, " richness"), bty = "l")
    lines(exp(pred1) ~ d1$x, col = rgb(0.6, 0, 0, 1), lwd = 2)
    lines(exp(pred2) ~ d2$x, col = rgb(0, 0, 0.6, 1), lwd = 2)
    text(xpos, max(exp(pred1)+2), 
         round(cor(log(v1[env$hii >= lim]), log(v2[env$hii >= lim]), use = "complete.obs"), 2), 
         xpd = NA, pos = 4, 
         col = rgb(0.6, 0, 0, 1), cex = 1.1)
    text(xpos, max(exp(pred2)), 
         round(cor(log(v1[env$hii < lim]), log(v2[env$hii < lim]), use = "complete.obs"), 2), 
         xpd = NA, pos = 4, 
         col = rgb(0, 0, 0.6, 1), cex = 1.1)
    mtext(let, line = 0.5, adj = 0, cex = 1.2)
    return(list(m1, m2))
    
  }

  par(mar = c(5, 5, 3, 5))
  a <- pc(env$exo_C3, env$nat_C3, "E", 51, "3")
  b <- pc(env$exo_C4, env$nat_C4, "F", 160, "4")
  
  dev.off()

################################################################################
# Figure showing areas with high hii
  hi <- temp_nz
  hi[env$cell_id] <- ifelse(env$hii >= lim, 1, 0)
  
  pdf("Results/figures/Figure S1 NZ.pdf")
  
  plot(hi, legend = F)
  plot(nz_sh, add = T)
  
  dev.off()
  
  table(ifelse(env$hii >= lim, 1, 0))

################################################################################

  
# function to fit model
  fit <- function(var) {
    dat <- env
    dat$rich <- var 
    m_formula <- formula(rich ~ area + amt + arid + ts + pwarmq +
                                pcoldq + th + hii)
    mod <- lm(m_formula, data = dat)

    return(mod)
  }
  

  m_nat_C3_nz <- fit(log(env$nat_C3))
  m_nat_C4_nz <- fit(log(env$nat_C4))
  m_exo_C3_nz <- fit(log(env$exo_C3))
  m_exo_C4_nz <- fit(log(env$exo_C4))

# new raster to store the modelled results
# predict to all cells
  predto <- env[, c(2:9, 14:15)]
  
  pr <- function(x) {
    pred_x <- exp(predict(x, newdata = predto))
    r <- temp_nz
    # val <- as.vector(pred_x / max(pred_x, na.rm = T))
    val <- as.vector(pred_x)
    values(r) <- val
    r <- setValues(r, val)
    return(r)
  }
  
  pred_nat_C3 <- pr(m_nat_C3_nz)
  pred_nat_C4 <- pr(m_nat_C4_nz)
  pred_exo_C3 <- pr(m_exo_C3_nz)
  pred_exo_C4 <- pr(m_exo_C4_nz)
  
  mod <- c(pred_nat_C3, pred_nat_C4, pred_exo_C3, pred_exo_C4)
  names(mod) <- c("nat_C3", "nat_C4", "exo_C3", "exo_C4")
  mod
  
# get the differences
  mod$dif_C3 <- mod$nat_C3 - mod$exo_C3
  mod$dif_C4 <- mod$nat_C4 - mod$exo_C4
  
# plot

  pdf("Results/figures/Figure 2 NZ.pdf")
  
  par(mfrow = c(3, 2), mar = c(2, 4, 4, 1))
  
  plot(mod$nat_C3, main = "Native C3 richness")
  plot(nz_sh, add = T)
  mtext("A", line = 0.5, adj = 0, cex = 1.2)
  
  plot(mod$nat_C4, main = "Native C4 richness", legend = F)
  plot(nz_sh, add = T)
  mtext("B", line = 0.5, adj = 0, cex = 1.2)
  
  plot(mod$exo_C3, main = "Exotic C3 richness", legend = F)
  plot(nz_sh, add = T)
  mtext("C", line = 0.5, adj = 0, cex = 1.2)
  
  plot(mod$exo_C4, main = "Exotic C4 richness", legend = F)
  plot(nz_sh, add = T)
  mtext("D", line = 0.5, adj = 0, cex = 1.2)
  
# max and min values for legend
  mm <- range(c(values(mod$dif_C3)[, 1], values(mod$dif_C4)[, 1]), na.rm = T)
  
  plot(mod$dif_C3, col = grDevices::hcl.colors(50, "Blue-Red 2"), range = mm, 
       main = "C3 difference")
  plot(nz_sh, add = T)
  mtext("E", line = 0.5, adj = 0, cex = 1.2)
  
  plot(mod$dif_C4, col = grDevices::hcl.colors(50, "Blue-Red 2"), range = mm, 
       legend = F, main = "C4 difference")
  plot(nz_sh, add = T)
  mtext("F", line = 0.5, adj = 0, cex = 1.2)
  
  dev.off()

###############################################################################
  pdf("Results/figures/Figure 3 NZ.pdf")
  
  par(mfcol = c(3, 2), mar = c(4, 4, 3, 1))
  
  c3 <- values(mod$dif_C3)[, 1]
  c4 <- values(mod$dif_C4)[, 1]
  yl <- min(c(c3, c4), na.rm = T)
  yu <- max(c(c3, c4), na.rm = T)
  br <- seq(yl, yu, length.out = 15)
  
  hist(c3, main = "C3", breaks = br, xlab = "")
    abline(v = 0, lwd = 2)
    mtext("A", line = 0.5, adj = 0, cex = 1.2)
  hist(c4, breaks = br, main = "C4",
       xlab = "Difference in scaled richness (native - non-native)")
    abline(v = 0, lwd = 2)
    mtext("B", line = 0.5, adj = 0, cex = 1.2)
    
  dev.off()  
################################################################################
# model comparisons
    
# r2 values
    
  snc3 <- summary(m_nat_C3_nz)  
  snc4 <- summary(m_nat_C4_nz)  
  sec3 <- summary(m_exo_C3_nz)  
  sec4 <- summary(m_exo_C4_nz)  
    
  r2 <- data.frame(model = c("nat_C3", "nat_C4", "exo_C3", "exo_C4"),
                   r.sq = c(snc3$r.sq, snc4$r.sq, sec3$r.sq, sec4$r.sq))
  
  r2
    
# recode this to be like a data frame of coefficient values --------------------  
  # sum_nat_C3 <- data.frame(chisq = snc3$chi.sq,
  #                      pval = snc3$s.pv,
  #                      edf = snc3$edf)
  # 
  # sum_exo_C3 <- data.frame(chisq = sec3$chi.sq,
  #                          pval = sec3$s.pv,
  #                          edf = sec3$edf)
  # 
  # round(sum_nat_C3, 3)
  # round(sum_exo_C3, 3)
  # 
  # 
  # sum_nat_C4 <- data.frame(chisq = snc4$chi.sq,
  #                          pval = snc4$s.pv,
  #                          edf = snc4$edf)
  # 
  # sum_exo_C4 <- data.frame(chisq = sec4$chi.sq,
  #                          pval = sec4$s.pv,
  #                          edf = sec4$edf)
  # 
  # round(sum_nat_C4, 3)
  # round(sum_exo_C4, 3)
  
# requirements for NZ-Aus modelling script -------------------------------------
  nz_r2 <- r2
# obs richness
  nz_rich <- rich
# modelled richness  
  nz_mod <- mod
# model 
  nz_fit <- fit
# predict function
  nz_pr <- pr
# NZ spp and env data
  nz_vars <- env
# NZ env only
  nz_env <- env %>% select(cell_id, amt, arid, pcoldq, pwarmq, ts, th, hii, 
                           area, long, lat)

  save.image("ESA.rdata")
  load("ESA.rdata")
  