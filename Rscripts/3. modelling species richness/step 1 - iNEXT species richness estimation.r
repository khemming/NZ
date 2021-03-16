
# library -------------------------------------------------------------------------
  library(raster)
  library(rgdal)
  library(iNEXT)
  library(tidyverse)

  rm(list = ls())

# data ----------------------------------------------------------------------------
# New Zealand 
  nz_raster <- raster("Data files/NZ/NZ 83.grd")
  plot(nz_raster)
# NZ records
  nz_c3 <- readRDS("Data files/ALA/master data/master data.RDS") %>%
           filter(pp == "C3",
                  country == "NZ")
  nz_c4 <- readRDS("Data files/ALA/master data/master data.RDS") %>%
           filter(pp == "C4",
                  country == "NZ")
  
# Australia
  aus_raster <- raster("Data files/Australia/Australia 1141.grd")
  plot(aus_raster)

# Australian records
  aus_c3 <- readRDS("Data files/ALA/master data/master data.RDS") %>%
    filter(pp == "C3",
           country == "Aus")
  aus_c4 <- readRDS("Data files/ALA/master data/master data.RDS") %>%
    filter(pp == "C4",
           country == "Aus")
  
# iNEXT function ------------------------------------------------------------------
# includes function to do rarefaction using both coverage and size rarefaction
# specify minimum number of records, which is used as the size to rarify to in the size rarefaction (default = 15)
# specifiy coverage to rarify to (default = 0.8)
  inext <- function(dat, raster, min.rec = 15, coverage = 0.8) {

# assign each point in the dataframe to raster cell
  xy <- cbind(dat$longitude, dat$latitude)
  dat$cell <- raster::extract(raster, xy)
                                       
# number of records per cell
  nr <- dat %>%
        group_by(cell) %>%
        summarise(n.rec = n()) %>%
        filter(!is.na(cell))
        
  dat <- full_join(dat, nr)

# filter by min.rec and extract number of records of each species in each cell
  cr <- dat %>%
        ungroup() %>%
        filter(n.rec >= min.rec) %>%
        mutate(species = factor(species)) %>%
        group_by(species, cell) %>%
        summarise(n = n()) 
        
# get a list of the occupied cell numbers
  cell.list <- as.numeric(as.character(levels(factor(cr$cell))))
  
# store coverage output
  out_cov <- numeric()
# store size output
  out_size <- numeric()
# check for warning
  out_warn <- numeric()
  
# do the rarefaction cell by cell  
    for(i in 1:length(cell.list)) {
      td <- data.frame(spp = cr$n[cr$cell == cell.list[i]])
      td <- td[!is.na(td$spp), ]
  
# coverage rarefaction using iNEXT function
# check for warning
      out_warn[i] <- 0
      temp1 <- tryCatch(estimateD(td, datatype = "abundance", base = "coverage", level = coverage, conf = NULL), warning = function(w) {out_warn[i] <<- 1})
      
# if there was a warning save again
      if(out_warn[i] == 1) temp1 <- estimateD(td, datatype = "abundance", base = "coverage", level = coverage, conf = NULL)
      out_cov[i] <- temp1[, 4]
  
# size rarefaction using iNEXT function
  temp2 <- estimateD(td, datatype = "abundance", base = "size", level = min.rec, conf = NULL)
  out_size[i] <- temp2[, 4]
}
  
# put the rarefaction estimates into the raster
# need to include the missing cell values as well as the occupied cells
  cell_cov <- rep(NA, length(getValues(raster)))
  cell_size <- rep(NA, length(getValues(raster)))
  cell_cov_warn <- rep(NA, length(getValues(raster)))

# add the occupied cells
  cell_cov[cell.list] <- out_cov
  cell_size[cell.list] <- out_size

# coverage estimates with warning cells set to NA
  cell_cov_warn[cell.list] <- out_warn
  out_cov_warn <- ifelse(out_warn == 1, NA, out_cov)
  cell_cov_warn[cell.list] <- out_cov_warn

# generate the raster object for estimated richness  
  rast_cov <- setValues(raster, cell_cov)
  rast_size <- setValues(raster, cell_size)

# coverage raster with warning cells set to NA
  rast_cov_warn <- setValues(raster, cell_cov_warn)
  
# number of records per cell
  nrec <- rep(NA, length(getValues(raster)))
  nrec[nr$cell] <- nr$n.rec
  
# raw species richness
  spp_per_cell <- as.numeric(factor(dat$species))
  n_spp <- rasterize(xy, raster, field = spp_per_cell, fun = function(x,...) {length(unique(na.omit(x))) })
  m_spp <- mask(n_spp, raster)
  plot(m_spp)
  
#return the values for each cell and the rasters
  return(list(cell_cov, rast_cov, cell_size, rast_size, nrec, cell_cov_warn, rast_cov_warn, m_spp))
  
}
# -----------------------------------------------------------------------------------

# run & save function ---------------------------------------------------------
#notes -----------------------------------------------------------------------------
#there are lists for each family x status
#these 8 lists as follows: [[1]] df sr calculated by coverage - w warning cells
#[[2]] raster sr calculated by coverage - w warning cells
#[[3]] df sr calculated by size (i.e. 15-rec rarefaction)           
#[[4]] raster sr calculated by size (i.e. 15-rec rarefaction)
#[[5]] df number of records per cell
#[[6]] df coverage w warning cells removed
#[[7]] raster coverage w warning cells removed
#[[8]] raster raw species richness
  
# I am going to save [[7]] into raster
# the others will go into dedicated CSVs
# ------------------------------------------------------------------------------------    
# writing and saving function (status and pathway)
# relies on species and raster data and iNEXT function
  inext_save <- function(dat, country, origin, pathway, raster) {
    
    dat_origin <- dat %>% filter(status == origin)
    
    x <- inext(dat_origin, raster)  
    par(mfrow = c(2, 2))
    
  # plot records
    nrec <- setValues(raster, x[[5]])
    plot(nrec)
  # plot raw sr
    plot(x[[8]]) 
  # plot iNEXT  warining cells retained
    plot(x[[2]])   
  # plot iNEXT  warining cells removed
    plot(x[[7]])   
    
  # save raster
    rasterfile <- paste0("Results/rasters/iNEXT/", country, "_", origin, "_", pathway, ".grd")
    writeRaster(x[[7]],
                filename = rasterfile,
                overwrite = T)
    
  # save data frame  
    records <- x[[5]]
    raw.richness <- getValues(x[[8]])
    rarefaction <- x[[3]]
    inext.w.warnings <- x[[1]]
    inext <- getValues(x[[7]]) 
    
    df <- cbind(records, 
                raw.richness,
                rarefaction,
                inext.w.warnings,
                inext)
    head(df)
    csvfile <- paste0("Results/csv/species richness/", country, "_", origin, "_", pathway, ".csv")
    write.csv(df, csvfile)
    
  }
 
# ------------------------------------------------------------------------------------  
  
# run
  inext_save(nz_c3, "NZ", "native", "C3", nz_raster)
  inext_save(nz_c4, "NZ", "native", "C4", nz_raster)
  inext_save(nz_c3, "NZ", "nonnative", "C3", nz_raster)
  inext_save(nz_c4, "NZ", "nonnative", "C4", nz_raster)
  
  inext_save(aus_c3, "Aus", "native", "C3", aus_raster)
  inext_save(aus_c4, "Aus", "native", "C4", aus_raster)
  
  inext_save(aus_c3, "Aus", "nonnative", "C3", aus_raster)
  inext_save(aus_c4, "Aus", "nonnative", "C4", aus_raster)
  

# -------------------------------------------------------------------------------------    
  
  