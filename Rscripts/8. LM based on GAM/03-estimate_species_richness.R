

# library ----------------------------------------------------------------------
  library(terra)
  library(iNEXT)
  library(tidyverse)

  rm(list = ls())
  
# read in environment rasters
  aus_env_rast <- rast("Data files/predictor variables/Environment/Environment raster Aus.tif")
  nz_env_rast <- rast("Data files/predictor variables/Environment/Environment raster NZ.tif")
  
# template rasters
  temp_aus <- rast(extent = ext(aus_env_rast), resolution = res(aus_env_rast))
  crs(temp_aus) <- "+init=epsg:2193 +proj=merc 
              +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 
              +units=m +no_defs +lat_0=0 +lon_0=173 +k=0.9996"
  temp_aus
  
  temp_nz <- rast(extent = ext(nz_env_rast), resolution = res(nz_env_rast))
  crs(temp_nz) <- "+init=epsg:2193 +proj=merc 
              +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 
              +units=m +no_defs +lat_0=0 +lon_0=173 +k=0.9996"
  temp_nz
  
# data -------------------------------------------------------------------------
# poaceae
  dat_all <- readRDS("Data files/ALA/master data/master data.RDS") 

# function to compute richness for given coverage 

  covrich <- function(dat, min.rec, coverage, temp_rast) {  

  # assign each point in the dataframe to raster cell
    xy <- cbind(dat$longitude, dat$latitude)
    dat$cell <- terra::extract(temp_rast, xy, cell = T)[, 2]
  
  # number of records per cell
    nr <- dat %>%
          group_by(cell) %>%
          summarise(n.rec = n()) %>%
          filter(!is.na(cell))
          
    dat2 <- full_join(dat, nr, by = "cell")
  
  # filter by min.rec and extract number of records of each species in each cell
    cr <- dat2 %>%
          ungroup() %>%
          filter(n.rec >= min.rec) %>%
          mutate(species = factor(species)) %>%
          group_by(species, cell) %>%
          summarise(n = n()) 
          
  # do iNEXT cell by cell  
    cell.list <- as.numeric(as.character(levels(factor(cr$cell))))
    temp1 <- data.frame(q0 = 0,
                        n.rec = 0,
                        n.spp = 0, 
                        warn = 0,
                        cell_id = 0)
    
  # abundance of each spp for each cell
    for(i in 1:length(cell.list)) {
      td <- cr %>% filter(cell %in% cell.list[i]) %>%
        ungroup() %>% 
        rename(spp = n) %>% 
        drop_na() %>% 
        dplyr::select(spp)
    
  # coverage rarefaction using iNEXT function
  # check for warning
    out_warn <- 0
    x <- suppressWarnings(tryCatch(estimateD(td, datatype = "abundance", base = "coverage",
                                level = coverage, nboot = 0, q = 0), 
                  warning = function(w) {out_warn <<- 1}))
        
  # run coverage
    temp1[i, 1] <- suppressWarnings(estimateD(td, datatype = "abundance", base = "coverage",
                          level = coverage, nboot = 0, q = 0)$qD)
    temp1[i, 2] <- sum(td)
    temp1[i, 3] <- nrow(td)
    temp1[i, 4] <- out_warn[1]
    temp1[i, 5] <- cell.list[i]
    
  }
  return(temp1)
  
}

# ------------------------------------------------------------------------------
# NZ
nz_nat_c3 <- covrich(dat = filter(dat_all, 
                                  pp == "C3" & 
                                  status == "native"& 
                                  country == "NZ"), 
                     min.rec = 1, coverage = 0.8, temp_rast = temp_nz)

nz_nat_c4 <- covrich(dat = filter(dat_all, pp == "C4" & 
                                    status == "native" & 
                                    country == "NZ"), 
                     min.rec = 1, coverage = 0.8, temp_rast = temp_nz)

nz_nonnat_c3 <- covrich(dat = filter(dat_all, pp == "C3" & 
                                       status == "nonnative" & 
                                       country == "NZ"), 
                     min.rec = 1, coverage = 0.8, temp_rast = temp_nz)
nz_nonnat_c4 <- covrich(dat = filter(dat_all, pp == "C4" & 
                                       status == "nonnative" & 
                                       country == "NZ"), 
                        min.rec = 1, coverage = 0.8, temp_rast = temp_nz)

# Aus
aus_nat_c3 <- covrich(dat = filter(dat_all, pp == "C3" & 
                                     status == "native" & 
                                     country == "Aus"), 
                     min.rec = 1, coverage = 0.8, temp_rast = temp_aus)

aus_nat_c4 <- covrich(dat = filter(dat_all, pp == "C4" & 
                                     status == "native" & 
                                     country == "Aus"), 
                     min.rec = 1, coverage = 0.8, temp_rast = temp_aus)

aus_nonnat_c3 <- covrich(dat = filter(dat_all, pp == "C3" & 
                                        status == "nonnative" & 
                                        country == "Aus"), 
                        min.rec = 1, coverage = 0.8, temp_rast = temp_aus)
aus_nonnat_c4 <- covrich(dat = filter(dat_all, pp == "C4" & 
                                        status == "nonnative" & 
                                        country == "Aus"), 
                        min.rec = 1, coverage = 0.8, temp_rast = temp_aus)


# write to csv files  
write.csv(nz_nat_c3, "Results/csv/species richness/nz_nat_c3.csv", 
          row.names = F)
write.csv(nz_nat_c4, "Results/csv/species richness/nz_nat_c4.csv", 
          row.names = F)
write.csv(nz_nonnat_c3, "Results/csv/species richness/nz_nonnat_c3.csv", 
          row.names = F)
write.csv(nz_nonnat_c4, "Results/csv/species richness/nz_nonnat_c4.csv", 
          row.names = F)

write.csv(aus_nat_c3, "Results/csv/species richness/aus_nat_c3.csv", 
          row.names = F)
write.csv(aus_nat_c4, "Results/csv/species richness/aus_nat_c4.csv", 
          row.names = F)
write.csv(aus_nonnat_c3, "Results/csv/species richness/aus_nonnat_c3.csv", 
          row.names = F)
write.csv(aus_nonnat_c4, "Results/csv/species richness/aus_nonnat_c4.csv", 
          row.names = F)
