

# library ----------------------------------------------------------------------
  library(terra)
  library(iNEXT)
  library(tidyverse)
  
# read in environment raster
  env_rast <- rast("Data files/predictor variables/Environment/Environment raster Aus.tif")
  env_rast
  
# template raster based on env raster
  temp_rast <- rast(extent = ext(env_rast), resolution = res(env_rast))
  crs(temp_rast) <- "epsg:4326"
  temp_rast
  
# data -------------------------------------------------------------------------
# poaceae
  dat_all <- readRDS("Data files/ALA/master data/master grass records.rds") 

# function to compute richness for given coverage 

covrich <- function(dat, min.rec, coverage) {  

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
    x <- tryCatch(estimateD(td, datatype = "abundance", base = "coverage",
                                level = coverage, nboot = 0, q = 0), 
                      warning = function(w) {out_warn <<- 1})
        
  # run coverage
    temp1[i, 1] <- estimateD(td, datatype = "abundance", base = "coverage",
                          level = coverage, nboot = 0, q = 0)$qD
    temp1[i, 2] <- sum(td)
    temp1[i, 3] <- length(td)
    temp1[i, 4] <- out_warn
    temp1[i, 5] <- cell.list[i]
    
  }
  
  return(temp1)
}

rich_natC3 <- covrich(dat = filter(dat_all, pp == "C3" & status == "native"), 
                      min.rec = 1, coverage = 0.8)
rich_natC4 <- covrich(dat = filter(dat_all, pp == "C4" & status == "native"), 
                      min.rec = 1, coverage = 0.8)
rich_exoC3 <- covrich(dat = filter(dat_all, pp == "C3" & status == "nonnative"), 
                      min.rec = 1, coverage = 0.8)
rich_exoC4 <- covrich(dat = filter(dat_all, pp == "C4" & status == "nonnative"), 
                      min.rec = 1, coverage = 0.8)

# write to csv files  
write.csv(rich_natC3, "Results/csv/species richness/rich_natC3.csv", 
          row.names = F)
write.csv(rich_natC4, "Results/csv/species richness/rich_natC4.csv", 
          row.names = F)
write.csv(rich_exoC3, "Results/csv/species richness/rich_exoC3.csv", 
          row.names = F)
write.csv(rich_exoC4, "Results/csv/species richness/rich_exoC4.csv", 
          row.names = F)


