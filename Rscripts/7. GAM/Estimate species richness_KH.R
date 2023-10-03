

# library -------------------------------------------------------------------------
  library(terra)
  library(iNEXT)
  library(tidyverse)

  rm(list = ls())
  
# read in environment raster
  env_rast <- rast("Data files/predictor variables/Environment/Environment raster Aus.tif")
  env_rast
  
# template raster based on env raster
  temp_rast <- rast(extent = ext(env_rast), resolution = res(env_rast))
  crs(temp_rast) <- "epsg:4326"
  temp_rast
  
# data ----------------------------------------------------------------------------
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
          
  # get a list of the occupied cell numbers
    cell.list <- as.numeric(as.character(levels(factor(cr$cell))))
    
    td <- data.frame(spp = cr$n[cr$cell == cell.list[1]])
    td <- td[!is.na(td$spp), ]
    # check for warning
    out_warn <- 0
    temp1 <- tryCatch(estimateD(td, datatype = "abundance", base = "coverage", level = coverage, conf = NULL), warning = function(w) {out_warn <<- 1})
    # does not work: Error in data.frame(..., check.names = FALSE) : 
    # arguments imply differing number of rows: 3, 0
    
    # run coverage
    temp1 <- estimateD(td, datatype = "abundance", base = "coverage", level = coverage, conf = NULL)
    temp1$n.rec <- sum(td)
    temp1$n.spp <- length(td)
    temp1$warn <- out_warn
    temp1$cell_id <- cell.list[1]
    names(temp1)[4:6] <- c("q0", "q1", "q2")
    cov <- temp1
    
    # do the rarefaction cell by cell  
      for(i in 2:length(cell.list)) {
        td <- data.frame(spp = cr$n[cr$cell == cell.list[i]])
        td <- td[!is.na(td$spp), ]
    
    # coverage rarefaction using iNEXT function
    # check for warning
        out_warn <- 0
        temp1 <- tryCatch(estimateD(td, datatype = "abundance", base = "coverage", level = coverage, conf = NULL), warning = function(w) {out_warn <<- 1})
        
    # run coverage
        temp1 <- estimateD(td, datatype = "abundance", base = "coverage", level = coverage, conf = NULL)
        temp1$n.rec <- sum(td)
        temp1$n.spp <- length(td)
        temp1$warn <- out_warn
        temp1$cell_id <- cell.list[i]
        names(temp1)[4:6] <- c("q0", "q1", "q2")
        
        cov <- bind_rows(cov, temp1)
      }
    return(cov)
}

rich_natC3 <- covrich(dat = filter(dat_all, pp == "C3" & status == "native"), min.rec = 1, coverage = 0.8)
rich_natC4 <- covrich(dat = filter(dat_all, pp == "C4" & status == "native"), min.rec = 1, coverage = 0.8)
rich_exoC3 <- covrich(dat = filter(dat_all, pp == "C3" & status == "nonnative"), min.rec = 1, coverage = 0.8)
rich_exoC4 <- covrich(dat = filter(dat_all, pp == "C4" & status == "nonnative"), min.rec = 1, coverage = 0.8)

# write to csv files  

write.csv(rich_natC3, "Results/csv/rich_natC3.csv", row.names = F)
write.csv(rich_natC4, "Results/csv/rich_natC4.csv", row.names = F)
write.csv(rich_exoC3, "Results/csv/rich_exoC3.csv", row.names = F)
write.csv(rich_exoC4, "Results/csv/rich_exoC4.csv", row.names = F)


