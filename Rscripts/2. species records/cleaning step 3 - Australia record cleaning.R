

# library --------------------------------------------------------
  library(raster)
  library(rgdal)
  library(tidyverse)
  library(data.table)
  
  rm(list = ls())
  
# data -------------------------------------------------------------------------------------  
# ala records
  dat <- fread("Data files/ALA/supplied data/data.csv")
  ala <- dat %>% dplyr::select("Year", 
                               "Latitude", 
                               "Longitude",
                               "Record ID", 
                               "Species", 
                               "Coordinate Uncertainty in Metres", 
                               "Taxon Rank", 
                               "Family",
                               "Genus") 
# ignore warning
  
  col_names <- c("year", 
                 "latitude", 
                 "longitude",
                 "id", 
                 "species", 
                 "coordinate_uncertainty", 
                 "taxon_rank", 
                 "family",
                 "genus")
  colnames(ala) <- col_names
  ala_names <- data.frame(species = ala$species, ala = 1) 
  
# templates
  aus <- raster("Data files/Australia/Australia 1141.grd")
  plot(aus)

# APC names
  apc <- read.csv("Data files/ALA/cleaning steps/step 2 - Australia APC names, status.csv", strip.white = T) %>%
    dplyr::select(name, status) 
  
# filter steps --------------------------------------------------------
# crop records --------------------------------------------------------
  xy <- cbind(ala$longitude, ala$latitude)
  cell <- raster::extract(aus, xy)
  table(cell, exclude = NULL)
  
  ala$country <- ifelse(!is.na(cell), "Aus", "ocean")
  table(ala$country, exclude = NULL)
  
  ala2 <- ala %>% 
    filter(country == "Aus") %>%
    droplevels()
  
# missing values
  ala3 <- ala2 %>% tidyr::drop_na("species", "genus", "latitude", "longitude", "year")
  
# missing (blank), genera, 'forms', 'variety' and subspecies
  table(ala3$taxon_rank, exclude = F)
  ala3a <- filter(ala3, grepl(" ", species))
  ala3b <- filter(ala3a, !grepl("form", taxon_rank))
  ala3c <- filter(ala3b, !grepl("variety", taxon_rank))
  ala3d <- filter(ala3c, !genus == "")
  table(ala3d$taxon_rank, exclude = F)
  ala3d$taxon_rank <- if_else(ala3d$taxon_rank == "subspecies", "species", ala3d$taxon_rank)
  table(ala3d$taxon_rank, exclude = F)
  
# incorrect year (where year = 0)
  table(ala3d$year, exclude = F)
  ala4 <- filter(ala3d, !year == "0")
  table(ala4$year, exclude = F)
  
# records with coordiantes that are incorrect, uncertain (NA) and with large uncertainties (above 10 km radius)
  ala5 <- filter(ala4, coordinate_uncertainty <= 10000 & !coordinate_uncertainty <= 0)
  table(ala5$coordinate_uncertainty, exclude = F)
  
# duplicates 
# round lat/longs to ~1-km (2dp)
  ala5$latitude <- round(ala5$latitude, digits = 2)
  ala5$longitude <- round(ala5$longitude, digits = 2)
# find unique (distinct) records 
  ala6 <- ala5 %>% distinct(species, year, latitude, longitude, .keep_all = TRUE) 
  
# list of species
  spp.list <- levels(as.factor(ala6$species))
  length(spp.list)
  spp.list.df <- data.frame(spp.list) # 1441
  
# assign native/nonnative status ---------------------------------------------------------
# ala species names
  ala6$species <- as.factor(ala6$species)
  ala.names <- data.frame(species = as.character(levels(ala6$species)), ala = 1) 
  
# removing Cynochloris genus because it is a hybrid: http://www.theplantlist.org/1.1/browse/A/Poaceae/Cynochloris/ & http://ausgrass2.myspecies.info/content/x-cynochloris-reynoldensis
  ala7 <- filter(ala6, genus != "Cynochloris")
  table(ala7$genus == "Cynochloris")

# remove duplicate APC names
  apc.dup <- distinct(apc, name, .keep_all = T)
  
# apc status (native/nonnative)
  apc$name <- as.factor(apc$name) 
  apc.names <- data.frame(species = as.character(levels(as.factor(apc.dup$name))), 
                          apc = 1, 
                          apc.status = table(apc.dup$name, apc.dup$status)[, 1])  # start here
  
# match ALA to APC names
  allnames <- merge(ala.names, apc.names, by = "species", all = T)
  allnames <- mutate(allnames, ala = ifelse(is.na(ala) == T, 0, 1),
                     apc = ifelse(is.na(apc) == T, 0, 1),
                     tot = ala + apc)
  matching.names <- filter(allnames, tot == 2)
  
# filter ALA records for appropriate names
  ala7$species <- as.character(ala7$species)
  apc.names$species <- as.character(apc.names$species)
  ala8 <- filter(ala7, ala7$species %in% matching.names$species)
  
# join APC and ALA names  
  ala9 <- left_join(ala8, apc.names, by = "species")
  
# checks
  table(ala9$apc.status, exclude = F)
  table(ala9$apc, exclude = F)
  
  ala10 <- ala9 %>% 
           mutate(status = if_else(apc.status == 1, "native", "nonnative")) %>%
           dplyr::select(-apc.status, -apc)

# save ---------------------------------------------------------------
  saveRDS(ala10, "Data files/ALA/cleaning steps/step 3 - Australia records.rds")
  
# -----------------------------------------------------------------------------