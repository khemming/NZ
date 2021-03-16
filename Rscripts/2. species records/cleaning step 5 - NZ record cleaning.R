

# library ----------------------------------------------------------------
  library(raster)
  library(rgdal)
  library(tidyverse)
  library(data.table)
  rm(list = ls())

# data ---------------------------------------------------------------------
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
  
# useable names
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

# NZ flora names
  nzflora <- read.csv("Data files/ALA/cleaning steps/step 4 - NZ flora names, status.csv", encoding="UTF-8-BOM") %>%
    dplyr::select(CurrentNamePart, native_status)
  table(nzflora$native_status, exclude = NULL)
  
# template
  nz <- raster("Data files/NZ/NZ 83.grd")
 
# filter steps --------------------------------------------------------
# crop records --------------------------------------------------------
  xy <- cbind(ala$longitude, ala$latitude)
  cell <- raster::extract(nz, xy)
  
  table(cell, exclude = NULL)
  
  ala$country <- ifelse(!is.na(cell), "NZ", "ocean")
  table(ala$country, exclude = NULL)
  
  ala2 <- ala %>% 
    filter(country == "NZ") %>%
    droplevels()
  
# missing values
  ala3a <- ala2 %>% tidyr::drop_na("species", "genus", "latitude", "longitude", "year") %>%
           filter(species != "")
# remove taxa identifed as 'form' and 'variety', and rerank subspecies as species
  table(ala3a$taxon_rank, exclude = F)
  ala3b <- filter(ala3a, taxon_rank != "form", 
                         taxon_rank != "variety")
  ala3b$taxon_rank <- if_else(ala3b$taxon_rank == "subspecies", "species", ala3b$taxon_rank)
  table(ala3b$taxon_rank, exclude = F)
  
# year 
  table(ala3b$year, exclude = F)
  
# removing records with uncertainty of > 10,000 m
# note: keeping records missing uncertainty because this group constitutes the majority of the records
  ala4 <- ala3b
  table(ala4$coordinate_uncertainty, exclude = NULL)
  ala5a <- ala4 %>% filter(is.na(coordinate_uncertainty))
  ala5b <- ala4 %>% filter(!coordinate_uncertainty >10000)
  ala6 <- bind_rows(ala5a, ala5b)
  table(ala6$coordinate_uncertainty, exclude = F)
  
# duplicates 
# round lat/longs to ~1-km (2dp)
  ala6$latitude <- round(ala6$latitude, digits = 2)
  ala6$longitude <- round(ala6$longitude, digits = 2)
# ratain unique (distinct) records 
  ala7 <- ala6 %>% distinct(species, year, latitude, longitude, .keep_all = TRUE) 
  
# list of species
  spp.list <- levels(as.factor(ala7$species))
  length(spp.list)
  spp.list.df <- data.frame(spp.list) # 447
  
# assign native/nonnative status -----------------------------------------
# ala species names
  dat <- ala7
  dat$species <- as.factor(dat$species)
  ala.names <- data.frame(species = as.character(levels(dat$species)), ala = 1) 
  
  nzflora2 <- data.frame(species = as.character(levels(as.factor(nzflora$CurrentNamePart))), 
                         nz_flora = 1, 
                         native_status = table(nzflora$CurrentNamePart, nzflora$native_status)[, 1]) 
  head(nzflora2)
  
# match nz flora names and status with ALA species
  allnames <- merge(ala_names, nzflora2, by = "species", all = T) %>%
              mutate(ala = ifelse(is.na(ala) == T, 0, 1),
                     nz_flora = ifelse(is.na(nz_flora) == T, 0, 1),
                     tot = ala + nz_flora) # where they agree
  tail(allnames)
  
# take only names which have both ALA and APC (i.e. tot = 2)  
  matching_names <- filter(allnames, tot == 2)
  tail(matching_names)
  
# subset ala based on these names
  nzflora2$species <- as.character(nzflora2$species)
  ala_status_names <- filter(dat, dat$species %in% matching_names$species) %>%
                      mutate(species = as.character(species))
  
# add native status
  ala_status <- left_join(ala_status_names, nzflora2, by = "species") %>%
    dplyr::select(-nz_flora)
  head(ala_status)
  
# update status
  ala_nzflora2 <- ala_status %>% 
                  mutate(status = ifelse(native_status == "1", "native", "nonnative")) %>%
                  dplyr:: select(-native_status)
  ala_nzflora2$species <- as.factor(ala_nzflora2$species)
  length(levels(ala_nzflora2$species))
  table(ala_nzflora2$status, exclude = F)
  
  glimpse(ala_nzflora2)
  
  ala_nzflora3 <- ala_nzflora2 %>% 
                  dplyr::select(species, status)%>%
                  distinct(species, .keep_all = T)

  table(nzf.f$Origin)
  
# save
  saveRDS(ala_nzflora2, "Data files/ALA/cleaning steps/step 5 - NZ records.RDS")
  
# ----------------------------------------------------------------------------------------