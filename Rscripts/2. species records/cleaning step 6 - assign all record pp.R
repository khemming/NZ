
# library --------------------------------------------------------
  library(tidyverse)
  
  rm(list = ls())

# notes on assigning pathways ------------------------------------
# Photosynthetic pathways were assigned first using Osborne et al. (2014) and then
# Watson et al. (1992) via their website found here: https://www.delta-intkey.com/grass/index.htm
# for Watson, I went through and added them individually in the cases where Osborne lacked genera information
# Genus Panicum was done individually via Osborne
# ----------------------------------------------------------------

# data -----------------------------------------------------------
# records
  nz_rec <- readRDS("Data files/ALA/cleaning steps/step 5 - NZ records.RDS")
  aus_rec <- readRDS("Data files/ALA/cleaning steps/step 3 - Australia records.RDS")
  ala <- rbind(nz_rec, aus_rec) %>%
         mutate(species = as.character(species))
  glimpse(ala)

# Osborne's pp data   
  os <- read.csv("Data files/C3-C4 pathways/c3-c4.csv", 
                 fileEncoding = "UTF-8-BOM", header = T)
  colnames(os) <- c("pp", "genus")
  os$genus <- as.character(os$genus)
  table(os$pp) # note there are 'C3 and C4's and 'unknown' categories

# Panicum pathway bvy genus  
  os_panicum <- read.csv("Data files/C3-C4 pathways/Panicum.csv", fileEncoding = "UTF-8-BOM", header = T) %>%
    dplyr::select(Species, Pathway)
  colnames(os_panicum) <- c("species", "pani_pp")
  head(os_panicum) 
  
# name change: Megathyrsus maxmimus is a synonym for Panicum maximum
  ala[ala$genus == "Megathyrsus", "genus"] <- "Panicum"
  ala[ala$species == "Megathyrsus maximus", "species"] <- "Panicum maximum"   
  
# assign pathways by genus using Osborne ----------------------------------
  gen <- distinct(ala, genus) %>%
    arrange(., genus)
  
  gen.a <- left_join(gen, os, by = "genus")
 
  table(gen.a$pp, exclude = NULL) # tidy up these
  
# assign remaining genera -------------------------------------------------
# there are 24 NAs and a few which are both C3 and C4
# if Watson can't resolve these, we will exclude from study
  gen.is.na <- filter(gen.a, is.na(gen.a$pp) == T)
  gen.is.na
  gen.c3.c4 <- filter(gen.a, pp == "C3 & C4")
  gen.c3.c4
  
# C3  
  c3 <- c("Achnatherum",
          "Amelichloa",   # reference: https://www.delta-intkey.com/grass/www/nassella.htm
          "Amphibromus",
          "Anthosachne", # reference: https://www.delta-intkey.com/grass/www/elymus.htm
          "Austrostipa",
          "Australopyrum",
          "Avellinia",
          "Chascolytrum",
          "Deyeuxia",
          "Dichanthelium",
          "Elytrigia",
          "Hookerochloa",
          "Jarava",
          "Lachnagrostis",
          "Lophopyrum",
          "Periballia",
          "Microlaena",
          "Molineriella",
          "Saxipoa", # no data on PP; genus recently moved from Poa, which is C3, so assuming that's conserved
          "Sylvipoa", # no data on PP; genus recently moved from Poa, which is C3, so assuming that's conserved
          "Sasaella",
          "Tetrarrhena",
          "Thinopyrum",
          "Walwhalleya",
          "Zotovia")
          
  c3.index <- (gen.a$genus %in% c3)
  gen.a$pp[c3.index]<- "C3"

# C4
  c4 <- c("Aristida",
          "Diplachne",
          "Eragrostis",
          "Moorochloa",
          "Panicum",
          "Pseudopogonatherum",
          "Thellungia",
          "Zuloagaea")
  
  c4.index <- (gen.a$genus %in% c4)
  gen.a$pp[c4.index] <- "C4"
  
  gen.a$pp[is.na(gen.a$pp)] <- "remove"
  
# merge genera and pathways -----------------------------------------------
  table(gen.a$pp)
  
  ala_pp <- left_join(ala, gen.a, by = "genus")
  
  head(ala_pp)
  table(ala_pp$pp, exclude = F)
  
# assign Panicum by species --------------------------------------------
# isolate panicum
  ala_pan <- ala_pp %>% 
    filter(genus == "Panicum") %>%
    dplyr::select(-pp)
  head(ala_pan)
  
# join ala data with Osborne data  
  pan_pp <- left_join(ala_pan, os_panicum, by = "species")
  head(pan_pp)
  
# label species with no pathway   
  pan_pp$pani_pp[is.na(pan_pp$pani_pp)] <- "remove"
  pan_pp2 <- pan_pp %>% rename(pp = pani_pp)
  head(pan_pp2)
  
# save excluded Panicum species  
  excluded_panicum <- pan_pp2 %>% filter(pp == "remove") %>%
    distinct(species)
  
  write.csv(excluded_panicum, "Results/csv/species richness/Panicum species excluded.csv", row.names = F)
  
# inset Panicum records into Poaceae records 
  ala2 <- ala_pp %>% 
          filter(genus != "Panicum")
  
  ala3 <- rbind(pan_pp2, ala2) %>%
    group_by(species) %>%
    arrange(species) 
  table(ala3$pp)
  
# make note of and remove taxa by species
  excl_taxa <- ala_pp %>% 
               filter(pp == "C3 & C4" |
                      pp == "remove") %>%
               distinct(species, .keep_all = T) %>%
               dplyr::select(species, genus)
  excl_taxa
  write.csv(excl_taxa, "Results/csv/species richness/excluded taxa.csv", row.names = F)
  
  incl_taxa <- ala_pp %>% 
               filter(pp != "C3 & C4" &
                      pp != "remove")
  
# save --------------------------------------------------------------------
  saveRDS(incl_taxa, "Data files/ALA/master data/master data.RDS")
  
# -------------------------------------------------------------------------    