

# library --------------------------------------------------------
  library(tidyverse)
  
  rm(list = ls())

# notes on assigning pathways ------------------------------------
# Photosynthetic pathways were assigned first using Osborne et al. (2014) and then
# Watson et al. (1992) via their website found here: https://www.delta-intkey.com/grass/index.htm
# for Watson, I went through and added them individually in the cases where Osborne lacked genera information
# ----------------------------------------------------------------

# data -----------------------------------------------------------
# Aus data
  dat <- readRDS("Data files/ALA/cleaning steps/ALA records filtered by accuracy.RDS")
  
  ala <- dat %>% filter(genus != "Steinchisma",       # intermediate C3/C4
                        genus != "Alloteropsis",      # intermediate C3/C4
                        genus != "Neurachne",         # intermediate C3/C4
                        genus != "Connorochloa") %>%  # unknown at this time
                        droplevels()
  
# Osborne's pp data   
  os <- read.csv("Data files/C3-C4 pathways/c3-c4.csv", 
                 fileEncoding = "UTF-8-BOM", header = T)
  colnames(os) <- c("pp", "genus")
  os$genus <- as.character(os$genus)
  table(os$pp) # note there are 'C3 and C4's and 'unknown' categories
  
# name change: Megathyrsus maxmimus is a synonym for Panicum maximum
  ala[ala$genus == "Megathyrsus", "genus"] <- "Panicum"
 
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
  c3 <- c("Anemanthele",
          "Achnatherum",
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
  
# merge genera and pathways -----------------------------------------------
  table(gen.a$pp)
  
  ala_pp <- left_join(ala, gen.a, by = "genus") %>%
            droplevels()
  head(ala_pp)
  table(ala_pp$pp, exclude = F)
  
 # save --------------------------------------------------------------------
  saveRDS(ala_pp, "Data files/ALA/master data/master Aus data.RDS")
  
# -------------------------------------------------------------------------    