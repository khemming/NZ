
#############################################################
# number of APC recognised native grass species
#############################################################

# library ---------------------------------------------------
  library(tidyverse)

# data ------------------------------------------------------
# APC
  apc <- read.csv("Data files/ALA/cleaning steps/step 1 ALA names & status.csv", strip.white = T) %>%
          filter(familia == "Poaceae",
                 naturalised == 0)
  
  spp <- apc %>% distinct(name, .keep_all = TRUE)
  
# -----------------------------------------------------------  