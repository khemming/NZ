
# library --------------------------------------------------------
  library(tidyverse)
  library(data.table)
  
  rm(list = ls())
  
# data -----------------------------------------------------------
# Checklist of NZ flora names
  nzflora <- read.csv("Data files/ALA/Supplied data/2018 plant name checklist.csv", 
                      fileEncoding = "UTF-8-BOM", header = T) 
  table(nzflora$Occurrence, exclude = F)
  table(nzflora$Origin, exclude = F)
  
# cleaning steps: 
# 1) accepted spp. level taxa and current names only
  nzf.a <- filter(nzflora, IsCurrent == "TRUE" & TaxonRank == "species")
# 2) no extinct spp.
  nzf.b <- filter(nzf.a, Occurrence != "Extinct")
# 3) what are those 2 uncertain spp.?
  nzf.uncer <- filter(nzf.b, Origin == "Uncertain") 
# they're really old records, so I will exclude them
  nzf.c <- filter(nzf.b, !Origin == "Uncertain")  
  
# final checks
  table(nzf.c$Occurrence, exclude = F)
  table(nzf.c$Origin, exclude = F)
  nzf.d <- droplevels(nzf.c)
   
# notes---------------------------------------------------
# there still this is 'sometimes present' tag, but I will retain these records
# you'll see in the data frame some weird species names -- that's actually where an 'x' is in the original .csv file, and because x denotes a hybrid, I will exclude them in the following cleaning script 
# ---------------------------------------------------------
  
# assign native/nonnative status
  nzf.e <- mutate(nzf.d, native_status = ifelse(Origin == "Exotic", "nonnative", "native")) %>%
    droplevels()
  
  table(nzf.e$native_status, exclude = NULL)
  
  nzf.f <- nzf.e %>% 
           dplyr::select(CurrentNamePart, Origin, Family) %>%
           filter(Family == "Gramineae")
  table(nzf.f$Origin)

# save
  write.csv(nzf.e, "Data files/ALA/cleaning steps/step 4 - NZ flora names, status.csv")
  
# ---------------------------------------------------------  
  
  