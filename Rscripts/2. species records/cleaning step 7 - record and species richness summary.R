

# library --------------------------------------------------------------------
  library(tidyverse)
  library(data.table)
  library(janitor)
  library(raster)

  rm(list = ls())


# data ------------------------------------------------------------------------
# records
  dat <- readRDS("Data files/ALA/master data/master data.RDS")
  glimpse(dat)
  
  
  
# records, raw species richness --------------------------------------------
  spp_no <- dat %>% 
            group_by(country, pp, status) %>%
            distinct(species) %>%
            summarise(sr = n()) %>%
            pivot_wider(names_from = status, values_from = sr) %>%
            mutate(spp_tot = native + nonnative) %>%
            rename(nat_spp = native,
                   nnat_spp = nonnative)
  spp_no
  
  rec_no <- dat %>% 
            group_by(country, status, pp) %>%
            summarise(recs = n()) %>%
            pivot_wider(names_from = status, values_from = recs)  %>%
            mutate(rec_tot = native + nonnative) %>%
            rename(nat_rec = native,
                   nnat_rec = nonnative)
  rec_no

# merge
  table <- left_join(spp_no, rec_no, by = c("country", "pp")) %>%
           adorn_totals() 
  table

  write.csv(table, "Results/csv/species richness/record and richness summary.csv", row.names = F)
  
  
# shared species --------------------------------------------------  
# complete lists of species
  tot_spp <- dat %>% 
             dplyr::select(species, pp) %>%
             distinct(species, .keep_all = T)
  
  nz_spp <- dat %>% 
            filter(country == "NZ") %>%
            dplyr::select(species, status, pp) %>%
            distinct(species, .keep_all = T) %>%
            mutate(nz = 1) %>%
            rename(nz_stat = status)
  
  aus_spp <- dat %>% 
             filter(country == "Aus") %>%
             dplyr::select(species, status, pp) %>%
             distinct(species, .keep_all = T) %>%
             mutate(aus = 1) %>%
             rename(aus_stat = status)
    
# merge total spp with countries of origin and measure shared spp
  tot_spp2 <- left_join(tot_spp, nz_spp, by = c("species", "pp")) %>%
              left_join(., aus_spp, by = c("species", "pp")) %>% 
              replace_na(list(nz = 0, aus = 0, 
                              nz_stat = "not_shared",
                              aus_stat = "not_shared")) %>%
              mutate(shared_spp = nz + aus)
  head(tot_spp2)
  
# add in pp and status
  table(tot_spp2$nz_stat, tot_spp2$aus_stat, exclude = NULL)
  
# by pathway
  tot_c3 <- tot_spp2 %>% filter(pp == "C3")
  table(tot_c3$nz_stat, tot_c3$aus_stat, exclude = NULL)
  
  tot_c4 <- tot_spp2 %>% filter(pp == "C4")
  table(tot_c4$nz_stat, tot_c4$aus_stat, exclude = NULL)
  
# ----------------------------------------------------------------------------