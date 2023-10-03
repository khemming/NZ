  
  
# library --------------------------------------------------------------------
  library(tidyverse)
  library(data.table)
  library(janitor)
  
  rm(list = ls())
  
  
# data ------------------------------------------------------------------------
# records
  dat <- readRDS("Data files/ALA/master data/master data.RDS")
  glimpse(dat)
  
# Australian species checklist  
  asc <- read.csv("Data files/ALA/cleaning steps/step 2 - Australia APC names, status.csv") %>%
    filter(familia == "Poaceae") %>%
    mutate(cnty = "AU",
           status = ifelse(status == 0, "native", "nonnative"),
           species = name) %>%
    select(genus, species, status, cnty) 
  head(asc)
  
# NZ species checklist
  nzsc <- read.csv("Data files/ALA/cleaning steps/step 4 - NZ flora names, status.csv") %>%
    filter(Family == "Gramineae") %>%
    rename(genus = Genus,
           species = CurrentNamePart,
           status = native_status) %>%
    mutate(cnty = "NZ") %>%
    select(genus, species, status, cnty)
  head(nzsc)
  
  dat <- bind_rows(asc, nzsc)
  head(dat)
  
# compare checklist species inventories ---------------------------------------------- 
# shared spp
  au_spp <- asc %>% 
              select(species, status) %>%
              mutate(au = 1) %>%
              rename(au_stat = status)
  nz_spp <- nzsc %>% 
             select(species, status) %>%
             mutate(nz = 1) %>%
             rename(nz_stat = status)
  
  spp_ls <- dat %>%
              distinct(species, genus) %>%
              left_join(au_spp, by = "species") %>%
              left_join(nz_spp, by = "species") %>%
              replace_na(list(nz = 0, au = 0)) %>%
              mutate(shared = au + nz)
  
  head(spp_ls)
  
  test <- spp_ls %>%
            group_by()
  

  
  
# totals  
  spp_tot <- dat %>% 
    group_by(cnty, status) %>%
    summarise(spp = n()) %>%
    pivot_wider(names_from = status, values_from = spp)
  
# total spp
  tot_spp <- dat %>% 
    dplyr::select(species) %>%
    distinct(species)
  
  
  
# -------------------------------------------------------------------  
  
  
# Aus  
  aus_spp <- dat %>% 
    filter(cnty == "AU") %>%
    dplyr::select(species, status) %>%
    mutate(aus = 1) %>%
    rename(aus_stat = status)
  head(aus_spp)
  
# NZ  
  nz_spp <- dat %>% 
    filter(cnty == "NZ") %>%
    dplyr::select(species, status) %>%
    mutate(nz = 1) %>%
    rename(nz_stat = status)
  head(nz_spp)

# merge total spp with countries of origin and measure shared spp
  tot_spp2 <- left_join(tot_spp, nz_spp, "species") %>%
    left_join(., aus_spp, by = "species") %>% 
    replace_na(list(nz = 0, aus = 0, 
                    nz_stat = "not_shared",
                    aus_stat = "not_shared")) %>%
    mutate(shared_spp = nz + aus)
  head(tot_spp2)
  
# add in pp and status
  spp_tab <- table(tot_spp2$nz_stat, tot_spp2$aus_stat, exclude = NULL)
  spp_tab
  
# by genus --------------------------------------------  
# total genera
  tot_gen <- dat %>% 
    dplyr::select(genus) %>%
    distinct(genus)
  
# Aus  
  aus_gen <- dat %>% 
    filter(cnty == "AU") %>%
    dplyr::select(genus, status) %>%
    distinct() %>%
    mutate(aus = 1) %>%
    rename(aus_stat = status)
  head(aus_gen)
  
# NZ  
  nz_gen <- dat %>% 
    filter(cnty == "NZ") %>%
    dplyr::select(genus, status) %>%
    distinct() %>%
    mutate(nz = 1) %>%
    rename(nz_stat = status)
  head(nz_gen)
  
# merge total gen with countries of origin and measure shared gen
  tot_gen2 <- left_join(tot_gen, nz_gen, "genus") %>%
    left_join(., aus_gen, by = "genus") %>% 
    replace_na(list(nz = 0, aus = 0, 
                    nz_stat = "not_shared",
                    aus_stat = "not_shared")) %>%
    mutate(shared_gen = nz + aus)
  head(tot_gen2)
  
# add in pp and status
  gen_tab <- table(tot_gen2$nz_stat, tot_gen2$aus_stat, exclude = NULL)
  gen_tab  
  
# save
  write.csv(spp_tab, "Results/csv/species richness/flora checklist shared spp.csv", row.names = T)
  write.csv(gen_tab, "Results/csv/species richness/flora checklist shared genera.csv", row.names = T)
  
# --------------------------------------------------------  