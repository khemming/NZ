
# library -------------------------------------------------------------
  library(raster)


# data ----------------------------------------------------------------
# observed richness
setwd("C:/Users/s436862/Dropbox/NZ/Results/rasters/log")
files_ls <- list.files(pattern = ".grd")

nz_ls <- Filter(function(x) grepl("NZ_", x), files_ls)
nz_names <- gsub(pattern = "\\.grd$", "", nz_ls)
nz_stack <- stack(nz_ls)
names(nz_stack) <- nz_names
list2env(setNames(unstack(nz_stack), names(nz_stack)), .GlobalEnv)

aus_ls <- Filter(function(x) grepl("Aus_", x), files_ls)
aus_names <- gsub(pattern = "\\.grd$", "", aus_ls)
aus_stack <- stack(aus_ls)
names(aus_stack) <- aus_names
list2env(setNames(unstack(aus_stack), names(aus_stack)), .GlobalEnv)

# predicted richness to NZ
setwd("C:/Users/s436862/Dropbox/NZ/Results/rasters/log predicted")
files_ls <- list.files(pattern = "predicted.grd")

nz_ls <- Filter(function(x) grepl("NZ_", x), files_ls)
nz_names <- gsub(pattern = "\\.grd$", "", nz_ls)
nz_stack <- stack(nz_ls)
names(nz_stack) <- nz_names
list2env(setNames(unstack(nz_stack), names(nz_stack)), .GlobalEnv)

aus_ls <- Filter(function(x) grepl("Aus_", x), files_ls)
aus_names <- gsub(pattern = "\\.grd$", "", aus_ls)
aus_stack <- stack(aus_ls)
names(aus_stack) <- aus_names
list2env(setNames(unstack(aus_stack), names(aus_stack)), .GlobalEnv)

# predicted richness to Aus
setwd("C:/Users/s436862/Dropbox/NZ/Results/rasters/log predicted")
files_ls <- list.files(pattern = "to_Aus.grd")
aus_names2 <- gsub(pattern = "\\.grd$", "", files_ls)
pr2aus_stack <- stack(files_ls)
names(pr2aus_stack) <- aus_names2
list2env(setNames(unstack(pr2aus_stack), names(pr2aus_stack)), .GlobalEnv)

setwd("C:/Users/s436862/Dropbox/NZ")

# median richness -----------------------------------------------------
# convert from log to raw species richness
# NZ distribution data frame
  msr_nz_stk <- exp(stack(NZ_native_C3,  NZ_native_C3_predicted,
                      Aus_native_C3_predicted,
                      NZ_nonnative_C3,  NZ_nonnative_C3_predicted,
                      
                      NZ_native_C4,  
                      Aus_native_C4_predicted,
                      NZ_nonnative_C4,  NZ_nonnative_C4_predicted))

  msr_nz <- round(cellStats(msr_nz_stk, stat = "median", na.rm = T), 3)
  msr_nz <- data.frame(msr_nz)
  msr_nz <- data.frame(spp = row.names(msr_nz),
                    msr = msr_nz[,1])
  msr_nz
  
# Aus data frame
  msr_aus_stk <- exp(stack(Aus_native_C3, Aus_native_C4))
  msr_aus <- round(cellStats(msr_aus_stk, stat = "median", na.rm = T), 3)
  msr_aus <- data.frame(msr_aus)
  msr_aus <- data.frame(spp = row.names(msr_aus),
                        msr = msr_aus[,1])
  msr_aus

# bind and save
  msr <- rbind(msr_nz, msr_aus)
  
  write.csv(msr, "Results/csv/species richness/median species richness.csv", row.names = F)

# ----------------------------------------------------------------    

  
  
  
    