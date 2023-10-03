
# library --------------------------------------------------------
  library(ALA4R)
  library(data.table)
  
  rm(list = ls())
  
# all Plantae data -----------------------------------------------------------
  # required fields from download
  dl_fields <- c("year",
                 "latitude",
                 "longitude",
                 "coordinate_uncertainty",
                 "rank",
                 "family",
                 "genus",
                 "species",
                 "id",
                 "collector")
  
  occurrences(taxon = "family:Poaceae",
              fields= dl_fields,
              download_reason_id = 11,
              method = "offline",
              email = "kyle.hemming@canberra.edu.au")
  
# ---------------------------------------------------------------------------

             
  