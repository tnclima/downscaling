# create sub-ensemble for downscaling

library(eurocordexr)
library(data.table)
setDTthreads(4)
library(magrittr)

inv_sub <- function(
    path_ds ="/home/climatedata/downscaling/rcm_lonlat_tnaa/",
    file_kkz = "data/sub-ensemble-kkz-tn-01-selected-models.rds"
){
  
  dat_kkz <- readRDS(file_kkz)
  dat_inv <- get_inventory(path_ds)
  dat_inv[variable == "orog" & institute_rcm == "UHOH-WRF361H", 
          institute_rcm := "IPSL-WRF381P"] # since wrf381 has no fx info
  dat_inv_sub <- dat_inv[variable != "orog"] %>% 
    merge(dat_kkz,
          by = c("gcm", "institute_rcm", "experiment", "ensemble",
                 "downscale_realisation")) %>% 
    rbind(dat_inv[variable == "orog" & institute_rcm %in% dat_kkz$institute_rcm], fill = T)

  class(dat_inv_sub) <- c("eurocordexr_inv", class(dat_inv_sub))
  
  # dat_inv_sub[, .N, variable]
  # dat_inv_sub[, gcm:variable] %>% dcast(... ~ variable)
  dat_inv_sub
  
}