# create sub-ensemble for downscaling

library(eurocordexr)
library(data.table)
setDTthreads(4)
library(magrittr)

inv_sub_reanalysis <- function(
    path_ds = "/home/climatedata/downscaling/rcm_lonlat_tnaa_eraint/"
){
  
  dat_inv <- get_inventory(path_ds)
  dat_inv[variable == "orog" & institute_rcm == "UHOH-WRF361H", 
          institute_rcm := "IPSL-WRF381P"] # since wrf381 has no fx info
  
  dat_inv_sub <- dat_inv[! institute_rcm %in% c("CNRM-ALADIN53", "DHMZ-RegCM4-2", "RMIB-UGent-ALARO-0")]
  # dat_inv_sub[, .N, variable]
  # dat_inv_sub[, gcm:variable] %>% dcast(... ~ variable)
  
  dat_inv_sub
}