# 


library(terra)
library(magrittr)
library(data.table)
setDTthreads(4)
library(eurocordexr)

# create xy lookup rasters crespi-rcm ---------------------------------------------------

file_rcm_orog <- "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eobs.nc"
file_obs_orog <- "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_crespi_lonlat_1km_temperature.nc"

rs_rcm_orog <- rast(file_rcm_orog)
rs_obs_orog <- rast(file_obs_orog)

rs_rcm_cells <- rs_rcm_orog
rs_rcm_cells[] <- 1:ncell(rs_rcm_cells)

rs_cells_rcm_obs <- project(rs_rcm_cells, rs_obs_orog, method = "near")
# rs_cells_rcm_obs[is.na(rs_obs_orog)] <- NA # mask outside TNAA?
names(rs_cells_rcm_obs) <- "rcm_cell"

dat_aux <- rs_cells_rcm_obs %>% 
  as.data.table(xy = T, cells = T, na.rm = F)

setnames(dat_aux, c("cell", "rcm_cell"), c("icell_1km", "icell_011deg"))

dat_aux[, orog := rs_obs_orog[]]

saveRDS(dat_aux, "/home/climatedata/downscaling/validation-cv/rdata-summary-v1/eval-08-aux-icell-1km-011deg.rds")



# orog rcm rcp85 only ----------------------------------------------------------------

dat_selected_models <- readRDS("data/sub-ensemble-kkz-01-selected-models.rds")

dat_inv_orog <- get_inventory("/home/climatedata/downscaling/rcm_lonlat_tnaa/orog/")
dat_inv_orog[institute_rcm == "UHOH-WRF361H", institute_rcm := "IPSL-WRF381P"]

dat_inv_orog_sub <- dat_inv_orog[institute_rcm %in% dat_selected_models[experiment == "rcp85", institute_rcm]]

l_orog <- lapply(dat_inv_orog_sub$list_files, \(x) nc_grid_to_dt(x, add_xy = T))
names(l_orog) <- dat_inv_orog_sub$institute_rcm

dat_rcm_orog <- rbindlist(l_orog, idcol = "institute_rcm")
dat_rcm_orog[, date := NULL]

dat_out <- dat_selected_models[experiment == "rcp85", .(institute_rcm, centers)] %>% 
  merge(dat_rcm_orog, allow.cartesian = T)

saveRDS(dat_out, "/home/climatedata/downscaling/validation-cv/rdata-summary-v1/eval-09-aux-rcm.rds")
