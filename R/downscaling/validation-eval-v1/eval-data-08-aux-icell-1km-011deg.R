# 


library(terra)
library(magrittr)
library(data.table)
setDTthreads(4)

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

saveRDS(dat_aux, "/home/climatedata/downscaling/validation-cv/rdata-summary/eval-08-aux-icell-1km-011deg.rds")

