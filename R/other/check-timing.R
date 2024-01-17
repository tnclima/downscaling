# test speed


library(terra)
library(magrittr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(fs)
library(purrr)
# library(ggplot2)
# library(patchwork)
# library(mgcViz)
# library(forcats)
# library(scico)
library(MBC)

source("R/functions/create_empty_netcdf.R")
source("R/functions/get_rcm_values2.R")
source("R/functions/inv_sub.R")



# settings - variables ----------------------------------------------------

cell_match_type <-  "xy" # elev or xy
n_cells <- 1 # number of cells for elev, or width (odd) of square for xy
detrend <- F # detrend tas*  prior to ba? (and add trend back future)
mbcn_iter <- 10 # iterations of mbcn algorithm (default 30, 15 is faster)

l_file_obs <- list(
  tasmax = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MaxTemp.nc",
  tasmin = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MinTemp.nc",
  pr = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_Prec.nc"
)

l_file_obs_orog <- list(
  tasmax = "/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc",
  tasmin = "/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc",
  pr = "/home/climatedata/obs/orography/crespi_lonlat_1km_precipitation.nc"
)

# inventory ---------------------------------------------------------------

dat_inv <- inv_sub()
# dat_inv_loop <- dat_inv[experiment == "rcp85" & variable %in% c("tasmin", "tasmax", "pr")]
dat_inv_loop <- dat_inv[experiment == "rcp85" & 
                          variable %in% c("tasmin", "tasmax", "pr") & 
                          institute_rcm %in% c("SMHI-RCA4", "IPSL-WRF381P")]

dat_inv_loop_mod <- dat_inv_loop[, .(gcm, institute_rcm, experiment, 
                                     ensemble, downscale_realisation, centers)] %>% unique()



# read 1 rcm --------------------------------------------------------------

system.time({
  rs_rcm <- rast(dat_inv_loop[1, list_files[[1]]])
  mat_rcm <- values(rs_rcm, mat = T)
})
# ~ 30sec

format(object.size(mat_rcm), units = "auto")
# ~ 170 Mb

# read 1 crespi -----------------------------------------------------------


system.time({
  rs_crespi <- rast(l_file_obs$tasmax)
  mat_crespi <- values(rs_crespi, mat = T)
})
# ~ 20sec

format(object.size(mat_crespi), units = "auto")
# ~ 3.3 Gb



