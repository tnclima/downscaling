# 

library(terra)
library(magrittr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(fs)
library(foreach)
library(stringr)
# library(mgcv)

source("R/functions/create_empty_netcdf.R")
# source("R/functions/get_rcm_values2.R")
source("R/functions/inv_sub.R")

# settings - variables ----------------------------------------------------

path_out <- "/home/climatedata/downscaling/validation-cv/data-daily/ds-lrfix/"

date_rcm_sub <- as.Date(c("1981-01-01", "2020-12-31"))
dates_loop <- seq(date_rcm_sub[1], date_rcm_sub[2], by = "day")
n_nc_sync <- 200 # intermediate save to nc_out file every n dates

file_obs_orog <- "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc"
rs_template_tnaa <- rast("/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc")

# inventory ---------------------------------------------------------------

dat_inv <- inv_sub()
dat_inv_loop <- dat_inv[experiment == "rcp85" & 
                          variable %in% c("tasmin", "tasmax", "pr")]


# lapse rate data ---------------------------------------------------------

dat_lr_fix <- readRDS("data/lapse-rates-02-clim-crespi-station.rds")
dat_lr_fix <- dat_lr_fix[vv != "prec" & ds == "crespi_clim", .(month, lr, vv)]
dat_lr_fix[vv == "tmin", vv := "tasmin"]
dat_lr_fix[vv == "tmax", vv := "tasmax"]


# precip disagg -----------------------------------------------------------

rs_pr_fact <- rast("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/apgd_precip_disagg_factors.nc")

# main loop ---------------------------------------------------------------

mitmatmisc::init_parallel_ubuntu(6)

zz <- foreach(
  i_inv = 1:nrow(dat_inv_loop),
  .inorder = F
) %dopar% {
  
  i_rcm_name <- dat_inv_loop[i_inv, institute_rcm]
  i_var <- dat_inv_loop[i_inv, variable]
  
  file_rcm_orog <- dat_inv[variable == "orog" & institute_rcm == i_rcm_name, list_files[[1]]]
  file_rcm <- dat_inv_loop[i_inv, list_files[[1]]]
  
  file_out <- path(path_out,
                   dat_inv_loop[i_inv, 
                                paste(variable, centers, institute_rcm, 
                                      gcm, experiment, sep = "_")],
                   ext = "nc")
  
  if(file_exists(file_out)) return(NULL)
  
  # data --------------------------------------------------------------------
  
  rs_rcm_orog <- rast(file_rcm_orog)
  rs_obs_orog <- rast(file_obs_orog)
  
  rs_orog_rcm_obs <- project(rs_rcm_orog, rs_obs_orog, method = "near")
  rs_orog_diff_rcm_obs <- rs_orog_rcm_obs - rs_obs_orog
  
  cells_obs <- which(!is.na(rs_template_tnaa[]))
  cells_obs_na <- which(is.na(rs_template_tnaa[]))
  
  rs_rcm <- rast(file_rcm)
  
  # time stuff --------------------------------------------------------------
  
  # non-standard cal workaround
  nc_rcm <- nc_open(file_rcm)
  raw_times <- ncdf4.helpers::nc.get.time.series(nc_rcm)
  mapped_times <- eurocordexr::map_non_standard_calendar(raw_times)
  dates_rcm <- mapped_times$dates_full
  nc_close(nc_rcm)
  
  # outfile ---------------------------------------------------------
  
  dir_create(path_dir(file_out))
  create_emtpy_netcdf(file_template = file_obs_orog,
                      file_out = file_out,
                      l_varinfo = l_nc_info[[i_var]],
                      date_period = date_rcm_sub,
                      overwrite = F)
  
  # main proc ---------------------------------------------------------------
  
  nc_out <- nc_open(file_out, write = T)
  i_nc_sync <- 0
  
  for(i_date in seq_along(dates_loop)){
    
    i_month <- month(dates_loop[i_date])
    i_rcm <- mapped_times[dates_loop[i_date] == dates_full, idx_pcict] # for non-standard cal
    
    rs_i <- unwrap(wrap(rs_rcm[[i_rcm]])) # workaround needed?
    rs_i_ds <- resample(rs_i, rs_obs_orog, method = "near")
    # rs_cells_rcm_obs[is.na(rs_obs_orog)] <- NA # mask outside TNAA?
    if(i_var == "pr"){
      rs_i_ds2 <- rs_i_ds * rs_pr_fact[[i_month]]
    } else {
      i_lr <- dat_lr_fix[vv == i_var & month == i_month, lr]
      rs_i_ds2 <- rs_i_ds - i_lr*rs_orog_diff_rcm_obs
    }
    
    
    ncvar_put(nc_out, varid = i_var, vals = values(rs_i_ds2), 
              start = c(1, 1, i_date), count = c(-1, -1, 1))
    
    if(i_nc_sync %% n_nc_sync == 0) nc_sync(nc_out)
  }
  
  nc_close(nc_out)
  
}




