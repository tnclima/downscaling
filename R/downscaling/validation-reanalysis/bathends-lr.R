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
source("R/functions/inv_sub_reanalysis.R")

# settings - variables ----------------------------------------------------

path_out <- "/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v2/"
ba_variants <- c("qdm", "mbcn")

date_rcm_sub <- as.Date(c("1989-01-02", "2008-12-31"))
n_nc_sync <- 200 # intermediate save to nc_out file every n dates
n_cores <- 1 # parallel computation; bottleneck maybe disk access; (reads RCM layer-by-layer, no need for crespi)

rs_template_tnaa <- rast("/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc")

dat_inv <- inv_sub_reanalysis()

# lapse rate data ---------------------------------------------------------

dat_lr_fix <- readRDS("data/lapse-rates-02-clim-crespi-station.rds")
dat_lr_fix <- dat_lr_fix[vv != "prec" & ds == "crespi_clim", .(month, lr, vv)]
dat_lr_fix[vv == "tmin", vv := "tasmin"]
dat_lr_fix[vv == "tmax", vv := "tasmax"]


# main loop ---------------------------------------------------------------

mitmatmisc::init_parallel_ubuntu(n_cores)

for(i_ba in ba_variants){
  
  files_ba <- dir_ls(path(path_out, str_c("ba-", i_ba)))
  files_ba <- str_subset(files_ba, "pr_", negate = T)
  
  zz <- foreach(
    i_file_ba = files_ba,
    .inorder = F
  ) %dopar% {
    
    i_file_ba_split <- i_file_ba %>% 
      path_file() %>% 
      str_split_1("_")
    
    i_rcm_name <- i_file_ba_split[2]
    i_var <- i_file_ba_split[1]
    
    file_rcm_orog <- dat_inv[variable == "orog" & institute_rcm == i_rcm_name, list_files[[1]]]
    # file_obs_orog <- "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc"
    if(i_var == "pr"){
      file_obs_orog <- "/home/climatedata/obs/orography/crespi_lonlat_1km_precipitation.nc"
    } else {
      file_obs_orog <- "/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc"
    }
    
    file_out <- path(path_out,
                     str_c("ba-", i_ba, "-ds-lr"),
                     path_file(i_file_ba))
    
    if(file_exists(file_out)) return(NULL)
    
    
    # data --------------------------------------------------------------------
    
    rs_rcm_orog <- rast(file_rcm_orog)
    rs_obs_orog <- rast(file_obs_orog)
    
    rs_orog_rcm_obs <- project(rs_rcm_orog, rs_obs_orog, method = "near")
    rs_orog_diff_rcm_obs <- rs_orog_rcm_obs - rs_obs_orog
    
    cells_obs <- which(!is.na(rs_template_tnaa[]))
    cells_obs_na <- which(is.na(rs_template_tnaa[]))
    
    rs_rcm <- rast(i_file_ba)
    
    # time stuff --------------------------------------------------------------
    
    # non-standard cal workaround
    nc_rcm <- nc_open(i_file_ba)
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
    
    for(i_date in seq_along(dates_rcm)){
      
      i_month <- month(dates_rcm[i_date])
      i_rcm <- mapped_times[i_date, idx_pcict] # for non-standard cal
      
      rs_i <- unwrap(wrap(rs_rcm[[i_rcm]])) # workaround needed?
      rs_i_ds <- resample(rs_i, rs_obs_orog, method = "near")
      
      i_lr <- dat_lr_fix[vv == i_var & month == i_month, lr]
      rs_i_ds2 <- rs_i_ds - i_lr*rs_orog_diff_rcm_obs
      
      rs_i_ds2[cells_obs_na] <- NA
      
      ncvar_put(nc_out, varid = i_var, vals = values(rs_i_ds2), 
                start = c(1, 1, i_date), count = c(-1, -1, 1))
      
      if(i_nc_sync %% n_nc_sync == 0) nc_sync(nc_out)
    }
    
    nc_close(nc_out)
    
  }
  
}



