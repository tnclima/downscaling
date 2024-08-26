# 

library(terra)
library(magrittr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(fs)
library(foreach)
library(stringr)
library(mgcv)
library(sf)

source("R/functions/create_empty_netcdf.R")
source("R/functions/inv_sub_reanalysis.R")

# settings - variables ----------------------------------------------------

kk_basis <- 5 # basis for gam (as is for s(orog) and ^2 for s(x,y))

path_out <- "/home/climatedata/downscaling/validation-cv-reanalysis/data-daily/ds-gam/"

date_rcm_sub <- as.Date(c("1989-01-02", "2008-12-31"))
dates_loop <- seq(date_rcm_sub[1], date_rcm_sub[2], by = "day")
n_nc_sync <- 200 # intermediate save to nc_out file every n dates
n_cores <- 4 # parallel computation; bottleneck maybe disk access?

file_obs_orog <- "/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc"
rs_template_tnaa <- rast("/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc")

sf_tnaa <- readRDS("data/sf-tnaa/tnaa-latlon.rds") %>% st_as_sf()

# inventory ---------------------------------------------------------------

dat_inv <- inv_sub_reanalysis()
dat_inv_loop <- dat_inv[variable %in% c("tasmin", "tasmax")]

# main loop ---------------------------------------------------------------

mitmatmisc::init_parallel_ubuntu(n_cores)

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
                                paste(variable, institute_rcm, 
                                      gcm, experiment, sep = "_")],
                   ext = "nc")
  
  if(file_exists(file_out)) return(NULL)
  
  # data --------------------------------------------------------------------
  
  rs_rcm_orog <- rast(file_rcm_orog)
  rs_obs_orog <- rast(file_obs_orog)
  
  cells_obs <- which(!is.na(rs_template_tnaa[]))
  cells_obs_na <- which(is.na(rs_template_tnaa[]))
  
  rs_rcm <- rast(file_rcm)
  rs_rcm_tnaa <- mask(rs_rcm, sf_tnaa)
  mat_rcm <- values(rs_rcm_tnaa, mat = T)
  
  dt_rcm_orog <- as.data.table(rs_rcm_orog, xy = T, na.rm = F)
  dt_obs_orog <- as.data.table(rs_obs_orog, xy = T, na.rm = F)
  
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
    
    i_rcm <- mapped_times[dates_loop[i_date] == dates_full, idx_pcict] # for non-standard cal
    vals_rcm <- mat_rcm[, i_rcm]
    
    dt_rcm <- cbind(dt_rcm_orog, vals_rcm)
    
    dt_rcm[, vals_rcm := vals_rcm - 273.15]
    
    # subset to tnaa
    dt_rcm <- dt_rcm[!is.na(vals_rcm)]
      
    gm_fit <-  gam(vals_rcm ~ s(x, y, k = kk_basis^2) + 
                     s(orog, k = kk_basis),
                   # family = gm_family,
                   data = dt_rcm)
    vals_out <- predict(gm_fit, dt_obs_orog, type = "response")
    
    # vals_out[cells_obs_na] <- NA
    
    ncvar_put(nc_out, varid = i_var, vals = vals_out, 
              start = c(1, 1, i_date), count = c(-1, -1, 1))
    
    if(i_nc_sync %% n_nc_sync == 0) nc_sync(nc_out)
  }
  
  nc_close(nc_out)
  
}




