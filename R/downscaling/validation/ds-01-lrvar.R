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

source("R/functions/create_empty_netcdf.R")
# source("R/functions/get_rcm_values2.R")
source("R/functions/inv_sub.R")

# settings - variables ----------------------------------------------------

kk_basis <- 5 # basis for gam (as is for s(orog) and ^2 for s(x,y))
pr_min_nonzero <- 20 # (precip-only) minimum number of non-zero values to fit 
                     # the gam model (total ncell_rcm = 391); 
                     # if below large-scale values are uniformly replicated
conserve_largescale <- T # ensure large-scale values match sum/means of downscaled values?
conserve_largescale_cell <- F # conservation at grid cell level (T) or for whole extent (F)
wrap_gamfit_try <- F # wrap gam fitting proc in try()? -> if T, won't stop for errors, but can produce NA fields

path_out <- "/home/climatedata/downscaling/validation-cv/data-daily/ds-lrvar/"

date_rcm_sub <- as.Date(c("1981-01-01", "2020-12-31"))
dates_loop <- seq(date_rcm_sub[1], date_rcm_sub[2], by = "day")
n_nc_sync <- 200 # intermediate save to nc_out file every n dates

file_obs_orog <- "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc"
rs_template_tnaa <- rast("/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc")


# inventory ---------------------------------------------------------------

dat_inv <- inv_sub()
dat_inv_loop <- dat_inv[experiment == "rcp85" & 
                          variable %in% c("tasmin", "tasmax", "pr")]

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
  
  cells_obs <- which(!is.na(rs_template_tnaa[]))
  cells_obs_na <- which(is.na(rs_template_tnaa[]))
  
  rs_rcm <- rast(file_rcm)
  mat_rcm <- values(rs_rcm, mat = T)
  
  dt_rcm_orog <- as.data.table(rs_rcm_orog, xy = T, na.rm = F)
  dt_obs_orog <- as.data.table(rs_obs_orog, xy = T, na.rm = F)
  
  if(conserve_largescale & conserve_largescale_cell){
    # create xy lookup rasters crespi-rcm
    rs_rcm_cells <- rs_rcm_orog
    rs_rcm_cells[] <- 1:ncell(rs_rcm_cells)
    rs_cells_rcm_obs <- resample(rs_rcm_cells, rs_obs_orog, method = "near")
    names(rs_cells_rcm_obs) <- "rcm_cell"
  }
  
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
    
    if(i_var == "pr" & length(which(vals_rcm > 0)) < pr_min_nonzero){
      rs_rcm_i <- rast(rs_rcm_orog)
      rs_rcm_i[] <- vals_rcm
      rs_rcm_i2 <- resample(rs_rcm_i, rs_obs_orog, "near")
      vals_out <- rs_rcm_i2[]
    } else {
      gm_family <- if(i_var == "pr") tw() else gaussian()
      
      dt_rcm <- cbind(dt_rcm_orog, vals_rcm)
      
      if(wrap_gamfit_try){
        gm_fit <- try({
          gam(vals_rcm ~ s(x, y, k = kk_basis^2) + 
                s(orog, k = kk_basis),
              family = gm_family,
              data = dt_rcm)
        })
        
        if(inherits(gm_fit, "try-error")){
          vals_out <- rep(NA_real_, ncell(rs_obs_orog))
        } else {
          vals_out <- predict(gm_fit, dt_obs_orog, type = "response")
        }
      } else {
        gm_fit <-  gam(vals_rcm ~ s(x, y, k = kk_basis^2) + 
                         s(orog, k = kk_basis),
                       family = gm_family,
                       data = dt_rcm)
        vals_out <- predict(gm_fit, dt_obs_orog, type = "response")
      }
      
      if(conserve_largescale){
        rs_rcm_i <- rast(rs_rcm_orog)
        rs_rcm_i[] <- vals_rcm
        rs_rcm_i2 <- resample(rs_rcm_i, rs_obs_orog, "near")
        if(conserve_largescale_cell){
          rs_ds <- rast(rs_obs_orog)
          rs_ds[] <- vals_out
          rs_ds_agg <- zonal(rs_ds, rs_cells_rcm_obs, as.raster = T)
          rs_ds_scaled <- rs_ds/rs_ds_agg * rs_rcm_i2
          vals_out <- rs_ds_scaled[]
        } else {
          vals_out <- vals_out / mean(vals_out) * mean(rs_rcm_i2[]) 
        }
      }
    }
    
    # vals_out[cells_obs_na] <- NA
    
    ncvar_put(nc_out, varid = i_var, vals = vals_out, 
              start = c(1, 1, i_date), count = c(-1, -1, 1))
    
    if(i_nc_sync %% n_nc_sync == 0) nc_sync(nc_out)
  }
  
  nc_close(nc_out)
  
}




