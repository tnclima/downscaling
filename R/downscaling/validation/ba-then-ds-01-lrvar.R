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

path_out <- "/home/climatedata/downscaling/validation-cv/data-daily/"
ba_variants <- c("qdm", "qm", "mbcn")

date_rcm_sub <- as.Date(c("1981-01-01", "2020-12-31"))
n_nc_sync <- 200 # intermediate save to nc_out file every n dates

dat_inv <- inv_sub()

# main loop ---------------------------------------------------------------

mitmatmisc::init_parallel_ubuntu(6)

for(i_ba in ba_variants){
  
  files_ba <- dir_ls(path(path_out, str_c("ba-", i_ba)))
  
  zz <- foreach(
    i_file_ba = files_ba,
    .inorder = F
  ) %dopar% {
    
    i_file_ba_split <- i_file_ba %>% 
      path_file() %>% 
      str_split_1("_")
    
    i_rcm_name <- i_file_ba_split[3]
    i_var <- i_file_ba_split[1]
    
    file_rcm_orog <- dat_inv[variable == "orog" & institute_rcm == i_rcm_name, list_files[[1]]]
    if(i_var == "pr"){
      file_obs_orog <- "/home/climatedata/obs/orography/crespi_lonlat_1km_precipitation.nc"
    } else {
      file_obs_orog <- "/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc"
    }
    
    file_out <- path(path_out,
                     str_c("ba-", i_ba, "-ds-lrvar"),
                     path_file(i_file_ba))
    
    if(file_exists(file_out)) return(NULL)
    
    
    # data --------------------------------------------------------------------
    
    rs_rcm_orog <- rast(file_rcm_orog)
    rs_obs_orog <- rast(file_obs_orog)
    
    cells_obs <- which(!is.na(rs_obs_orog[]))
    cells_obs_na <- which(is.na(rs_obs_orog[]))
    
    rs_rcm <- rast(i_file_ba)
    mat_rcm <- values(rs_rcm, mat = T)
    
    dt_rcm_orog <- as.data.table(rs_rcm_orog, xy = T, na.rm = F)
    dt_obs_orog <- as.data.table(rs_obs_orog, xy = T, na.rm = F)
    
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
      
      i_rcm <- mapped_times[i_date, idx_pcict] # for non-standard cal
      vals_rcm <- mat_rcm[, i_rcm]
      
      # if(i_var == "pr" & length(unique(vals_rcm)) < 10) next
      
      gm_family <- if(i_var == "pr") tw() else gaussian()
      
      dt_rcm <- cbind(dt_rcm_orog, vals_rcm)
      # e-obs only:
      dt_rcm <- dt_rcm[!is.na(vals_rcm)]
      
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
      
      ncvar_put(nc_out, varid = i_var, vals = vals_out, 
                start = c(1, 1, i_date), count = c(-1, -1, 1))
      
      if(i_nc_sync %% n_nc_sync == 0) nc_sync(nc_out)
    }
    
    nc_close(nc_out)
    
  }
  
}



