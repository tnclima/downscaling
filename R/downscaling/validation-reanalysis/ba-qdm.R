# 

library(terra)
library(magrittr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(fs)
library(foreach)
# library(ggplot2)
# library(qmap)
library(MBC)

source("R/functions/create_empty_netcdf.R")
# source("R/functions/get_nc_1d.R")
source("R/functions/inv_sub_reanalysis.R")

# settings - variables ----------------------------------------------------

path_out <- "/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v2/ba-qdm/"

date_rcm_sub <- as.Date(c("1989-01-02", "2008-12-31"))
l_years_train_period <- readRDS("data/random-years-reanalysis.rds")

# l_wet_day <- list(tasmax = F, tasmin = F, pr = 0.05) # QM: 0.05 for consistency with QDM()
l_ratio <- list(tasmax = F, tasmin = F, pr = T) # QDM: ratio in QDM()

temp_mv <- F # temporal moving window +-1 month? 
# cell_match_type <-  "xy" # elev or xy # NOT IMPLEMENTED, fixed xy
# n_cells <- 1 # number of cells for elev, or width (odd) of square for xy # NOT IMPLEMENTED, fixed 1
read_obs_memory <- T # reduces computation time, increases memory usage (a lot for 1km data!)
n_nc_sync <- 200 # intermediate save to nc_out file every n cells
n_cores <- 1 # parallel computation; bottleneck maybe disk access?
  
l_file_obs <- list(
  tasmax = "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/tasmax_crespi.nc",
  tasmin = "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/tasmin_crespi.nc",
  pr = "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/pr_crespi.nc"
)

file_nc_template <- "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem.nc"

# l_file_obs_orog <- list(
#   tasmax = "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eobs.nc",
#   tasmin = "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eobs.nc",
#   pr = "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_apgd.nc"
# )

dir_create(path_out)

# inventory ---------------------------------------------------------------

dat_inv <- inv_sub_reanalysis()
dat_inv_loop <- dat_inv[variable %in% c("tasmin", "tasmax", "pr")]

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
  # file_obs_orog <- l_file_obs_orog[[i_var]]
  
  file_out <- path(path_out,
                   dat_inv_loop[i_inv, 
                                paste(variable, institute_rcm, 
                                      gcm, experiment, sep = "_")],
                   ext = "nc")
  
  
  if(file_exists(file_out)) return(NULL)
  
  
  
  
  
  # data --------------------------------------------------------------------
  
  rs_rcm_orog <- rast(file_rcm_orog)
  # rs_obs_orog <- rast(file_obs_orog)
  
  rs_rcm <- rast(file_rcm)
  mat_rcm <- values(rs_rcm, mat = T)
  rs_obs <- rast(l_file_obs[[i_var]])
  if(read_obs_memory) mat_obs <- values(rs_obs, mat = T)
  
  obs_all_na <- apply(mat_obs, 1, \(x) any(is.na(x))) 
  cells_obs <- which(!obs_all_na)
  cells_obs_na <- which(obs_all_na)
  
  dt_rcm_orog <- as.data.table(rs_rcm_orog, xy = T, na.rm = F)
  # dt_obs_orog <- as.data.table(rs_obs_orog, xy = T, na.rm = F)
  
  # time stuff --------------------------------------------------------------
  
  # non-standard cal workaround
  nc_rcm <- nc_open(file_rcm)
  raw_times <- ncdf4.helpers::nc.get.time.series(nc_rcm)
  mapped_times <- eurocordexr::map_non_standard_calendar(raw_times)
  dates_rcm <- mapped_times$dates_full
  nc_close(nc_rcm)
  months_rcm <- month(dates_rcm)
  years_rcm <- year(dates_rcm)
  
  dates_obs <- time(rs_obs)
  months_obs <- month(dates_obs)
  years_obs <- year(dates_obs)
  
  # outfile ---------------------------------------------------------
  
  create_emtpy_netcdf(file_template = file_nc_template,
                      file_out = file_out,
                      l_varinfo = l_nc_info[[i_var]],
                      date_period = date_rcm_sub,
                      overwrite = F)
  
  
  # main proc ---------------------------------------------------------------
  
  nc_out <- nc_open(file_out, write = T)
  i_nc_sync <- 0
  
  for(i_cell in cells_obs){
    
    i_nc_sync <- i_nc_sync + 1
    
    i_row <- rowFromCell(rs_obs, i_cell)
    i_col <- colFromCell(rs_obs, i_cell)
    if(read_obs_memory) {
      vals_obs <- mat_obs[i_cell, ]
    } else {
      vals_obs <- values(rs_obs, mat = F, nrows = 1, ncols = 1,
                         row = i_row,
                         col = i_col)
    }
    # elev_obs <- as.vector(rs_obs_orog)[i_cell]
    
    dat_obs <- data.table(date = dates_obs,
                          year = years_obs,
                          # year0 = years_obs - min(years_obs),
                          month = months_obs,
                          month_fct = mitmatmisc::month_fct(months_obs),
                          value = vals_obs)
    dat_obs <- dat_obs[date >= date_rcm_sub[1] & date <= date_rcm_sub[2]]
    
    vals_rcm <- mat_rcm[i_cell, ]
    
    if(i_var == "pr"){
      vals_rcm <- vals_rcm*24*3600
    } else {
      vals_rcm <- vals_rcm-273.15
    }
    
    # non-standard cal adjustment
    vals_rcm <- vals_rcm[mapped_times$idx_pcict]
    
    dat_rcm <- data.table(date = dates_rcm,
                          year = years_rcm,
                          decade = ceiling(years_rcm/10),
                          # year0 = years_rcm - min(years_rcm),
                          month = months_rcm,
                          # month_fct = mitmatmisc::month_fct(months_rcm),
                          value = vals_rcm,
                          value_ba = NA_real_)
    dat_rcm <- dat_rcm[date >= date_rcm_sub[1] & date <= date_rcm_sub[2]]
    
    for(years_train_period in l_years_train_period){
      
      dat_obs_hist <- dat_obs[year %in% years_train_period]
      dat_rcm_hist <- dat_rcm[year %in% years_train_period]
      
      # moving window correction 
      for(i_month in 1:12){
        
        if(temp_mv){
          i_month_window <- c(12,1:12,1)[1 + i_month+c(-1:1)]
        } else {
          i_month_window <- i_month 
        }
        
        vals_train_obs <- dat_obs_hist[month %in% i_month_window][["value"]]
        vals_train_rcm <- dat_rcm_hist[month %in% i_month_window][["value"]]
        
        dat_rcm_fut_window <- dat_rcm[month %in% i_month_window & 
                                        ! year %in% years_train_period]
        
        vals_future_rcm <- dat_rcm_fut_window[["value"]] 
        l_qdm <- QDM(vals_train_obs, vals_train_rcm, vals_future_rcm,
                     ratio = l_ratio[[i_var]])
        dat_rcm_fut_window[, value_ba := l_qdm$mhat.p]
        
        
        # update only month in the middle (not moving)
        dat_rcm[month == i_month & ! year %in% years_train_period, 
                value_ba := dat_rcm_fut_window[month == i_month, value_ba]]
        
      }
    }
    
    
    vals_out <- dat_rcm$value_ba
    ncvar_put(nc_out, varid = i_var, vals = vals_out, 
              start = c(i_col, nrow(rs_obs) - i_row + 1, 1), count = c(1, 1, -1))
    
    if(i_nc_sync %% n_nc_sync == 0) nc_sync(nc_out)
    
    # cat(sprintf("%s - cell#(x,y): %i (%i,%i)", date(), i_cell, i_col, i_row), "\n")
    
  }
  
  nc_sync(nc_out)
  
  # fill with NA rest
  i_nc_sync <- 0
  for(i_cell in cells_obs_na){
    i_nc_sync <- i_nc_sync + 1
    
    i_row <- rowFromCell(rs_obs, i_cell)
    i_col <- colFromCell(rs_obs, i_cell)
    
    ncvar_put(nc_out, varid = i_var, vals = rep(NA_real_, nc_out$dim$time$len), 
              start = c(i_col, nrow(rs_obs) - i_row + 1, 1), count = c(1, 1, -1))
    
    if(i_nc_sync %% n_nc_sync == 0) nc_sync(nc_out)
    
    # cat(sprintf("%s - NA cell#(x,y): %i (%i,%i)", date(), i_cell, i_col, i_row), "\n")
  }
  
  nc_close(nc_out)
  
}
