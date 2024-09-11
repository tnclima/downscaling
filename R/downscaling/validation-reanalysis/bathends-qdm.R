# 

library(terra)
library(magrittr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(fs)
library(foreach)
library(stringr)
# library(ggplot2)
# library(patchwork)
# library(mgcViz)
# library(forcats)
# library(scico)
library(MBC)

source("R/functions/create_empty_netcdf.R")
source("R/functions/get_nc_1d.R")
source("R/functions/inv_sub_reanalysis.R")

# settings - variables ----------------------------------------------------

path_out <- "/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v2/"
ba_variants <- c("qdm", "mbcn")

date_rcm_sub <- as.Date(c("1989-01-02", "2008-12-31"))
# l_years_train_period <- readRDS("data/random-years-reanalysis.rds")
l_years_train_period <- readRDS("data/random-years-reanalysis2.rds")

# l_wet_day <- list(tasmax = F, tasmin = F, pr = 0.05) # QM: 0.05 for consistency with QDM()
l_ratio <- list(tasmax = F, tasmin = F, pr = T) # QDM: ratio in QDM()

temp_mv <- F # temporal moving window +-1 month? 
n_nc_sync <- 200 # intermediate save to nc_out file every n cells
n_cores <- 4 # parallel computation; bottleneck maybe disk access; (reads RCM memory, crespi cell-by-cell)

upscaled_crespi <- T # needed info, because different NA cells at boundaries

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

dat_inv <- inv_sub_reanalysis()
dat_inv_loop <- dat_inv[variable %in% c("tasmin", "tasmax", "pr")]

# main loop ---------------------------------------------------------------

mitmatmisc::init_parallel_ubuntu(n_cores)

for(i_ba in ba_variants){
  
  files_ba <- dir_ls(path(path_out, str_c("ba-", i_ba)))
  
  zz <- foreach(
    i_file_ba = files_ba,
    .inorder = F
  ) %dopar% {
    
    i_file_ba_split <- i_file_ba %>% 
      path_file() %>% 
      str_split_1("_")
    
    i_rcm_name <- i_file_ba_split[2]
    i_var <- i_file_ba_split[1]
    file_rcm <- i_file_ba
    
    file_rcm_orog <- dat_inv[variable == "orog" & institute_rcm == i_rcm_name, list_files[[1]]]
    file_obs_orog <- l_file_obs_orog[[i_var]]
    
    file_out <- path(path_out,
                     str_c("ba-", i_ba, "-ds-qdm2"),
                     path_file(i_file_ba))
    
    
    if(file_exists(file_out)) return(NULL)
    
    
    
    
    
    # data --------------------------------------------------------------------
    
    rs_rcm_orog <- rast(file_rcm_orog)
    rs_obs_orog <- rast(file_obs_orog)
    
    if(!upscaled_crespi){
      cells_obs <- which(!is.na(rs_obs_orog[]))
      cells_obs_na <- which(is.na(rs_obs_orog[]))
    }
    
    rs_rcm <- rast(file_rcm)
    mat_rcm <- values(rs_rcm, mat = T)
    rs_obs <- rast(l_file_obs[[i_var]])
    
    dt_rcm_orog <- as.data.table(rs_rcm_orog, xy = T, na.rm = F)
    dt_obs_orog <- as.data.table(rs_obs_orog, xy = T, na.rm = F)
    
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
    
    # create xy lookup rasters crespi-rcm ---------------------------------------------------
    
    rs_rcm_cells <- rs_rcm_orog
    rs_rcm_cells[] <- 1:ncell(rs_rcm_cells)
    
    rs_cells_rcm_obs <- project(rs_rcm_cells, rs_obs_orog, method = "near")
    # rs_cells_rcm_obs[is.na(rs_obs_orog)] <- NA # mask outside TNAA?
    names(rs_cells_rcm_obs) <- "rcm_cell"
    
    
    # update cells na (upscaled crespi) ---------------------------------------------------
    
    if(upscaled_crespi){
      
      # rs_zz <- unwrap(wrap())
      icell_na_upscale <- which(is.na(rs_rcm[[1]][]))
      # which(rs_cells_rcm_obs[] %in% icell_na_upscale)
      
      cells_obs <- intersect(which(!rs_cells_rcm_obs[] %in% icell_na_upscale),
                             which(!is.na(rs_obs_orog[])))
      cells_obs_na <- setdiff(1:ncell(rs_obs_orog), cells_obs)
    }
    
    
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
    
    for(i_cell in cells_obs){
      
      i_nc_sync <- i_nc_sync + 1
      
      i_row <- rowFromCell(rs_obs, i_cell)
      i_col <- colFromCell(rs_obs, i_cell)
      
      vals_obs <- get_nc_1d(l_file_obs[[i_var]], i_row, i_col)
      elev_obs <- as.vector(rs_obs_orog)[i_cell]
      
      dat_obs <- data.table(date = dates_obs,
                            year = years_obs,
                            # year0 = years_obs - min(years_obs),
                            month = months_obs,
                            month_fct = mitmatmisc::month_fct(months_obs),
                            value = vals_obs)
      dat_obs <- dat_obs[date >= date_rcm_sub[1] & date <= date_rcm_sub[2]]
      
      i_cell_rcm <- as.vector(rs_cells_rcm_obs)[i_cell]
      vals_rcm <- mat_rcm[i_cell_rcm, ]
    
      
      # if(i_var == "pr"){
      #   vals_rcm <- vals_rcm*24*3600
      # } else {
      #   vals_rcm <- vals_rcm-273.15
      # }
      
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
          
          # for(i_decade in all_decades){
          # i_decade_window <- i_decade + c(-1:1)
          dat_rcm_fut_window <- dat_rcm[month %in% i_month_window & 
                                          # decade %in% i_decade_window &
                                          ! year %in% years_train_period] 
          
          vals_future_rcm <- dat_rcm_fut_window[["value"]]
          l_qdm <- QDM(vals_train_obs, vals_train_rcm, vals_future_rcm,
                       ratio = l_ratio[[i_var]])
          dat_rcm_fut_window[, value_ba := l_qdm$mhat.p]
          
          # update only month and decade in the middle (not moving)
          dat_rcm[month == i_month & ! year %in% years_train_period, 
                  value_ba := dat_rcm_fut_window[month == i_month, value_ba]]
          
          # }
          
        }
      }
      
      
      vals_out <- dat_rcm$value_ba
      ncvar_put(nc_out, varid = i_var, vals = vals_out, 
                start = c(i_col, i_row, 1), count = c(1, 1, -1))
      
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
                start = c(i_col, i_row, 1), count = c(1, 1, -1))
      
      if(i_nc_sync %% n_nc_sync == 0) nc_sync(nc_out)
      
      # cat(sprintf("%s - NA cell#(x,y): %i (%i,%i)", date(), i_cell, i_col, i_row), "\n")
    }
    
    
    
    nc_close(nc_out)
    
  }
  
    
}
