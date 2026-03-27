# 

library(terra)
library(magrittr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(fs)
library(purrr)
library(foreach)
library(stringr)
# library(ggplot2)
# library(patchwork)
# library(mgcViz)
# library(forcats)
# library(scico)
library(MBC)

source("R/functions/create_empty_netcdf.R")
source("R/functions/inv_sub_reanalysis.R")

# settings - variables ----------------------------------------------------

path_out <- "/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v2/ba-mbcnspat/"

date_rcm_sub <- as.Date(c("1989-01-02", "2008-12-31"))
l_years_train_period <- readRDS("data/random-years-reanalysis.rds")

var_order <- c("tasmax", "tasmin", "pr")
# v1
# var_order_ba <- c("tasmax", "dtr", "pr")
# v_ratio <- c(F, T, T) # as in ?MBC::cccma
# v_trace <- c(Inf, 0, 0.05) # as in ?MBC::cccma
# v2
var_order_ba <- c("tasmax", "tasmin", "pr")
v_ratio <- c(F, F, T) # as in ?MBC::cccma
v_trace <- c(Inf, Inf, 0.05) # as in ?MBC::cccma
names(v_ratio) <- var_order_ba
names(v_trace) <- var_order_ba

temp_mv <- F # temporal moving window +-1 month? 
# cell_match_type <-  "xy" # elev or xy # NOT IMPLEMENTED, fixed xy
# n_cells <- 1 # number of cells for elev, or width (odd) of square for xy # NOT IMPLEMENTED, fixed 1
read_obs_memory <- T # reduces computation time, increases memory usage (a lot for 1km data!) # required T
mbcn_iter <- 15 # iterations of mbcn algorithm (default 30, 10-15 is faster)
n_nc_sync <- 200 # intermediate save to nc_out file every n cells
n_cores <- 4 # parallel computation; bottleneck maybe disk access?

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

dat_inv_loop_mod <- dat_inv_loop[, .(gcm, institute_rcm, experiment, 
                                     ensemble, downscale_realisation)] %>% unique()
  
# main loop ---------------------------------------------------------------

mitmatmisc::init_parallel_ubuntu(n_cores)

zz <- foreach(
  i_inv = 1:nrow(dat_inv_loop_mod),
  .inorder = F
) %dopar% {
  
  i_rcm_name <- dat_inv_loop_mod[i_inv, institute_rcm]
  
  file_rcm_orog <- dat_inv[variable == "orog" & institute_rcm == i_rcm_name, list_files[[1]]]
  dat_inv_loop_mod[i_inv, ] %>% 
    merge(dat_inv_loop) %>% 
    {setNames(unlist(.$list_files), .$variable)} -> files_rcm
  # files_obs_orog <- l_file_obs_orog[var_order] %>% unlist
  
  files_out <- path(path_out,
                    dat_inv_loop_mod[i_inv, 
                                     paste(var_order, institute_rcm, 
                                           gcm, experiment, sep = "_")],
                    ext = "nc")
  names(files_out) <- var_order
  
  if(all(file_exists(files_out)))  return(NULL)
  
  
  # data --------------------------------------------------------------------
  
  rs_rcm_orog <- rast(file_rcm_orog)
  # rss_obs_orog <- rast(files_obs_orog)
  # names(rss_obs_orog) <- var_order
  
  l_rs_rcm <- map(files_rcm, rast)
  l_mat_rcm <- map(l_rs_rcm, \(x) values(x, mat = T))
  l_rs_obs <- map(l_file_obs[var_order], rast)
  l_mat_obs <- map(l_rs_obs, \(x) values(x, mat = T))
  
  obs_all_na <- apply(l_mat_obs$tasmax, 1, \(x) any(is.na(x))) 
  cells_obs <- which(!obs_all_na)
  cells_obs_na <- which(obs_all_na)
  
  dt_rcm_orog <- as.data.table(rs_rcm_orog, xy = T, na.rm = F)
  # dt_obs_orog <- as.data.table(rss_obs_orog, xy = T, na.rm = F)
  
  # time stuff --------------------------------------------------------------
  
  # non-standard cal workaround
  nc_rcm <- nc_open(files_rcm[1])
  raw_times <- ncdf4.helpers::nc.get.time.series(nc_rcm)
  mapped_times <- eurocordexr::map_non_standard_calendar(raw_times)
  dates_rcm <- mapped_times$dates_full
  nc_close(nc_rcm)
  months_rcm <- month(dates_rcm)
  years_rcm <- year(dates_rcm)
  
  l_dates_obs <- map(l_rs_obs, time)
  l_months_obs <- map(l_dates_obs, month)
  l_years_obs <- map(l_dates_obs, year)
  
  # outfile ---------------------------------------------------------
  
  walk(var_order, \(vv){
    create_emtpy_netcdf(file_template = file_nc_template,
                        file_out = files_out[vv],
                        l_varinfo = l_nc_info[[vv]],
                        date_period = date_rcm_sub,
                        overwrite = F)
  })
  
  
  # main proc ---------------------------------------------------------------
  
  l_nc_out <- map(files_out, \(x) nc_open(x, write = T))
  i_nc_sync <- 0
  
  
  dat_obs2 <- foreach(
    i_cell = cells_obs,
    .final = rbindlist
  ) %do% {
    
    l_vals_obs <- map(l_mat_obs, \(mat_obs) mat_obs[i_cell, ])
    
    dat_obs <- map2(l_vals_obs, l_dates_obs, \(values, dates){
      data.table(date = dates, value = values)
    }) %>% 
      rbindlist(idcol = "variable") %>% 
      dcast(date ~ variable)
    
    dat_obs[, year := year(date)]
    dat_obs[, month := month(date)]
    
    dat_obs <- dat_obs[date >= date_rcm_sub[1] & date <= date_rcm_sub[2]]
    dat_obs[, dtr := tasmax - tasmin]
    dat_obs2 <- melt(dat_obs, measure.vars = c(var_order, "dtr"))
    cbind(dat_obs2, icell = i_cell)
  }
  
  dat_rcm <- foreach(
    i_cell = cells_obs,
    .final = rbindlist
  ) %do% {
    
    l_vals_rcm <- map(l_mat_rcm, \(mat_rcm) mat_rcm[i_cell, ])
    l_vals_rcm <- imap(l_vals_rcm, \(x, i_var){
      if(i_var == "pr") x*24*3600 else x-273.15
    })
    
    # non-standard cal adjustment
    l_vals_rcm <- map(l_vals_rcm, \(x) x[mapped_times$idx_pcict])
    l_vals_rcm$dtr <- l_vals_rcm$tasmax - l_vals_rcm$tasmin
    
    dat_rcm <- map(l_vals_rcm, \(x){
      dat_rcm <- data.table(date = dates_rcm,
                            year = years_rcm,
                            decade = ceiling(years_rcm/10),
                            # year0 = years_rcm - min(years_rcm),
                            month = months_rcm,
                            # month_fct = mitmatmisc::month_fct(months_rcm),
                            value = x,
                            value_ba = NA_real_)
    }) %>% list_rbind(names_to = "variable")
    dat_rcm <- dat_rcm[date >= date_rcm_sub[1] & date <= date_rcm_sub[2]]
    cbind(dat_rcm, icell = i_cell)
  }
  
  
  
  for(years_train_period in l_years_train_period){
    
    dat_obs2_hist <- dat_obs2[year %in% years_train_period]
    dat_rcm_hist <- dat_rcm[year %in% years_train_period]
    
    # moving window correction
    for(i_month in 1:12){
      
      if(temp_mv){
        i_month_window <- c(12,1:12,1)[1 + i_month+c(-1:1)]
      } else {
        i_month_window <- i_month 
      }
      
      vals_train_obs <- dat_obs2_hist[month %in% i_month_window &
                                        variable %in% var_order_ba] %>%
        dcast(date ~ variable + icell, value.var = "value") %>% 
        .[, -c("date")] %>% 
        as.matrix() 
      
      vals_train_rcm <- dat_rcm_hist[month %in% i_month_window & 
                                       variable %in% var_order_ba] %>% 
        dcast(date ~ variable + icell, value.var = "value") %>% 
        .[, -c("date")] %>% 
        as.matrix() 
      
      stopifnot(setequal(colnames(vals_train_obs), colnames(vals_train_rcm)))
      
      colorder <- colnames(vals_train_obs)
      vals_train_rcm <- vals_train_rcm[, colorder]
      
      dat_rcm_fut_window <- dat_rcm[month %in% i_month_window  &
                                      ! year %in% years_train_period]
      
      dat_rcm_fut_window2 <- dat_rcm_fut_window[month %in% i_month_window & 
                                                  variable %in% var_order_ba] %>% 
        dcast(date ~ variable + icell, value.var = "value")
      vals_future_rcm <- dat_rcm_fut_window2 %>% 
        .[, -c("date")] %>% 
        as.matrix()
      vals_future_rcm <- vals_future_rcm[, colorder]
      
      
      
      
      l_mbcn <- MBCn(vals_train_obs,
                     vals_train_rcm,
                     vals_future_rcm,
                     ratio.seq = v_ratio[str_split_i(colorder, "_", 1)], 
                     trace = v_trace[str_split_i(colorder, "_", 1)],
                     iter = mbcn_iter,
                     silent = T)
      
      colnames(l_mbcn$mhat.p) <- colorder
      dat_rcm_fut_window3 <- dat_rcm_fut_window2[, .(date)] %>% 
        cbind(as.data.table(l_mbcn$mhat.p)) %>% 
        melt(id.vars = c("date"), variable.name = "variable_icell", 
             value.name = "value_mbcn") 
      dat_rcm_fut_window3[, c("variable", "icell") := tstrsplit(variable_icell, "_", type.convert = T)]
      dat_rcm_fut_window3 <- merge(dat_rcm_fut_window3, dat_rcm_fut_window)
      
      
      
      # update only month and decade in the middle (not moving)
      walk(var_order_ba, \(i_var){
        walk(cells_obs, \(i_cell){
          dat_rcm[month == i_month & 
                    ! year %in% years_train_period &
                    variable == i_var & 
                    icell == i_cell, 
                  value_ba := dat_rcm_fut_window3[month == i_month &
                                                    variable == i_var & 
                                                    icell == i_cell,
                                                  value_mbcn]]
        })
      })
      
    } # end month
    
  } # end years train
  
  walk(cells_obs, \(i_cell){
    
    dat_rcm_out <- dat_rcm[icell == i_cell] %>% 
      dcast(date ~ variable, value.var = "value_ba")
    
    if(all(is.na(dat_rcm_out$tasmin))) dat_rcm_out[, tasmin := tasmax - dtr]
    i_maxmin <- dat_rcm_out[, which(tasmax < tasmin)]
    if(length(i_maxmin) > 0){
      zz_tasmax <- dat_rcm_out[i_maxmin, tasmax]
      zz_tasmin <- dat_rcm_out[i_maxmin, tasmin]
      dat_rcm_out[i_maxmin, tasmax := zz_tasmin]
      dat_rcm_out[i_maxmin, tasmin := zz_tasmax]
    }
    
    i_row <- rowFromCell(l_rs_obs$tasmax, i_cell)
    i_col <- colFromCell(l_rs_obs$tasmax, i_cell)
    
    walk(var_order, \(i_var){
      ncvar_put(l_nc_out[[i_var]], varid = i_var, vals = dat_rcm_out[[i_var]], 
                start = c(i_col, nrow(l_rs_obs$tasmax) - i_row + 1, 1), count = c(1, 1, -1))
    })
    
  })
  
  
  walk(var_order, \(i_var) nc_sync(l_nc_out[[i_var]]))
  
  # fill with NA rest
  i_nc_sync <- 0
  for(i_cell in cells_obs_na){
    i_nc_sync <- i_nc_sync + 1
    
    i_row <- rowFromCell(l_rs_obs$tasmax, i_cell)
    i_col <- colFromCell(l_rs_obs$tasmax, i_cell)
    
    walk(var_order, \(i_var){
      ncvar_put(l_nc_out[[i_var]], varid = i_var, 
                vals = rep(NA_real_, l_nc_out[[i_var]]$dim$time$len), 
                start = c(i_col, nrow(l_rs_obs$tasmax) - i_row + 1, 1), count = c(1, 1, -1))
      if(i_nc_sync %% n_nc_sync == 0) nc_sync(l_nc_out[[i_var]])
    })
    
  }
  
  walk(var_order, \(i_var) nc_close(l_nc_out[[i_var]]))
  
}
