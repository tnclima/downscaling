# 

library(terra)
library(magrittr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(fs)
library(purrr)
library(foreach)
# library(ggplot2)
# library(patchwork)
# library(mgcViz)
# library(forcats)
# library(scico)
library(MBC)

source("R/functions/create_empty_netcdf.R")
source("R/functions/get_rcm_values2.R")
source("R/functions/get_nc_1d.R")
source("R/functions/inv_sub.R")

# settings - variables ----------------------------------------------------

path_out <- "/home/climatedata/downscaling/validation-cv/data-daily-v3/bads-mbcn/"

date_rcm_sub <- as.Date(c("1981-01-01", "2020-12-31"))
# l_years_train_period <- list(c(1981:2000), c(2001:2020))
l_years_train_period <- list(c(1981+0:19*2), c(1981+0:19*2+1))

var_order <- c("tasmax", "tasmin", "pr")
# v1
# var_order_ba <- c("tasmax", "dtr", "pr")
# v_ratio <- c(F, T, T) # as in ?MBC::cccma
# v_trace <- c(Inf, 0, 0.05) # as in ?MBC::cccma
# v2
var_order_ba <- c("tasmax", "tasmin", "pr")
v_ratio <- c(F, F, T) # as in ?MBC::cccma
v_trace <- c(Inf, Inf, 0.05) # as in ?MBC::cccma

temp_mv <- F # temporal moving window +-1 month? 
cell_match_type <-  "xy" # 'elev' or 'xy'
n_cells <- 1 # number of cells for elev, or width (odd) of square for xy
detrend <- F # detrend tas*  prior to ba? (and add trend back future)
mbcn_iter <- 15 # iterations of mbcn algorithm (default 30, 10-15 is faster)
n_nc_sync <- 200 # intermediate save to nc_out file every n cells
n_cores <- 6 # parallel computation; bottleneck maybe disk access?

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

dir_create(path_out)

# inventory ---------------------------------------------------------------

dat_inv <- inv_sub()
dat_inv_loop <- dat_inv[experiment == "rcp85" & 
                          variable %in% c("tasmin", "tasmax", "pr")]

dat_inv_loop_mod <- dat_inv_loop[, .(gcm, institute_rcm, experiment, 
                                     ensemble, downscale_realisation, centers)] %>% unique()
  
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
  files_obs_orog <- l_file_obs_orog[var_order] %>% unlist
  
  files_out <- path(path_out,
                    dat_inv_loop_mod[i_inv, 
                                     paste(var_order, centers, institute_rcm, 
                                           gcm, experiment, sep = "_")],
                    ext = "nc")
  names(files_out) <- var_order
  
  if(all(file_exists(files_out)))  return(NULL)
  
  
  # data --------------------------------------------------------------------
  
  rs_rcm_orog <- rast(file_rcm_orog)
  rss_obs_orog <- rast(files_obs_orog)
  names(rss_obs_orog) <- var_order
  
  cells_obs <- which(!is.na(rss_obs_orog$tasmax[]))
  cells_obs_na <- which(is.na(rss_obs_orog$tasmax[]))
  
  l_rs_rcm <- map(files_rcm, rast)
  l_mat_rcm <- map(l_rs_rcm, \(x) values(x, mat = T))
  l_rs_obs <- map(l_file_obs[var_order], rast)
  
  dt_rcm_orog <- as.data.table(rs_rcm_orog, xy = T, na.rm = F)
  dt_obs_orog <- as.data.table(rss_obs_orog, xy = T, na.rm = F)
  
  # time stuff --------------------------------------------------------------
  
  # non-standard cal workaround
  nc_rcm <- nc_open(files_rcm[1])
  raw_times <- ncdf4.helpers::nc.get.time.series(nc_rcm)
  mapped_times <- eurocordexr::map_non_standard_calendar(raw_times)
  dates_rcm <- mapped_times$dates_full
  nc_close(nc_rcm)
  months_rcm <- month(dates_rcm)
  years_rcm <- year(dates_rcm)
  
  dates_obs <- time(l_rs_obs[[1]])
  months_obs <- month(dates_obs)
  years_obs <- year(dates_obs)
  
  # create xy lookup rasters crespi-rcm ---------------------------------------------------
  
  rs_rcm_cells <- rs_rcm_orog
  rs_rcm_cells[] <- 1:ncell(rs_rcm_cells)
  
  rs_cells_rcm_obs <- project(rs_rcm_cells, rss_obs_orog, method = "near")
  # rs_cells_rcm_obs[is.na(rs_obs_orog)] <- NA # mask outside TNAA?
  names(rs_cells_rcm_obs) <- "rcm_cell"
  
  # outfile ---------------------------------------------------------
  
  walk(var_order, \(vv){
    create_emtpy_netcdf(file_template = files_obs_orog[vv],
                        file_out = files_out[vv],
                        l_varinfo = l_nc_info[[vv]],
                        date_period = date_rcm_sub,
                        overwrite = F)
  })
  
  
  # main proc ---------------------------------------------------------------
  
  l_nc_out <- map(files_out, \(x) nc_open(x, write = T))
  i_nc_sync <- 0

  for(i_cell in cells_obs){
    
    i_nc_sync <- i_nc_sync + 1
    
    i_row <- rowFromCell(l_rs_obs$tasmax, i_cell)
    i_col <- colFromCell(l_rs_obs$tasmax, i_cell)
 
    l_vals_obs <- map(l_file_obs[var_order], \(x) get_nc_1d(x, i_row, i_col))
    elev_obs <- rss_obs_orog[][i_cell, ]
    
    dat_obs <- data.table(date = dates_obs,
                          year = years_obs,
                          # year0 = years_obs - min(years_obs),
                          month = months_obs,
                          month_fct = mitmatmisc::month_fct(months_obs)) %>% 
      cbind(as.data.table(l_vals_obs))
    dat_obs <- dat_obs[date >= date_rcm_sub[1] & date <= date_rcm_sub[2]]
    dat_obs[, dtr := tasmax - tasmin]
    dat_obs2 <- melt(dat_obs, measure.vars = c(var_order, "dtr"))
    
    
    l_vals_rcm <- map(l_mat_rcm, \(mat_rcm) get_rcm_values2(
      i_cell, rs_cells_rcm_obs, mat_rcm,
      type = cell_match_type, n_cells = n_cells,
      elev_obs = elev_obs, rs_rcm_orog = rs_rcm_orog
    ))
    l_vals_rcm <- imap(l_vals_rcm, \(x, i_var){
      if(i_var == "pr") x*24*3600 else x-273.15
    })
    
    # non-standard cal adjustment
    l_vals_rcm <- map(l_vals_rcm, \(x) x[mapped_times$idx_pcict, , drop=F])
    l_vals_rcm$dtr <- l_vals_rcm$tasmax - l_vals_rcm$tasmin
    
    dat_rcm <- map(l_vals_rcm, \(x){
      dat_rcm <- data.table(date = dates_rcm,
                            year = years_rcm,
                            decade = ceiling(years_rcm/10),
                            # year0 = years_rcm - min(years_rcm),
                            month = months_rcm,
                            # month_fct = mitmatmisc::month_fct(months_rcm),
                            # value = vals_rcm,
                            value_ba = NA_real_) %>% 
        cbind(x)
    }) %>% list_rbind(names_to = "variable")
    dat_rcm <- dat_rcm[date >= date_rcm_sub[1] & date <= date_rcm_sub[2]]
    
    for(years_train_period in l_years_train_period){
      
      dat_obs2_hist <- dat_obs2[year %in% years_train_period]
      
      if(detrend){
        dat_obs2_hist[, year0 := year - min(year)]
        # detrend obs past by month
        dat_obs2_hist_lm <- dat_obs2_hist[variable %in% c("tasmax", "tasmin"), 
                                          broom::tidy(lm(value ~ year0)), 
                                          .(month, variable)]
        dat_obs2_hist <- dat_obs2_hist_lm[term == "year0", .(month, variable, slope_year = estimate)] %>% 
          merge(dat_obs2_hist, all.y = T)
        dat_obs2_hist[variable %in% c("tasmax", "tasmin"), value_detrended := value - slope_year*year0]
        dat_obs2_hist[!variable %in% c("tasmax", "tasmin"), value_detrended := value]
        
        # ggplot(dat_obs2_hist, aes(year, value))+
        #   geom_point()+
        #   geom_smooth(method = lm)+
        #   facet_grid(variable ~ month_fct, scales = "free_y")+
        #   ggtitle(sprintf("col: %i, row: %i, cell: %i", i_col, i_row, i_cell))
        # ggsave(sprintf("fig/test-mbcn/crespi-%i.png", i_cell), width = 20, height = 8)
      }
      
      dat_rcm_hist <- dat_rcm[year %in% years_train_period] %>% 
        melt(id.vars = c("date", "year", "month", "variable"),
             measure.vars = patterns("^V"),
             variable.name = "cell")
      
      if(detrend){
        dat_rcm_hist[, year0 := year - min(year)]
        dat_rcm_hist_lm <- dat_rcm_hist[variable %in% c("tasmax", "tasmin"), 
                                        broom::tidy(lm(value ~ year0)),
                                        .(month, variable, cell)]
        dat_rcm_hist <- dat_rcm_hist_lm[term == "year0", 
                                        .(month, variable, cell, slope_year = estimate)] %>% 
          merge(dat_rcm_hist, all.y = T)
        dat_rcm_hist[variable %in% c("tasmax", "tasmin"), value_detrended := value - slope_year*year0]
        dat_rcm_hist[!variable %in% c("tasmax", "tasmin"), value_detrended := value]
        
        # ggplot(dat_rcm_hist[cell == "V1"], aes(year, value))+
        #   geom_point()+
        #   geom_smooth(method = lm)+
        #   facet_grid(variable~month, scales = "free_y")
      }
      
      
      # all_decades <- sort(unique(dat_rcm$decade))
      
      melt_value_var <- if(detrend) "value_detrended" else "value"
      
      # moving window correction (+-1 month, +-1 decade)
      for(i_month in 1:12){
        
        if(temp_mv){
          i_month_window <- c(12,1:12,1)[1 + i_month+c(-1:1)]
        } else {
          i_month_window <- i_month 
        }
        
        vals_train_obs <- dat_obs2_hist[month %in% i_month_window & variable %in% var_order_ba] %>%
          dcast(date ~ variable, value.var = melt_value_var) %>% 
          .[, -c("date")] %>% 
          as.matrix() %>% 
          .[, var_order_ba]
        vals_train_rcm <- dat_rcm_hist[month %in% i_month_window & variable %in% var_order_ba] %>% 
          dcast(cell + date ~ variable, value.var = melt_value_var) %>% 
          .[, -c("cell", "date")] %>% 
          as.matrix() %>% 
          .[, var_order_ba]
        
        # for(i_decade in all_decades){
          # i_decade_window <- i_decade + c(-1:1)
          # detrend future
          dat_rcm_fut_window <- dat_rcm[month %in% i_month_window  &
                                          # decade %in% i_decade_window & 
                                          !year %in% years_train_period] %>% 
            melt(id.vars = c("date", "year", "decade", "month", "variable"), 
                 measure.vars = patterns("^V"),
                 variable.name = "cell")
          if(detrend){
            dat_rcm_fut_window[, year0 := year - min(year)]
            dat_rcm_fut_window_lm <- dat_rcm_fut_window[variable %in% c("tasmax", "tasmin"), 
                                                        broom::tidy(lm(value ~ year0)), 
                                                        .(month, variable, cell)]
            dat_rcm_fut_window <- dat_rcm_fut_window_lm[term == "year0", 
                                                        .(month, variable, cell, slope_year = estimate)] %>% 
              merge(dat_rcm_fut_window, all.y = T)
            dat_rcm_fut_window[variable %in% c("tasmax", "tasmin"), value_detrended := value - slope_year*year0]
            dat_rcm_fut_window[!variable %in% c("tasmax", "tasmin"), value_detrended := value]
            
            # ggplot(dat_rcm_fut_window, aes(year, value))+
            #   geom_point()+
            #   geom_smooth(method = lm)+
            #   facet_grid(variable~month)
          }
          
          
          dat_rcm_fut_window2 <- dat_rcm_fut_window[month %in% i_month_window & 
                                                      variable %in% var_order_ba] %>% 
            dcast(cell + date ~ variable, value.var = melt_value_var)
          vals_future_rcm <- dat_rcm_fut_window2 %>% 
            .[, -c("cell", "date")] %>% 
            as.matrix() %>% 
            .[, var_order_ba]
          
          l_mbcn <- MBCn(vals_train_obs, vals_train_rcm, vals_future_rcm,
                         ratio.seq = v_ratio, trace = v_trace, iter = mbcn_iter,
                         silent = T)
          
          colnames(l_mbcn$mhat.p) <- var_order_ba
          dat_rcm_fut_window3 <- dat_rcm_fut_window2[, .(cell, date)] %>% 
            cbind(as.data.table(l_mbcn$mhat.p)) %>% 
            melt(id.vars = c("cell", "date"), value.name = "value_mbcn") %>% 
            merge(dat_rcm_fut_window)
          
          if(detrend){
            # add trend back
            dat_rcm_fut_window3[variable %in% c("tasmax", "tasmin"), 
                                value_mbcn := value_mbcn + slope_year*year0]
          }
          
          # update only month and decade in the middle (not moving)
          walk(var_order_ba, \(i_var){
            dat_rcm[month == i_month & 
                      !year %in% years_train_period &
                      variable == i_var, 
                    value_ba := dat_rcm_fut_window3[cell == "V1" & month == i_month &
                                                      variable == i_var,
                                                    value_mbcn]]
            
          })
          
        # }
        
      }
      
    }
    
   
    
   
    
    dat_rcm_out <- dat_rcm %>% 
      dcast(date ~ variable, value.var = "value_ba")
    
    if(all(is.na(dat_rcm_out$tasmin))) dat_rcm_out[, tasmin := tasmax - dtr]
    i_maxmin <- dat_rcm_out[, which(tasmax < tasmin)]
    if(length(i_maxmin) > 0){
      zz_tasmax <- dat_rcm_out[i_maxmin, tasmax]
      zz_tasmin <- dat_rcm_out[i_maxmin, tasmin]
      dat_rcm_out[i_maxmin, tasmax := zz_tasmin]
      dat_rcm_out[i_maxmin, tasmin := zz_tasmax]
    }
    
    walk(var_order, \(i_var){
      ncvar_put(l_nc_out[[i_var]], varid = i_var, vals = dat_rcm_out[[i_var]], 
                start = c(i_col, i_row, 1), count = c(1, 1, -1))
      if(i_nc_sync %% n_nc_sync == 0) nc_sync(l_nc_out[[i_var]])
    })
    
    
    # cat(sprintf("%s - cell#(x,y): %i (%i,%i)", date(), i_cell, i_col, i_row), "\n")
    
  }
  
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
                start = c(i_col, i_row, 1), count = c(1, 1, -1))
      if(i_nc_sync %% n_nc_sync == 0) nc_sync(l_nc_out[[i_var]])
    })

  }
  
  walk(var_order, \(i_var) nc_close(l_nc_out[[i_var]]))
  
}
