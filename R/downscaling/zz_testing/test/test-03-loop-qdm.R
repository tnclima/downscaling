# gam test lr example

library(terra)
library(magrittr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(fs)
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

path_out <- "/home/climatedata/downscaling/zz_temp/loop-test-qdm/"

years_train_period <- c(1991,2020)
# wet_day <- F # or numeric threshold, e.g. 0.1
# l_wet_day <- list(tasmax = F, tasmin = F, pr = T) # or numeric threshold, e.g. 0.1
l_ratio <- list(tasmax = F, tasmin = F, pr = T) # ratio in QDM()
cell_match_type <-  "xy" # elev or xy
n_cells <- 1 # number of cells for elev, or width (odd) of square for xy
detrend <- F # detrend tas*  prior to ba? (and add trend back future)

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
# main loop ---------------------------------------------------------------


for(i_inv in 1:nrow(dat_inv_loop)){
  
  i_rcm_name <- dat_inv_loop[i_inv, institute_rcm]
  i_var <- dat_inv_loop[i_inv, variable]
  
  
  file_rcm_orog <- dat_inv[variable == "orog" & institute_rcm == i_rcm_name, list_files[[1]]]
  file_rcm <- dat_inv_loop[i_inv, list_files[[1]]]
  file_obs_orog <- l_file_obs_orog[[i_var]]
  
  file_out <- path(path_out,
                   dat_inv_loop[i_inv, 
                                paste(variable, centers, institute_rcm, 
                                      gcm, experiment, sep = "_")],
                   ext = "nc")
  
  
  if(file_exists(file_out)) next
  
  
  
  
  
  # data --------------------------------------------------------------------
  
  rs_rcm_orog <- rast(file_rcm_orog)
  rs_obs_orog <- rast(file_obs_orog)
  
  cells_obs <- which(!is.na(rs_obs_orog[]))
  cells_obs_na <- which(is.na(rs_obs_orog[]))
  
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
  
  # outfile ---------------------------------------------------------
  
  create_emtpy_netcdf(file_template = file_obs_orog,
                      file_out = file_out,
                      l_varinfo = l_nc_info[[i_var]],
                      date_period = range(dates_rcm),
                      overwrite = F)
  
  
  # main proc ---------------------------------------------------------------
  
  nc_out <- nc_open(file_out, write = T)
  
  for(i_cell in cells_obs){
    
    i_row <- rowFromCell(rs_obs, i_cell)
    i_col <- colFromCell(rs_obs, i_cell)
    vals_obs <- values(rs_obs, mat = F, nrows = 1, ncols = 1,
                       row = i_row, 
                       col = i_col)
    elev_obs <- as.vector(rs_obs_orog)[i_cell]
    
    # detrend obs past by month
    dat_obs <- data.table(date = dates_obs,
                          year = years_obs,
                          year0 = years_obs - min(years_obs),
                          month = months_obs,
                          month_fct = mitmatmisc::month_fct(months_obs),
                          value = vals_obs)
    dat_obs <- dat_obs[year >= years_train_period[1] & year <= years_train_period[2]]
    if(detrend & i_var != "pr"){
      dat_obs_lm <- dat_obs[, broom::tidy(lm(value ~ year0)), month]
      dat_obs <- dat_obs_lm[term == "year0", .(month, slope_year = estimate)] %>% 
        merge(dat_obs)
      dat_obs[, value_detrended := value - slope_year*year0]
      
      # ggplot(dat_obs, aes(year, value))+
      #   geom_point()+
      #   geom_smooth(method = lm)+
      #   facet_wrap(~month_fct)
    }
    
    
    vals_rcm <- get_rcm_values2(i_cell, rs_cells_rcm_obs, mat_rcm,
                                type = cell_match_type, n_cells = n_cells,
                                elev_obs = elev_obs, rs_rcm_orog = rs_rcm_orog)
    
    if(i_var == "pr"){
      vals_rcm <- vals_rcm*24*3600
    } else {
      vals_rcm <- vals_rcm-273.15
    }
    
    # non-standard cal adjustment
    vals_rcm <- vals_rcm[mapped_times$idx_pcict, , drop=F]
    
    dat_rcm <- data.table(date = dates_rcm,
                          year = years_rcm,
                          decade = ceiling(years_rcm/10),
                          year0 = years_rcm - min(years_rcm),
                          month = months_rcm,
                          # month_fct = mitmatmisc::month_fct(months_rcm),
                          # value = vals_rcm,
                          value_ba = NA_real_) %>% 
      cbind(vals_rcm)
    
    dat_rcm_hist <- dat_rcm[year >= years_train_period[1] & 
                              year <= years_train_period[2]] %>% 
      melt(id.vars = c("year", "year0", "month"), measure.vars = patterns("^V"))
    
    if(detrend & i_var != "pr"){
      dat_rcm_hist_lm <- dat_rcm_hist[, broom::tidy(lm(value ~ year0)), .(month, variable)]
      dat_rcm_hist <- dat_rcm_hist_lm[term == "year0", 
                                      .(month, variable, slope_year = estimate)] %>% 
        merge(dat_rcm_hist)
      
      dat_rcm_hist[, value_detrended := value - slope_year*year0]
      
      # ggplot(dat_rcm_hist, aes(year, value))+
      #   geom_point()+
      #   geom_smooth(method = lm)+
      #   facet_grid(variable~month)
    }
    all_decades <- sort(unique(dat_rcm$decade))
    
    value_var <- if(detrend & i_var != "pr") "value_detrended" else "value"
    
    # moving window correction (+-1 month, +-1 decade)
    for(i_month in 1:12){
      i_month_window <- c(12,1:12,1)[1 + i_month+c(-1:1)]
      
      vals_train_obs <- dat_obs[month %in% i_month_window][[value_var]]
      vals_train_rcm <- dat_rcm_hist[month %in% i_month_window][[value_var]]
      
      for(i_decade in all_decades){
        i_decade_window <- i_decade + c(-1:1)
        # detrend future
        dat_rcm_fut_window <- dat_rcm[month %in% i_month_window  & decade %in% i_decade_window] %>% 
          melt(id.vars = c("year", "year0", "decade", "month"), measure.vars = patterns("^V"))
        
        if(detrend){
          dat_rcm_fut_window_lm <- dat_rcm_fut_window[, broom::tidy(lm(value ~ year0)), .(month, variable)]
          dat_rcm_fut_window <- dat_rcm_fut_window_lm[term == "year0", 
                                                      .(month, variable, slope_year = estimate)] %>% 
            merge(dat_rcm_fut_window)
          dat_rcm_fut_window[, value_detrended := value - slope_year*year0]
          
          # ggplot(dat_rcm_fut_window, aes(year, value))+
          #   geom_point()+
          #   geom_smooth(method = lm)+
          #   facet_grid(variable~month)
        }
        vals_future_rcm <- dat_rcm_fut_window[[value_var]] # V1 is main value vector
        l_qdm <- QDM(vals_train_obs, vals_train_rcm, vals_future_rcm,
                     ratio = l_ratio[[i_var]])
        dat_rcm_fut_window[, value_qdm := l_qdm$mhat.p]
        
        if(detrend){
          # add trend back
          dat_rcm_fut_window[, value_qdm := value_qdm + slope_year*year0]
        }
        # update only month and decade in the middle (not moving)
        dat_rcm[month == i_month & decade == i_decade, 
                value_ba := dat_rcm_fut_window[month == i_month & decade == i_decade, value_qdm]]
        
      }
      
    }
    
    vals_out <- dat_rcm$value_ba
    ncvar_put(nc_out, varid = i_var, vals = vals_out, 
              start = c(i_col, i_row, 1), count = c(1, 1, -1))
    
    nc_sync(nc_out)
    
    cat(sprintf("%s - cell#(x,y): %i (%i,%i)", date(), i_cell, i_col, i_row), "\n")
    
  }
  
  # fill with NA rest
  for(i_cell in cells_obs_na){
    
    i_row <- rowFromCell(rs_obs, i_cell)
    i_col <- colFromCell(rs_obs, i_cell)
    
    ncvar_put(nc_out, varid = i_var, vals = rep(NA_real_, nc_out$dim$time$len), 
              start = c(i_col, i_row, 1), count = c(1, 1, -1))
    
    nc_sync(nc_out)
    
    # cat(sprintf("%s - NA cell#(x,y): %i (%i,%i)", date(), i_cell, i_col, i_row), "\n")
  }
  
  
  
  nc_close(nc_out)
  
}
