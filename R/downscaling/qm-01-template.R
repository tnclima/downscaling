# run an example QM for one file
# cell-by-cell
# 3 month window around month of interest
# detrend series first

library(terra)
library(magrittr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(fs)
source("R/functions/create_empty_netcdf.R")
source("R/functions/get_rcm_values2.R")
library(qmap)

# settings - variables ----------------------------------------------------

years_train_period <- c(1991,2020)
wet_day <- F # or numeric threshold, e.g. 0.1
cell_match_type <-  "elev" # elev or xy
n_cells <-  4 # number of cells for elev, or width (odd) of square for xy


i_var <- "tasmax"
l_nc_info <- list(name = i_var,
                  units = "degC",
                  longname = "Daily Maximum Near-Surface Air Temperature",
                  cell_methods = "time: maximum",
                  standard_name = "air_temperature")

file_rcm_orog <- "/home/climatedata/downscaling/rcm_lonlat_tnaa/orog/orog_EUR-11_CNRM-CERFACS-CNRM-CM5_rcp85_r1i1p1_CNRM-ALADIN63_v2_fx.nc"
file_rcm <- "/home/climatedata/downscaling/rcm_lonlat_tnaa/tasmax/tasmax_EUR-11_CNRM-CERFACS-CNRM-CM5_rcp26_r1i1p1_CNRM-ALADIN63_v2_day_19510101-21001231.nc"

file_obs_orog <- "/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc"
file_obs <- "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MaxTemp.nc"

file_out <- "/home/climatedata/downscaling/zz_temp/test-qm-tasmax-elev-window2.nc"




# create xy lookup rasters crespi-rcm ---------------------------------------------------

rs_rcm_orog <- rast(file_rcm_orog)
rs_rcm_cells <- rs_rcm_orog
rs_rcm_cells[] <- 1:ncell(rs_rcm_cells)

rs_obs_orog <- rast(file_obs_orog)
rs_cells_rcm_obs <- project(rs_rcm_cells, rs_obs_orog, method = "near")
# rs_cells_rcm_obs[is.na(rs_obs_orog)] <- NA
names(rs_cells_rcm_obs) <- "rcm_cell"



# data --------------------------------------------------------------------


cells_obs <- which(!is.na(rs_obs_orog[]))
cells_obs_na <- which(is.na(rs_obs_orog[]))

rs_obs <- rast(file_obs)
rs_rcm <- rast(file_rcm)
mat_rcm <- values(rs_rcm, mat = T)


# time stuff --------------------------------------------------------------

dates_obs <- time(rs_obs)
months_obs <- month(dates_obs)
years_obs <- year(dates_obs)

# check non-standard cal!
nc_rcm <- nc_open(file_rcm)
lgl_standard_calendar <- nc_rcm$dim$time$calendar %in% c("gregorian", "proleptic_gregorian")
if(lgl_standard_calendar){
  dates_rcm <- time(rs_rcm) 
} else {
  raw_times <- ncdf4.helpers::nc.get.time.series(nc_rcm)
  mapped_times <- eurocordexr::map_non_standard_calendar(raw_times)
  dates_rcm <- mapped_times$dates_full
}
nc_close(nc_rcm)
months_rcm <- month(dates_rcm)
years_rcm <- year(dates_rcm)



# outfile ---------------------------------------------------------


create_emtpy_netcdf(file_template = file_obs_orog, 
                    file_out = file_out, 
                    l_varinfo = l_nc_info, 
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
  
  
  vals_rcm <- get_rcm_values2(i_cell, rs_cells_rcm_obs, mat_rcm,
                              type = cell_match_type, n_cells = n_cells,
                              elev_obs = elev_obs, rs_rcm_orog = rs_rcm_orog)
  
  if(i_var == "pr"){
    vals_rcm <- vals_rcm*24*3600
  } else {
    vals_rcm <- vals_rcm-273.15
  }
  
  # detrend past by month
  dat_obs <- data.table(date = dates_obs,
                        year = years_obs,
                        year0 = years_obs - min(years_obs),
                        month = months_obs,
                        month_fct = mitmatmisc::month_fct(months_obs),
                        value = vals_obs)
  dat_obs <- dat_obs[year >= years_train_period[1] & year <= years_train_period[2]]
  dat_obs_lm <- dat_obs[, broom::tidy(lm(value ~ year0)), month]
  dat_obs <- dat_obs_lm[term == "year0", .(month, slope_year = estimate)] %>% 
    merge(dat_obs)
  dat_obs[, value_detrended := value - slope_year*year0]
  
  # ggplot(dat_obs, aes(year, value))+
  #   geom_point()+
  #   geom_smooth(method = lm)+
  #   facet_wrap(~month_fct)

  
  if(!lgl_standard_calendar){
    vals_rcm <- vals_rcm[mapped_times$idx_pcict, , drop=F]
  }
  
  dat_rcm <- data.table(date = dates_rcm,
                        year = years_rcm,
                        year0 = years_rcm - min(years_rcm),
                        month = months_rcm,
                        # month_fct = mitmatmisc::month_fct(months_rcm),
                        # value = vals_rcm,
                        value_qm = NA_real_) %>% 
    cbind(vals_rcm)
  
  dat_rcm_hist <- dat_rcm[year >= years_train_period[1] & 
                            year <= years_train_period[2]] %>% 
    melt(id.vars = c("year", "year0", "month"), measure.vars = patterns("^V"))
  dat_rcm_hist_lm <- dat_rcm_hist[, broom::tidy(lm(value ~ year0)), .(month, variable)]
  dat_rcm_hist <- dat_rcm_hist_lm[term == "year0", 
                                  .(month, variable, slope_year = estimate)] %>% 
    merge(dat_rcm_hist)
  
  dat_rcm_hist[, value_detrended := value - slope_year*year0]
  
  # ggplot(dat_rcm_hist, aes(year, value))+
  #   geom_point()+
  #   geom_smooth(method = lm)+
  #   facet_grid(variable~month)
  
  
  for(i_month in 1:12){
    i_month_window <- c(12,1:12,1)[1 + i_month+c(-1:1)]

    vals_train_obs <- dat_obs[month %in% i_month_window, value_detrended]
    vals_train_rcm <- dat_rcm_hist[month %in% i_month_window, value_detrended]
    
    qm_fit <- fitQmapQUANT(vals_train_obs, vals_train_rcm, wet.day = wet_day)
    vals_future_rcm <- dat_rcm[month == i_month, V1] # V1 is main value vector
    vals_future_rcm_qm <- doQmapQUANT(vals_future_rcm, qm_fit, type = "linear")
    
    dat_rcm[month == i_month, value_qm := vals_future_rcm_qm]
    
  }
  
  vals_out <- dat_rcm$value_qm
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
  
  cat(sprintf("%s - NA cell#(x,y): %i (%i,%i)", date(), i_cell, i_col, i_row), "\n")
}

nc_close(nc_out)




