# 

library(terra)
library(sf)
library(magrittr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(fs)
library(foreach)
library(MBC)

source("R/functions/get_nc_1d.R")
source("R/functions/inv_sub.R")

# settings - variables ----------------------------------------------------

date_rcm_sub <- as.Date(c("1981-01-01", "2010-12-31"))
years_train_period <- 1981:2010

# l_wet_day <- list(tasmax = F, tasmin = F, pr = 0.05) # QM: 0.05 for consistency with QDM()
l_ratio <- list(tasmax = F, tasmin = F, pr = T) # QDM: ratio in QDM()
temp_mv <- T # temporal moving window +-1 month? 

l_file_obs <- list(
  tasmax = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MaxTemp.nc",
  tasmin = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MinTemp.nc",
  pr = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_Prec.nc"
)

# data careser ------------------------------------------------------------

dat_mg <- fread("data-raw/CR2to2023_solrad.csv")

sf_mg <- st_as_sf(dat_mg[1, ], coords = c("X", "Y"), crs = 25832)
# mapview::mapview(sf_mg)
sf_mg_lonlat <- st_transform(sf_mg, 4326)
st_coordinates(sf_mg_lonlat)

dat_mg[, date := lubridate::mdy(datetime)]
dat_mg2 <- dat_mg[, .(date, tasmin = Tair_min, tasmax = Tair_max, pr = Pfilled)]




# inventory ---------------------------------------------------------------

dat_inv <- inv_sub()
dat_inv_loop <- dat_inv[variable %in% c("tasmin", "tasmax", "pr") & experiment == "rcp45"]


# main loop ---------------------------------------------------------------


dat_bads <- foreach(
  i_inv = 1:nrow(dat_inv_loop),
  .final = rbindlist
) %do% {
  
  i_rcm_name <- dat_inv_loop[i_inv, institute_rcm]
  i_var <- dat_inv_loop[i_inv, variable]
  i_exp <- dat_inv_loop[i_inv, experiment]
  
  file_rcm_orog <- dat_inv[variable == "orog" & institute_rcm == i_rcm_name, list_files[[1]]]
  file_rcm <- dat_inv_loop[i_inv, list_files[[1]]]
  
  
  # data --------------------------------------------------------------------
  
  rs_rcm_orog <- rast(file_rcm_orog)
  
  rs_rcm <- rast(file_rcm)
  mat_rcm <- values(rs_rcm, mat = T)
  
  i_cell_rcm <-  cellFromXY(rs_rcm_orog, st_coordinates(sf_mg_lonlat))
  
  # time stuff --------------------------------------------------------------
  
  # non-standard cal workaround
  nc_rcm <- nc_open(file_rcm)
  raw_times <- ncdf4.helpers::nc.get.time.series(nc_rcm)
  mapped_times <- eurocordexr::map_non_standard_calendar(raw_times)
  dates_rcm <- mapped_times$dates_full
  nc_close(nc_rcm)
  months_rcm <- month(dates_rcm)
  years_rcm <- year(dates_rcm)
  

  # main proc ---------------------------------------------------------------
  
  dat_obs <- data.table(date = dat_mg2$date,
                        year = year(dat_mg2$date),
                        month = month(dat_mg2$date),
                        value = dat_mg2[[i_var]])
  dat_obs <- dat_obs[date >= date_rcm_sub[1] & date <= date_rcm_sub[2]]
  
  
  
  
  vals_rcm <- mat_rcm[i_cell_rcm, ]
  
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
  # dat_rcm <- dat_rcm[date >= date_rcm_sub[1] & date <= date_rcm_sub[2]]
  
  
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
    
    all_decades <- sort(unique(dat_rcm$decade))
    
    for(i_decade in all_decades){
      i_decade_window <- i_decade + c(-1:1)
      dat_rcm_fut_window <- dat_rcm[month %in% i_month_window & 
                                      decade %in% i_decade_window] 
      
      vals_future_rcm <- dat_rcm_fut_window[["value"]]
      l_qdm <- QDM(vals_train_obs, vals_train_rcm, vals_future_rcm,
                   ratio = l_ratio[[i_var]])
      dat_rcm_fut_window[, value_ba := l_qdm$mhat.p]
      
      # update only month and decade in the middle (not moving)
      dat_rcm[month == i_month & decade == i_decade, 
              value_ba := dat_rcm_fut_window[month == i_month & decade == i_decade, value_ba]]
      
    }
      
  }


  dat_rcm %>% 
    cbind(dat_inv_loop[i_inv, .(gcm, institute_rcm, experiment, centers,
                                ensemble, downscale_realisation, variable)])
  
}

saveRDS(dat_bads, "data/use-cases/careser-mg.rds")

# dat_bads2 <- dat_bads %>% 
#   dcast(model + institute_rcm + gcm + experiment + date ~ variable, value.var = "value_ba")
# fwrite(dat_bads2, "data/use-cases/careser-mg.csv")

dat_bads3 <- dat_bads %>% 
  dcast(model + date ~ variable, value.var = "value_ba")
fwrite(dat_bads3, "data/use-cases/careser-mg.csv")
