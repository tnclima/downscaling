# calc metrics with respect to crespi

library(magrittr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(fs)
library(foreach)
library(stringr)
library(eurocordexr)
library(ggplot2)
library(scico)
library(patchwork)


# settings -------------------------------------------------------

# i_var <- "tasmin"
i_var <- "pr"

# file_obs_orog <- "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_crespi_lonlat_1km_temperature.nc"
file_obs_orog <- "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_crespi_lonlat_1km_precipitation.nc"

# tasmin_v <- "_v2" # suffix version for check of tasmin (_v2, _v3, or empty string)
tasmin_v <- "" # suffix version for check of tasmin (_v2, _v3, or empty string for precip )

pr_th <- 1 # threshold for zero precip (mm)

date_rcm_sub <- as.Date(c("2000-01-01", "2001-12-31"))


dat_inv <- get_inventory("/home/climatedata/downscaling/rcm_lonlat_tnaa_eraint/")
dat_inv[variable == "orog" & institute_rcm == "UHOH-WRF361H", 
        institute_rcm := "IPSL-WRF381P"] # since wrf381 has no fx info
dat_inv_loop <- dat_inv[variable == i_var]
all_rcms <- dat_inv_loop$institute_rcm

ds_all <- c(str_c("lm", 1:4), str_c("ked", 1:4))

l_file_obs <- list(
  tasmax = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MaxTemp.nc",
  tasmin = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MinTemp.nc",
  pr = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_Prec.nc"
)

dat_aux_1km <- nc_grid_to_dt(file_obs_orog, add_xy = T)
dat_aux_1km <- dat_aux_1km[, .(icell, orog, lon = longitude, lat = latitude)]

dat_obs <- nc_grid_to_dt(l_file_obs[[i_var]], date_range = date_rcm_sub)
setnames(dat_obs, 3, "value_crespi")
dat_obs <- dat_obs[!is.na(value_crespi)]

# fun metrics ----------------------------------------------------------------

f_data <- function(i_rcm, i_ds, i_var){
  
  file_ds <- str_c("/home/climatedata/downscaling/pca-ked/ds-test/", i_var, tasmin_v, "/",
                   i_ds, "_", i_rcm, ".nc")
  dat_ds <- nc_grid_to_dt(file_ds, date_range = date_rcm_sub)
  setnames(dat_ds, i_var, "value")


  dat_comp <- merge(dat_obs, dat_ds)  
    
  
  if(i_var == "pr"){
    dat_metrics <- dat_comp[!is.na(value) & !is.na(value_crespi), 
                            .(mean_ds = mean(value),
                              mean_crespi = mean(value_crespi),
                              mae = mean(abs(value - value_crespi)),
                              corr = cor(value, value_crespi),
                              wd_mean_ds = mean(value[value > pr_th]),
                              wd_mean_crespi = mean(value_crespi[value_crespi > pr_th]),
                              wd_ds = mean(value > pr_th),
                              wd_crespi = mean(value_crespi > pr_th),
                              wd_mae = mean(abs(value[value > pr_th & value_crespi > pr_th] - 
                                                  value_crespi[value > pr_th & value_crespi > pr_th])),
                              wd_corr = cor(value[value > pr_th & value_crespi > pr_th],
                                            value_crespi[value > pr_th & value_crespi > pr_th])),
                            .(icell, month(date))]
    
    
  } else {
    dat_metrics <- dat_comp[!is.na(value) & !is.na(value_crespi), 
                            .(mean_ds = mean(value),
                              mean_crespi = mean(value_crespi),
                              sd_ds = sd(value),
                              sd_crespi = sd(value_crespi),
                              mae = mean(abs(value - value_crespi)),
                              corr = cor(value, value_crespi)),
                            .(icell, month(date))]
    
  }
  
  cbind(dat_metrics, vari = i_var, ds = i_ds, rcm = i_rcm)
    
}


dat_out <- foreach(
  i_rcm = all_rcms,
  .final = rbindlist
) %do% {
  
  foreach(
    i_ds = ds_all,
    .final = rbindlist
  ) %do% {
    
    f_data(i_rcm, i_ds, i_var)
    
  }
  
}

saveRDS(
  dat_out, 
  file = str_c("/home/climatedata/downscaling/pca-ked/ds-test-metrics/monthly-", i_var, tasmin_v, ".rds")
)

# i_rcm <- all_rcms[1]
# i_ds <- ds_all[1]

