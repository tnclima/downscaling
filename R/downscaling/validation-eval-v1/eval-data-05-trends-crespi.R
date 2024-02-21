# 


library(eurocordexr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(magrittr)
library(ggplot2)
library(fs)
library(stringr)
library(purrr)
library(foreach)


source("R/functions/snowfall.R")
pctl <- c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)
l_years_train_period <- list(c(1981,2000), c(2001,2020))

# crespi ------------------------------------------------------------------



l_file_obs <- list(
  tasmax = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MaxTemp.nc",
  tasmin = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MinTemp.nc",
  pr = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_Prec.nc"
)


dat_crespi <- foreach(i_years = l_years_train_period,
                      .final = rbindlist) %do% {
  
  date_range <- c(str_c(i_years[1], "-01-01"), str_c(i_years[2], "-12-31"))
  
  dat_pr <- nc_grid_to_dt(l_file_obs[["pr"]], date_range = date_range, icell_raster_pkg = F)
  setnames(dat_pr, 3, "pr")
  dat_pr <- dat_pr[!is.na(pr)]
  
  dat_tasmin <- nc_grid_to_dt(l_file_obs[["tasmin"]], date_range = date_range, icell_raster_pkg = F)
  setnames(dat_tasmin, 3, "tasmin")
  dat_tasmin <- dat_tasmin[!is.na(tasmin)]
  
  dat_tasmax <- nc_grid_to_dt(l_file_obs[["tasmax"]], date_range = date_range, icell_raster_pkg = F)
  setnames(dat_tasmax, 3, "tasmax")
  dat_tasmax <- dat_tasmax[!is.na(tasmax)]
  
  dat_ref <- cbind(dat_pr, tasmax = dat_tasmax$tasmax, tasmin = dat_tasmin$tasmin)
  
  dat_ref[, hn := snowfall(pr, tasmax, tasmin)]
  rm(dat_pr, dat_tasmin, dat_tasmax);gc();
  
  
  dat_summ <- map(c("pr", "tasmin", "tasmax", "hn"), \(x){
    dat_ref[, 
            c(mitmatmisc::calc_pctl(value, pctl),
              mean_value = mean(value),
              variable = x),
            .(icell, month(date)),
            env = list(value = x)]
  }) %>% rbindlist
  
  dat_summ[, period20 := str_c(i_years, collapse = "-")]
  dat_summ
}

saveRDS(dat_crespi, "/home/climatedata/downscaling/validation-cv/rdata-summary/eval-05-trends-crespi.rds")
