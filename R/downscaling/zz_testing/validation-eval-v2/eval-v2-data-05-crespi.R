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

source("R/functions/etccdi.R")
source("R/functions/snowfall.R")
# pctl <- c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)
pctl <- c(0, 0.05, 0.5, 0.95, 1)
l_years_train_period <- list(c(1981,2000), c(2001,2020))

# crespi ------------------------------------------------------------------



l_file_obs <- list(
  tasmax = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MaxTemp.nc",
  tasmin = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MinTemp.nc",
  pr = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_Prec.nc"
)


l_month <- list()
l_etccdi <- list()

for(i in seq_along(l_years_train_period)){
  
  i_years <- l_years_train_period[[i]]
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
  l_month[[i]] <- dat_summ
  
  dat_summ_annual <- map(
    c("tr", "su", "id", "fd", "rx1day", "r20mm", "wd", "sdii", "cdd"), 
    \(ind){
      xx <- formalArgs(ind)
      fun <- get(ind)
      dat_ref[,
            .(val = fun(xpar)), 
            .(icell, year(date)), 
            env = list(xpar = xx)] %>% 
        .[, .(value = mean(val), variable = ind), icell]
    }) %>% rbindlist
  
  dat_summ_annual[, period20 := str_c(i_years, collapse = "-")]
  l_etccdi[[i]] <- dat_summ_annual
  
}

 
saveRDS(rbindlist(l_month), 
        "/home/climatedata/downscaling/validation-cv/rdata-summary-v2/eval-crespi-month.rds")

saveRDS(rbindlist(l_etccdi), 
        "/home/climatedata/downscaling/validation-cv/rdata-summary-v2/eval-crespi-etccdi.rds")
