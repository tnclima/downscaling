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
# l_years_train_period <- list(c(1981,2000), c(2001,2020))
# l_years_train_period <- list(c(1981:2000), c(2001:2020))
l_years_train_period <- list(c(1981+0:19*2), c(1981+0:19*2+1))

# crespi ------------------------------------------------------------------



l_file_obs <- list(
  tasmax = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MaxTemp.nc",
  tasmin = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MinTemp.nc",
  pr = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_Prec.nc"
)


dat_pr <- nc_grid_to_dt(l_file_obs[["pr"]], icell_raster_pkg = F)
setnames(dat_pr, 3, "pr")
dat_pr <- dat_pr[!is.na(pr)]

dat_tasmin <- nc_grid_to_dt(l_file_obs[["tasmin"]], icell_raster_pkg = F)
setnames(dat_tasmin, 3, "tasmin")
dat_tasmin <- dat_tasmin[!is.na(tasmin)]

dat_tasmax <- nc_grid_to_dt(l_file_obs[["tasmax"]], icell_raster_pkg = F)
setnames(dat_tasmax, 3, "tasmax")
dat_tasmax <- dat_tasmax[!is.na(tasmax)]

dat_ref <- cbind(dat_pr, tasmax = dat_tasmax$tasmax, tasmin = dat_tasmin$tasmin)

dat_ref[, hn := snowfall(pr, tasmax, tasmin)]
rm(dat_pr, dat_tasmin, dat_tasmax);gc();

dat_ref[, period20 := "odd"]
dat_ref[year(date) %in% l_years_train_period[[2]], period20 := "even"]


dat_summ <- map(c("pr", "tasmin", "tasmax", "hn"), \(x){
  dat_ref[, 
          c(mitmatmisc::calc_pctl(value, pctl),
            mean_value = mean(value),
            variable = x),
          .(period20, icell, month(date)),
          env = list(value = x)]
}) %>% rbindlist

dat_summ_annual <- map(
  c("tr", "su", "id", "fd", "rx1day", "r20mm", "wd", "sdii", "cdd"), 
  \(ind){
    xx <- formalArgs(ind)
    fun <- get(ind)
    dat_ref[,
            .(val = fun(xpar)), 
            .(period20, icell, year(date)), 
            env = list(xpar = xx)] %>% 
      .[, .(value = mean(val), variable = ind), .(period20, icell)]
  }) %>% rbindlist


saveRDS(dat_summ, 
        "/home/climatedata/downscaling/validation-cv/rdata-summary-v3/eval-crespi-month.rds")

saveRDS(dat_summ_annual, 
        "/home/climatedata/downscaling/validation-cv/rdata-summary-v3/eval-crespi-etccdi.rds")
