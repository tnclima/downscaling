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

date_sub <- as.Date(c("1989-01-02", "2008-12-31"))
l_years_train_period <- readRDS("data/random-years-reanalysis.rds")
pctl <- seq(0, 1, by=0.01)



# elev --------------------------------------------------------------------

file_orog <- "/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc"
# file_orog <- "/home/climatedata/obs/orography/crespi_lonlat_1km_precipitation.nc"

dat_orog <- nc_grid_to_dt(file_orog, add_xy = T)
dat_orog <- dat_orog[!is.na(orog)]

# dat_orog$orog %>% hist(50)
# dat_orog$orog %>% summary

elev_breaks <- seq(0, 3500, by = 250)
dat_orog[, elev_fct := cut(orog, breaks = elev_breaks, dig.lab = 5)]
# summary(dat_orog$elev_fct)


# crespi ------------------------------------------------------------------



l_file_obs <- list(
  tasmax = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MaxTemp.nc",
  tasmin = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MinTemp.nc",
  pr = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_Prec.nc"
)


dat_pr <- nc_grid_to_dt(l_file_obs[["pr"]], date_range = date_sub)
setnames(dat_pr, 3, "pr")
dat_pr <- dat_pr[!is.na(pr)]

dat_tasmin <- nc_grid_to_dt(l_file_obs[["tasmin"]], date_range = date_sub)
setnames(dat_tasmin, 3, "tasmin")
dat_tasmin <- dat_tasmin[!is.na(tasmin)]

dat_tasmax <- nc_grid_to_dt(l_file_obs[["tasmax"]], date_range = date_sub)
setnames(dat_tasmax, 3, "tasmax")
dat_tasmax <- dat_tasmax[!is.na(tasmax)]

dat_ref <- cbind(dat_pr, tasmax = dat_tasmax$tasmax, tasmin = dat_tasmin$tasmin)

dat_ref[, hn := snowfall(pr, tasmax, tasmin)]
# rm(dat_pr, dat_tasmin, dat_tasmax);gc();

dat_ref[, season := mitmatmisc::season_fct(month(date))]
dat_ref <- dat_ref %>% merge(dat_orog[, .(icell, elev_fct)])

dat_summ <- map(c("pr", "tasmin", "tasmax", "hn"), \(x){
  dat_ref[, 
          .(qval = quantile(value, pctl),
            pctl = pctl,
            variable = x),
          .(season),
          env = list(value = x)]
}) %>% rbindlist

dat_summ_icell <- map(c("pr", "tasmin", "tasmax", "hn"), \(x){
  dat_ref[, 
          .(qval = quantile(value, pctl),
            pctl = pctl,
            variable = x),
          .(icell, season),
          env = list(value = x)]
}) %>% rbindlist

dat_summ_elev <- map(c("pr", "tasmin", "tasmax", "hn"), \(x){
  dat_ref[, 
          .(qval = quantile(value, pctl),
            pctl = pctl,
            variable = x),
          .(elev_fct, season),
          env = list(value = x)]
}) %>% rbindlist



dat_summ_annual <- map(
  c("tr", "su", "id", "fd", "rx1day", "r20mm", "wd", "sdii", "cdd"), 
  \(ind){
    xx <- formalArgs(ind)
    fun <- get(ind)
    dat_ref[,
            .(val = fun(xpar)), 
            .(icell, year(date)), 
            env = list(xpar = xx)] %>% 
      .[, .(value = mean(val), variable = ind), .(icell)]
  }) %>% rbindlist


saveRDS(dat_summ,
        "/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-crespi-season.rds")
saveRDS(dat_summ_icell, 
        "/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-crespi-season-icell.rds")
saveRDS(dat_summ_elev, 
        "/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-crespi-season-elev.rds")

saveRDS(dat_summ_annual, 
        "/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-crespi-etccdi.rds")
