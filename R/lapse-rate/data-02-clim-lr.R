# calculate lapse rates from climatological data sets (30yr averages)

library(terra)
library(magrittr)
library(data.table)
setDTthreads(4)

# crespi prec -------------------------------------------------------------

rs_crespi_elev_prec <- rast("/home/climatedata/obs/orography/crespi_lonlat_1km_precipitation.nc")
xx_elev <- rs_crespi_elev_prec[] # %>% na.omit %>% as.vector()

f_lr <- function(yy, xx_elev = xx_elev){
  # str(yy); print(yy); return(NULL)
  mm <- cbind("icept" = 1, xx_elev, yy[]) %>% na.omit
  i_wetday <- mm[, 3] > 1
  if(sum(i_wetday) < 5) return(NULL) # only calc if more than some values
  zz <- RcppEigen::fastLmPure(mm[i_wetday, 1:2], mm[i_wetday, 3])
  data.table(lr = zz$coefficients["orog"], 
             frac_nonzero = sum(i_wetday) / nrow(mm), 
             max_prec = max(mm[, 3]))
}

rs_crespi_prec <- rast("/home/climatedata/obs/CRESPI/daily_1km_lonlat/Climatologies_1991_2020_MonthlyPrec.nc")
lr_crespi_prec <- lapply(rs_crespi_prec, f_lr, xx_elev = xx_elev)

dt_crespi_prec <- rbindlist(lr_crespi_prec) %>% 
  cbind(month = month(time(rs_crespi_prec)),
        vv = "prec", 
        ds = "crespi_clim")

# crespi tmin & tmax -------------------------------------------------------------

rs_crespi_elev_temp <- rast("/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc")
xx_elev <- rs_crespi_elev_temp[] # %>% na.omit %>% as.vector()

f_lr <- function(yy, xx_elev = xx_elev){
  # str(yy); print(yy); return(NULL)
  mm <- cbind("icept" = 1, xx_elev, yy[]) %>% na.omit
  zz <- RcppEigen::fastLmPure(mm[, 1:2], mm[, 3])
  zz$coefficients["orog"]
}

rs_crespi_tmin <- rast("/home/climatedata/obs/CRESPI/daily_1km_lonlat/Climatologies_1991_2020_MonthlyMinTemp.nc")
lr_crespi_tmin <- lapply(rs_crespi_tmin, f_lr, xx_elev = xx_elev)

dt_crespi_tmin <- data.table(month = month(time(rs_crespi_tmin)), 
                             lr = unlist(lr_crespi_tmin),
                             vv = "tmin", 
                             ds = "crespi_clim")


rs_crespi_tmax <- rast("/home/climatedata/obs/CRESPI/daily_1km_lonlat/Climatologies_1991_2020_MonthlyMaxTemp.nc")
lr_crespi_tmax <- lapply(rs_crespi_tmax, f_lr, xx_elev = xx_elev)

dt_crespi_tmax <- data.table(month = month(time(rs_crespi_tmax)), 
                             lr = unlist(lr_crespi_tmax),
                             vv = "tmax", 
                             ds = "crespi_clim")


# crespi stn --------------------------------------------------------------

dt_meta <- fread("/home/climatedata/stations/trentino-altoadige-crespi/metadata.csv")
dt_stn <- fs::dir_ls("/home/climatedata/stations/trentino-altoadige-crespi/climatologies/") %>% 
  lapply(fread) %>% 
  rbindlist()
dt_stn <- merge(dt_stn, dt_meta[, .(station = name, ele)])

dt_stn_tmin <- dt_stn[!is.na(tmin),
                      .(lr = RcppEigen::fastLmPure(cbind(1, ele), tmin)$coef[2],
                        nn_stn = .N,
                        max_elev = max(ele),
                        vv = "tmin", 
                        ds = "station_clim"),
                      .(month)]

dt_stn_tmax <- dt_stn[!is.na(tmax),
                      .(lr = RcppEigen::fastLmPure(cbind(1, ele), tmax)$coef[2],
                        nn_stn = .N,
                        max_elev = max(ele),
                        vv = "tmax", 
                        ds = "station_clim"),
                      .(month)]

dt_stn_prec <- dt_stn[prec > 1,
                      .(lr = RcppEigen::fastLmPure(cbind(1, ele), prec)$coef[2],
                        nn_stn = .N,
                        max_elev = max(ele),
                        vv = "prec", 
                        ds = "station_clim"),
                      .(month)]


# save all ----------------------------------------------------------------

dat_out <- rbind(dt_crespi_tmin,
                 dt_crespi_tmax,
                 dt_crespi_prec,
                 dt_stn_tmin,
                 dt_stn_tmax,
                 dt_stn_prec,
                 fill = T)

saveRDS(dat_out, file = "data/lapse-rates-02-clim-crespi-station.rds")
