# calculate lapse rates from daily data sets

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

rs_crespi_prec <- rast("/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_Prec.nc")
lr_crespi_prec <- lapply(rs_crespi_prec, f_lr, xx_elev = xx_elev)

dt_crespi_prec <- rbindlist(lr_crespi_prec) %>% 
  cbind(date = time(rs_crespi_prec),
        vv = "prec", 
        ds = "crespi")

# crespi tmin & tmax -------------------------------------------------------------

rs_crespi_elev_temp <- rast("/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc")
xx_elev <- rs_crespi_elev_temp[] # %>% na.omit %>% as.vector()

f_lr <- function(yy, xx_elev = xx_elev){
  # str(yy); print(yy); return(NULL)
  mm <- cbind("icept" = 1, xx_elev, yy[]) %>% na.omit
  zz <- RcppEigen::fastLmPure(mm[, 1:2], mm[, 3])
  zz$coefficients["orog"]
}

rs_crespi_tmin <- rast("/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MinTemp.nc")
lr_crespi_tmin <- lapply(rs_crespi_tmin, f_lr, xx_elev = xx_elev)

dt_crespi_tmin <- data.table(date = time(rs_crespi_tmin), 
                             lr = unlist(lr_crespi_tmin),
                             vv = "tmin", 
                             ds = "crespi")


rs_crespi_tmax <- rast("/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MaxTemp.nc")
lr_crespi_tmax <- lapply(rs_crespi_tmax, f_lr, xx_elev = xx_elev)

dt_crespi_tmax <- data.table(date = time(rs_crespi_tmax), 
                             lr = unlist(lr_crespi_tmax),
                             vv = "tmax", 
                             ds = "crespi")







# highlander --------------------------------------------------------------

rs_hl_elev <- rast("/home/climatedata/highlander/elevation_lonlat.nc")
rs_hl_elev_tnaa <- crop(rs_hl_elev, rs_crespi_tmin)
xx_elev_tnaa <- rs_hl_elev_tnaa[] # %>% na.omit %>% as.vector()

f_lr <- function(yy, xx_elev_tnaa = xx_elev_tnaa){
  # str(yy); print(yy); return(NULL)
  yy_tnaa <- crop(yy, rs_crespi_tmin)
  mm <- cbind("icept" = 1, xx_elev_tnaa, yy_tnaa[]) %>% na.omit
  zz <- RcppEigen::fastLmPure(mm[, 1:2], mm[, 3])
  zz$coefficients[2]
}

rs_hl_tmin <- rast("/home/climatedata/highlander/Daily_lonlat/rcp85_1989-2070_tasmin.nc")
# rs_test <- rs_hl_tmin[[1:10]]

lr_hl_tmin <- lapply(rs_hl_tmin, f_lr, xx_elev_tnaa = xx_elev_tnaa)

dt_hl_tmin <- data.table(date = as.Date(time(rs_hl_tmin)), 
                         lr = unlist(lr_hl_tmin),
                         vv = "tmin", 
                         ds = "highlander")


rs_hl_tmax <- rast("/home/climatedata/highlander/Daily_lonlat/rcp85_1989-2070_tasmax.nc")

lr_hl_tmax <- lapply(rs_hl_tmax, f_lr, xx_elev_tnaa = xx_elev_tnaa)

dt_hl_tmax <- data.table(date = as.Date(time(rs_hl_tmax)), 
                         lr = unlist(lr_hl_tmax),
                         vv = "tmax", 
                         ds = "highlander")



f_lr <- function(yy, xx_elev_tnaa = xx_elev_tnaa){
  # str(yy); print(yy); return(NULL)
  yy_tnaa <- crop(yy, rs_crespi_tmin)
  mm <- cbind("icept" = 1, xx_elev_tnaa, yy_tnaa[]) %>% na.omit
  i_wetday <- mm[, 3] > 1
  if(sum(i_wetday) < 5) return(NULL) # only calc if more than some values
  zz <- RcppEigen::fastLmPure(mm[i_wetday, 1:2], mm[i_wetday, 3])
  data.table(lr = zz$coefficients[2], 
             frac_nonzero = sum(i_wetday) / nrow(mm), 
             max_prec = max(mm[, 3]))
}

rs_hl_prec <- rast("/home/climatedata/highlander/Daily_lonlat/rcp85_1989-2070_pr.nc")
# rs_test <- rs_hl_prec[[1:10]]

lr_hl_prec <- lapply(rs_hl_prec, f_lr, xx_elev_tnaa = xx_elev_tnaa)

dt_hl_prec <- rbindlist(lr_hl_prec) %>% 
  cbind(date = as.Date(time(rs_hl_prec)),
        vv = "prec", 
        ds = "highlander")



# crespi stn --------------------------------------------------------------

dt_meta <- fread("/home/climatedata/stations/trentino-altoadige-crespi/metadata.csv")
dt_stn <- fs::dir_ls("/home/climatedata/stations/trentino-altoadige-crespi/single-stn/") %>% 
  lapply(fread) %>% 
  rbindlist()
dt_stn <- merge(dt_stn, dt_meta[, .(station = name, ele)])
dt_stn[, date := as.Date(date)]

dt_stn_tmin <- dt_stn[!is.na(tmin),
                      .(lr = RcppEigen::fastLmPure(cbind(1, ele), tmin)$coef[2],
                        nn_stn = .N,
                        max_elev = max(ele),
                        vv = "tmin", 
                        ds = "station"),
                      .(date)]

dt_stn_tmax <- dt_stn[!is.na(tmax),
                      .(lr = RcppEigen::fastLmPure(cbind(1, ele), tmax)$coef[2],
                        nn_stn = .N,
                        max_elev = max(ele),
                        vv = "tmax", 
                        ds = "station"),
                      .(date)]

dt_stn_prec <- dt_stn[prec > 1,
                      .(lr = RcppEigen::fastLmPure(cbind(1, ele), prec)$coef[2],
                        nn_stn = .N,
                        max_elev = max(ele),
                        vv = "prec", 
                        ds = "station"),
                      .(date)]


# save all ----------------------------------------------------------------

dat_out <- rbind(dt_crespi_tmin,
                 dt_crespi_tmax,
                 dt_crespi_prec,
                 dt_hl_tmin,
                 dt_hl_tmax,
                 dt_hl_prec,
                 dt_stn_tmin,
                 dt_stn_tmax,
                 dt_stn_prec,
                 fill = T)

saveRDS(dat_out, file = "data/lapse-rates-01-daily-crespi-highlander-station.rds")