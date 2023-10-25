# calculate local lapse rates for monthly climatologies
library(terra)
library(magrittr)
library(stars)
library(ggplot2)

# spatial window width (square), length of one side (in cell units, here 1km)
# eg. 25 means 12 in each direction from center
# test: 11, 25, 51, 75, 101
ww <- 75

# function to compute lapse rate
fun_lr <- function(x){
  rr_reg <- c(x, rr_orog/1000)
  rr_lapse <- focalReg(rr_reg, w = ww, intercept = T, na.rm = T)
  rr_lapse[is.na(x)] <- NA
  rr_lapse$orog
}


# tmin --------------------------------------------------------------------

rr_orog <- rast("/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc")
rr_clim <- rast("/home/climatedata/obs/CRESPI/daily_1km_lonlat/Climatologies_1981_2010_MonthlyMinTemp.nc")

rr_clim_lr <- sapp(rr_clim, fun_lr)
# rr_clim_lr <- lapply(rr_clim, fun_lr)
names(rr_clim_lr) <- month.abb
# plot(rr_clim_lr)
# st_as_stars(rr_clim_lr) %>% plot

rs_clim_lr <- st_as_stars(rr_clim_lr)
# rs_clim_lr <- st_as_stars(rast(rr_clim_lr))
names(rs_clim_lr) <- "lapse_rate"

gg1 <-
ggplot()+
  geom_stars(data = rs_clim_lr, na.action = na.omit)+
  coord_sf()+
  scale_fill_viridis_c()+
  facet_wrap(~band)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("TMIN lapse rate [degC/km]")

ggsave(paste0("fig/lapse-rate/crespi-1km-tmin-w", ww, ".png"), 
       gg1, width = 12, height = 6)


# tmax --------------------------------------------------------------------

rr_orog <- rast("/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc")
rr_clim <- rast("/home/climatedata/obs/CRESPI/daily_1km_lonlat/Climatologies_1981_2010_MonthlyMaxTemp.nc")

rr_clim_lr <- sapp(rr_clim, fun_lr)
names(rr_clim_lr) <- month.abb
# plot(rr_clim_lr)
# st_as_stars(rr_clim_lr) %>% plot

rs_clim_lr <- st_as_stars(rr_clim_lr)
names(rs_clim_lr) <- "lapse_rate"

gg1 <-
  ggplot()+
  geom_stars(data = rs_clim_lr, na.action = na.omit)+
  coord_sf()+
  scale_fill_viridis_c()+
  facet_wrap(~band)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("TMAX lapse rate [degC/km]")

ggsave(paste0("fig/lapse-rate/crespi-1km-tmax-w", ww, ".png"), 
       gg1, width = 12, height = 6)

# prec --------------------------------------------------------------------


rr_orog <- rast("/home/climatedata/obs/orography/crespi_lonlat_1km_precipitation.nc")
rr_clim <- rast("/home/climatedata/obs/CRESPI/daily_1km_lonlat/Climatologies_1981_2010_MonthlyPrec.nc")

rr_clim_lr <- sapp(rr_clim, fun_lr)
names(rr_clim_lr) <- month.abb
# plot(rr_clim_lr)
# st_as_stars(rr_clim_lr) %>% plot

rs_clim_lr <- st_as_stars(rr_clim_lr)
names(rs_clim_lr) <- "lapse_rate"

gg1 <-
  ggplot()+
  geom_stars(data = rs_clim_lr, na.action = na.omit)+
  coord_sf()+
  scale_fill_viridis_c()+
  facet_wrap(~band)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("PREC lapse rate [mm/km]")

ggsave(paste0("fig/lapse-rate/crespi-1km-prec-w", ww, ".png"), 
       gg1, width = 12, height = 6)
