# plot monthly clim
library(terra)
library(magrittr)
library(stars)
library(ggplot2)
library(scico)


# tmin --------------------------------------------------------------------

rr_orog <- rast("/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc")
rr_clim <- rast("/home/climatedata/obs/CRESPI/daily_1km_lonlat/Climatologies_1981_2010_MonthlyMinTemp.nc")

names(rr_clim) <- month.abb
rs_clim <- st_as_stars(rr_clim)
# names(rs_clim_lr) <- "lapse_rate"

gg1 <-
ggplot()+
  geom_stars(data = rs_clim, na.action = na.omit)+
  coord_sf()+
  scale_fill_scico("TMIN", palette = "lajolla")+
  facet_wrap(~ lubridate::month(time))+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("TMIN climatology by month")

ggsave("fig/lapse-rate/crespi-clim-1km-tmin.png", 
       gg1, width = 12, height = 6)

png("fig/lapse-rate/crespi-clim-1km-tmin-scatter.png", 
    width = 12, height = 6, units = "in", res = 300)
plot(rr_clim, rr_orog)
dev.off()


# tmax --------------------------------------------------------------------

rr_orog <- rast("/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc")
rr_clim <- rast("/home/climatedata/obs/CRESPI/daily_1km_lonlat/Climatologies_1981_2010_MonthlyMaxTemp.nc")

names(rr_clim) <- month.abb
rs_clim <- st_as_stars(rr_clim)

gg1 <-
  ggplot()+
  geom_stars(data = rs_clim, na.action = na.omit)+
  coord_sf()+
  scale_fill_scico("TMAX", palette = "lajolla")+
  facet_wrap(~ lubridate::month(time))+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("TMAX climatology by month")

ggsave("fig/lapse-rate/crespi-clim-1km-tmax.png", 
       gg1, width = 12, height = 6)

png("fig/lapse-rate/crespi-clim-1km-tmax-scatter.png", 
    width = 12, height = 6, units = "in", res = 300)
plot(rr_clim, rr_orog)
dev.off()



# prec --------------------------------------------------------------------


rr_orog <- rast("/home/climatedata/obs/orography/crespi_lonlat_1km_precipitation.nc")
rr_clim <- rast("/home/climatedata/obs/CRESPI/daily_1km_lonlat/Climatologies_1981_2010_MonthlyPrec.nc")

names(rr_clim) <- month.abb
rs_clim <- st_as_stars(rr_clim)

gg1 <-
  ggplot()+
  geom_stars(data = rs_clim, na.action = na.omit)+
  coord_sf()+
  scale_fill_scico("PREC", palette = "davos", direction = -1)+
  facet_wrap(~ lubridate::month(time))+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("PREC climatology by month")

ggsave("fig/lapse-rate/crespi-clim-1km-prec.png", 
       gg1, width = 12, height = 6)

png("fig/lapse-rate/crespi-clim-1km-prec-scatter.png", 
    width = 12, height = 6, units = "in", res = 300)
plot(rr_clim, rr_orog)
dev.off()

