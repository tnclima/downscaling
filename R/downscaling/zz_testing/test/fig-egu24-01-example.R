# some figures for EGU24

library(eurocordexr)
library(data.table)
setDTthreads(4)
library(magrittr)
library(ggplot2)
library(patchwork)
library(mgcv)
library(scico)

# i_date <- "1982-01-28" # tasmax smhi
# i_date <- "1993-11-26" # tasmax smhi

i_date <- "1981-10-30" # tasmin smhi
# i_date <- "1982-01-28" # tasmin smhi

# data --------------------------------------------------------------------

dat_icell <- readRDS("/home/climatedata/downscaling/validation-cv/rdata-summary-v1/eval-08-aux-icell-1km-011deg.rds")
dat_aux <- dat_icell[!is.na(orog), .(icell = icell_1km, x, y, orog)]

dt_rcm_orog <- nc_grid_to_dt("/home/climatedata/downscaling/rcm_lonlat_tnaa/orog/orog_EUR-11_MPI-M-MPI-ESM-LR_rcp26_r0i0p0_SMHI-RCA4_v1a_fx.nc",
                             add_xy = T)

file_rcm <- "/home/climatedata/downscaling/rcm_lonlat_tnaa/tasmin/tasmin_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_19700101-20991230.nc"
# non-standard cal workaround
nc_rcm <- ncdf4::nc_open(file_rcm)
raw_times <- ncdf4.helpers::nc.get.time.series(nc_rcm)
mapped_times <- eurocordexr::map_non_standard_calendar(raw_times)
dates_rcm <- mapped_times$dates_full
ncdf4::nc_close(nc_rcm)

dt_rcm <- nc_grid_to_dt(file_rcm,
                        date_range = c(mapped_times[dates_full == i_date, dates_pcict_inter], 
                                       mapped_times[dates_full == i_date, dates_pcict_inter]))

# dt_rcm <- nc_grid_to_dt(file_rcm,
#                         date_range =c(i_date, i_date))



dt_qm <- nc_grid_to_dt("/home/climatedata/downscaling/validation-cv/data-daily-v2/bads-qm/tasmin_3_SMHI-RCA4_MOHC-HadGEM2-ES_rcp85.nc",
                       date_range = c(i_date, i_date),
                       icell_raster_pkg = F)
dt_gam <- nc_grid_to_dt("/home/climatedata/downscaling/validation-cv/data-daily-v1/ds-lrvar/tasmin_3_SMHI-RCA4_MOHC-HadGEM2-ES_rcp85.nc",
                        date_range = c(i_date, i_date),
                        icell_raster_pkg = F)
dt_lr <- nc_grid_to_dt("/home/climatedata/downscaling/validation-cv/data-daily-v1/ds-lrfix/tasmin_3_SMHI-RCA4_MOHC-HadGEM2-ES_rcp85.nc",
                        date_range = c(i_date, i_date),
                       icell_raster_pkg = F)

dt_ds <- rbind(
  dt_qm[, .(icell, tasmin, ff = "qm")],
  dt_gam[, .(icell, tasmin = tasmin - 273.15, ff = "gam")],
  dt_lr[, .(icell, tasmin = tasmin - 273.15, ff = "lr")]
) %>% 
  merge(dat_aux)


# test non-standard cal
dt_rcm2 <- dt_rcm %>% merge(dt_rcm_orog[, .(icell, orog, lon, lat)])
gm1 <- gam(tasmin ~ s(lon, lat, k = 25) + s(orog, k = 5), data = dt_rcm2)



lim_x <- range(dt_rcm2$lon, dt_ds$x)
lim_y <- range(dt_rcm2$lat, dt_ds$y)
lims1 <- range(dt_rcm2$tasmin - 273.15, dt_ds$tasmin)

gg_rcm <- dt_rcm2 %>% 
  ggplot(aes(lon, lat, fill = tasmin - 273.15))+
  geom_raster()+
  scale_fill_viridis_c("tasmin", direction = -1, option = "A", limits = lims1)+
  facet_wrap(~"RCM")+
  coord_fixed()+
  xlim(lim_x)+ylim(lim_y)+
  theme_bw(20)+
  xlab(NULL)+ylab(NULL)

gg_ds <- dt_ds %>% 
  ggplot(aes(x, y, fill = tasmin))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims1)+
  facet_wrap(~ff)+
  coord_fixed()+
  xlim(lim_x)+ylim(lim_y)+
  theme_bw(20)+
  xlab(NULL)+ylab(NULL)

gg_out1 <- (gg_rcm | gg_ds)+
  plot_layout(widths = c(1,3))
  
dt_ds2 <- dt_ds %>% 
  dcast(icell + x + y + orog ~ ff, value.var = "tasmin")

lims_diff <- range(dt_ds2[, gam - qm], dt_ds2[, gam - lr], dt_ds2[, lr - qm])

gg_ds_diff1 <- dt_ds2 %>% 
  ggplot(aes(x, y, fill = gam - qm))+
  geom_raster()+
  scale_fill_scico(palette = "vik", limits = lims_diff, midpoint = 0)+
  coord_fixed()+
  xlim(lim_x)+ylim(lim_y)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)

gg_ds_diff2 <- dt_ds2 %>% 
  ggplot(aes(x, y, fill = gam - lr))+
  geom_raster()+
  scale_fill_scico(palette = "vik", limits = lims_diff, midpoint = 0)+
  coord_fixed()+
  xlim(lim_x)+ylim(lim_y)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)

gg_ds_diff3 <- dt_ds2 %>% 
  ggplot(aes(x, y, fill = lr - qm))+
  geom_raster()+
  scale_fill_scico(palette = "vik", limits = lims_diff, midpoint = 0)+
  coord_fixed()+
  xlim(lim_x)+ylim(lim_y)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)


gg_out2 <- gg_ds_diff1 | gg_ds_diff2 | gg_ds_diff3



ggsave("fig/example-downscaling/egu24-01.png", gg_out1, width = 20, height = 4)
ggsave("fig/example-downscaling/egu24-02.png", gg_out2, width = 16, height = 4)
