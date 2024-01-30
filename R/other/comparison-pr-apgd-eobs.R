# compare APGD vs E-OBS precipitaiton over TNAA region

library(terra)
library(lubridate)
library(data.table)
setDTthreads(4)
library(magrittr)
library(ggplot2)

dat_apgd <- eurocordexr::nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/pr_apgd.nc",
                                       date_range = ymd(c("1971-01-01", "2000-12-31")))
dat_eobs <- eurocordexr::nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/pr_eobs_v26.nc",
                                       date_range = ymd(c("1971-01-01", "2000-12-31")))
dat_aux <- eurocordexr::nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eobs.nc", add_xy = T)
dat_aux[, date := NULL]

dat <- merge(dat_apgd[, .(icell, date, pr_apgd = pr)],
             dat_eobs[, .(icell, date, pr_eobs = pr)])
dat <- dat[!is.na(pr_eobs)]


# monthly  ----------------------------------------------------------------



dat_summ <- dat[, 
                .(bias = mean(pr_eobs - pr_apgd),
                  bias_clim_rel = mean(pr_eobs) / mean(pr_apgd),
                  rel_bias = mean(pr_eobs - pr_apgd) / mean(pr_apgd),
                  mae = mean(abs(pr_eobs - pr_apgd)),
                  corr = cor(pr_eobs, pr_apgd)),
                .(icell, month(date))]

dat_plot <- dat_summ %>% merge(dat_aux)

dat_plot %>% 
  ggplot(aes(lon, lat, fill = bias))+
  geom_raster()+
  scale_fill_gradient2()+
  facet_wrap(~month)

dat_plot %>% 
  ggplot(aes(lon, lat, fill = rel_bias))+
  geom_raster()+
  scale_fill_gradient2(labels = scales::label_percent())+
  facet_wrap(~month)

dat_plot %>% 
  ggplot(aes(lon, lat, fill = bias_clim_rel))+
  geom_raster()+
  scale_fill_gradient2(labels = scales::label_percent())+
  facet_wrap(~month)

dat_plot %>% 
  ggplot(aes(lon, lat, fill = mae))+
  geom_raster()+
  scale_fill_viridis_c()+
  facet_wrap(~month)

dat_plot %>% 
  ggplot(aes(lon, lat, fill = corr))+
  geom_raster()+
  scale_fill_viridis_c()+
  facet_wrap(~month)


# annual evolution --------------------------------------------------------



dat_summ2 <- dat[, 
                .(bias = mean(pr_eobs - pr_apgd),
                  bias_clim_rel = mean(pr_eobs) / mean(pr_apgd),
                  mae = mean(abs(pr_eobs - pr_apgd)),
                  corr = cor(pr_eobs, pr_apgd)),
                .(icell, year(date))]

dat_summ2 %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(lon, lat, fill = bias))+
  geom_raster()+
  scale_fill_gradient2()+
  facet_wrap(~year)

dat_summ2 %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(lon, lat, fill = bias_clim_rel))+
  geom_raster()+
  scale_fill_gradient2(labels = scales::label_percent())+
  facet_wrap(~year)

dat_summ2 %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(lon, lat, fill = mae))+
  geom_raster()+
  scale_fill_viridis_c()+
  facet_wrap(~year)



# single cells ------------------------------------------------------------

dat_aux %>% 
  ggplot(aes(lon, lat, fill = icell))+geom_raster()+scale_fill_viridis_c()


dat_1cell <- dat[icell == 200]
ccf(dat_1cell$pr_apgd, dat_1cell$pr_eobs) %>% str

f_corr_lag <- function(x,y){
  obj_ccf <- ccf(x, y, plot = F)
  i_maxcorr <- which.max(obj_ccf$acf)
  data.table(maxcorr = obj_ccf$acf[i_maxcorr], lag_maxcorr = obj_ccf$lag[i_maxcorr])
}

dat_corr_lag <- dat[, f_corr_lag(pr_eobs, pr_apgd), .(icell)]

dat_corr_lag %>% 
  merge(dat_aux) %>% 
  ggplot(aes(lon, lat, fill = factor(lag_maxcorr)))+geom_raster()

dat_corr_lag %>% 
  merge(dat_aux) %>% 
  ggplot(aes(lon, lat, fill = maxcorr))+geom_raster()+scale_fill_viridis_c()



# bias lagged -------------------------------------------------------------

dat2 <- merge(dat, dat_corr_lag)
dat2[, pr_eobs2 := ifelse(lag_maxcorr == 0, pr_eobs, shift(pr_eobs, -1)), by = icell]

dat2_summ <- dat2[!is.na(pr_eobs2), 
                  .(bias = mean(pr_eobs2 - pr_apgd),
                    bias_clim_rel = mean(pr_eobs2) / mean(pr_apgd),
                    rel_bias = mean(pr_eobs2 - pr_apgd) / mean(pr_apgd),
                    mae = mean(abs(pr_eobs2 - pr_apgd)),
                    corr = cor(pr_eobs2, pr_apgd)),
                  .(icell, month(date))]

dat2_plot <- dat2_summ %>% merge(dat_aux)

dat2_plot %>% 
  ggplot(aes(lon, lat, fill = bias))+
  geom_raster()+
  scale_fill_gradient2()+
  facet_wrap(~month)

dat2_plot %>% 
  ggplot(aes(lon, lat, fill = rel_bias))+
  geom_raster()+
  scale_fill_gradient2(labels = scales::label_percent())+
  facet_wrap(~month)

dat2_plot %>% 
  ggplot(aes(lon, lat, fill = bias_clim_rel))+
  geom_raster()+
  scale_fill_gradient2(labels = scales::label_percent())+
  facet_wrap(~month)

dat2_plot %>% 
  ggplot(aes(lon, lat, fill = corr))+
  geom_raster()+
  scale_fill_viridis_c()+
  facet_wrap(~month)



