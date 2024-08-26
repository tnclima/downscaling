# plot of metrics


library(magrittr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(fs)
library(foreach)
library(stringr)
library(eurocordexr)
library(ggplot2)
library(scico)
library(patchwork)


dat_aux_1km <- nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc",
                             add_xy = T)
dat_aux_1km <- dat_aux_1km[, .(icell, orog, lon = longitude, lat = latitude)]




# tasmin ------------------------------------------------------------------

dat_tasmin <- readRDS("/home/climatedata/downscaling/pca-ked/ds-test-metrics/tasmin.rds")


## domain average ----------------------------------------------------------


dat_tasmin_avg <- dat_tasmin[,
                             lapply(.SD, mean),
                             .(vari, ds, rcm)]

dat_tasmin_avg %>% 
  ggplot(aes(ds, mean_ds - mean_crespi))+
  geom_point()+
  facet_wrap(~rcm)+
  theme_bw()

dat_tasmin_avg %>% 
  ggplot(aes(ds, sd_ds - sd_crespi))+
  geom_point()+
  facet_wrap(~rcm)+
  theme_bw()

dat_tasmin_avg %>% 
  ggplot(aes(ds, corr))+
  geom_point()+
  facet_wrap(~rcm)+
  theme_bw()


## domain and rcm avg ------------------------------------------------------


dat_tasmin_avg2 <- dat_tasmin[,
                              lapply(.SD, mean),
                              .(vari, ds),
                              .SDcols = mean_ds:corr]
dat_tasmin_avg2[, .(ds, mean_ds - mean_crespi, sd_ds - sd_crespi)]



## maps --------------------------------------------------------------------

dat_tasmin %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = mae))+
  geom_raster()+
  scale_fill_viridis_c()+
  facet_grid(ds ~ rcm)+
  theme_bw()

dat_tasmin %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = corr))+
  geom_raster()+
  scale_fill_viridis_c()+
  facet_grid(ds ~ rcm)+
  theme_bw()

dat_tasmin %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = mean_ds - mean_crespi))+
  geom_raster()+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  facet_grid(ds ~ rcm)+
  theme_bw()

dat_tasmin %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = sd_ds - sd_crespi))+
  geom_raster()+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  facet_grid(ds ~ rcm)+
  theme_bw()

dat_tasmin$mean_crespi %>% range
dat_tasmin$mean_ds %>% range

gg1 <- dat_tasmin[ds %in% c("ked1", "ked2", "lm1", "lm2")] %>%
# dat_tasmin %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = mean_ds))+
  geom_raster()+
  scale_fill_viridis_c(limits = c(-15, 10), oob = scales::oob_squish)+
  facet_grid(ds ~ rcm)+
  theme_bw()


gg2 <- dat_tasmin[ds %in% c("ked2")] %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = mean_crespi))+
  geom_raster()+
  scale_fill_viridis_c(limits = c(-15, 10), oob = scales::oob_squish)+
  facet_grid("crespi" ~ rcm)+
  theme_bw()
gg_out <- (gg2/gg1)+plot_layout(heights = c(1, 4))
ggsave(gg_out, 
       filename = "fig/pca-ked/test-results/clim-tasmin.png", 
       width = 16, height = 8)


# tasmin v2/v3 ------------------------------------------------------------

# 1: orog
# 2: PC1
# 3: orog + PC2-6
# 4: PC1 + PC2-6


# v3 from maps: not working with PC1, as expected!

dat_tasmin_v2 <- readRDS("/home/climatedata/downscaling/pca-ked/ds-test-metrics/tasmin_v2.rds")


## domain average ----------------------------------------------------------


dat_tasmin_v2_avg <- dat_tasmin_v2[,
                                   lapply(.SD, mean),
                                   .(vari, ds, rcm)]

dat_tasmin_v2_avg %>% 
  ggplot(aes(ds, mean_ds - mean_crespi))+
  geom_point()+
  facet_wrap(~rcm)+
  theme_bw()

dat_tasmin_v2_avg %>% 
  ggplot(aes(ds, sd_ds/sd_crespi))+
  geom_point()+
  facet_wrap(~rcm)+
  theme_bw()

dat_tasmin_v2_avg %>% 
  ggplot(aes(ds, corr))+
  geom_point()+
  facet_wrap(~rcm)+
  theme_bw()


## domain and rcm avg ------------------------------------------------------


dat_tasmin_v2_avg2 <- dat_tasmin_v2[,
                                    lapply(.SD, mean),
                                    .(vari, ds),
                                    .SDcols = mean_ds:corr]
dat_tasmin_v2_avg2[, .(ds, mean_ds - mean_crespi, sd_ds/sd_crespi)]



## maps --------------------------------------------------------------------

dat_tasmin_v2 %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = mae))+
  geom_raster()+
  scale_fill_viridis_c()+
  facet_grid(ds ~ rcm)+
  theme_bw()

dat_tasmin_v2 %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = corr))+
  geom_raster()+
  scale_fill_viridis_c()+
  facet_grid(ds ~ rcm)+
  theme_bw()

dat_tasmin_v2 %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = mean_ds - mean_crespi))+
  geom_raster()+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  facet_grid(ds ~ rcm)+
  theme_bw()

dat_tasmin_v2 %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = sd_ds/sd_crespi))+
  geom_raster()+
  scale_fill_scico(palette = "vik", midpoint = 1)+
  facet_grid(ds ~ rcm)+
  theme_bw()

dat_tasmin_v2$mean_crespi %>% range
dat_tasmin_v2$mean_ds %>% range

gg1 <- dat_tasmin_v2[ds %in% c("ked2", "ked4", "lm2", "lm4")] %>%
  # dat_tasmin_v2 %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = mean_ds))+
  geom_raster()+
  scale_fill_viridis_c(limits = c(-15, 10), oob = scales::oob_squish)+
  facet_grid(ds ~ rcm)+
  theme_bw()


gg2 <- dat_tasmin_v2[ds %in% c("ked2")] %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = mean_crespi))+
  geom_raster()+
  scale_fill_viridis_c(limits = c(-15, 10), oob = scales::oob_squish)+
  facet_grid("crespi" ~ rcm)+
  theme_bw()
gg_out <- (gg2/gg1)+plot_layout(heights = c(1, 4))
ggsave(gg_out, 
       filename = "fig/pca-ked/test-results/clim-tasmin_v2.png", 
       width = 16, height = 8)




# pr ------------------------------------------------------------------

dat_pr <- readRDS("/home/climatedata/downscaling/pca-ked/ds-test-metrics/pr.rds")


## domain average ----------------------------------------------------------


dat_pr_avg <- dat_pr[,
                     lapply(.SD, mean),
                     .(vari, ds, rcm)]

dat_pr_avg %>% 
  ggplot(aes(ds, mean_ds - mean_crespi))+
  geom_point()+
  facet_wrap(~rcm)+
  theme_bw()

dat_pr_avg %>% 
  ggplot(aes(ds, (mean_ds - mean_crespi)/mean_crespi))+
  geom_point()+
  facet_wrap(~rcm)+
  scale_y_continuous(labels = scales::label_percent())+
  theme_bw()

dat_pr_avg %>% 
  # ggplot(aes(ds, corr))+
  ggplot(aes(ds, wd_corr))+
  geom_point()+
  facet_wrap(~rcm)+
  theme_bw()

dat_pr_avg %>% 
  ggplot(aes(ds, mae))+
  geom_point()+
  facet_wrap(~rcm)+
  theme_bw()

dat_pr_avg %>% 
  ggplot(aes(ds, wd_ds - wd_crespi))+
  geom_point()+
  facet_wrap(~rcm)+
  theme_bw()


## domain and rcm avg ------------------------------------------------------


dat_pr_avg2 <- dat_pr[,
                      lapply(.SD, mean),
                      .(vari, ds),
                      .SDcols = mean_ds:wd_corr]
dat_pr_avg2[, .(ds, mean_ds - mean_crespi, (mean_ds - mean_crespi)/mean_crespi)]



## maps --------------------------------------------------------------------

dat_pr %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = mae))+
  geom_raster()+
  scale_fill_viridis_c()+
  facet_grid(ds ~ rcm)+
  theme_bw()

dat_pr %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = corr))+
  geom_raster()+
  scale_fill_viridis_c()+
  facet_grid(ds ~ rcm)+
  theme_bw()

dat_pr %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = mean_ds - mean_crespi))+
  geom_raster()+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  facet_grid(ds ~ rcm)+
  theme_bw()

dat_pr %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = (mean_ds - mean_crespi)/mean_crespi))+
  geom_raster()+
  scale_fill_scico(palette = "vik", midpoint = 0, limits = c(-1,2), oob = scales::oob_squish)+
  facet_grid(ds ~ rcm)+
  theme_bw()


dat_pr$mean_crespi %>% range
dat_pr$mean_ds %>% range

dat_pr %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = mean_ds))+
  geom_raster()+
  scale_fill_viridis_c(limits = c(1, 8), oob = scales::oob_squish)+
  facet_grid(ds ~ rcm)+
  theme_bw()

gg1 <- dat_pr[ds %in% c("ked1", "ked2", "lm1", "lm2")] %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = mean_ds))+
  geom_raster()+
  scale_fill_viridis_c(limits = c(1, 8), oob = scales::oob_squish)+
  facet_grid(ds ~ rcm)+
  theme_bw()
gg2 <- dat_pr[ds %in% c("ked2")] %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = mean_crespi))+
  geom_raster()+
  scale_fill_viridis_c(limits = c(1, 8), oob = scales::oob_squish)+
  facet_grid("crespi" ~ rcm)+
  theme_bw()
gg_out <- (gg2/gg1)+plot_layout(heights = c(1, 4))
ggsave(gg_out, 
       filename = "fig/pca-ked/test-results/clim-pr.png", 
       width = 16, height = 8)



# tasmin_v2 monthly ----------------------------------------------------------



dat_tasmin_v2_monthly <- readRDS("/home/climatedata/downscaling/pca-ked/ds-test-metrics/monthly-tasmin_v2.rds")


## domain average ----------------------------------------------------------


dat_tasmin_v2_monthly_avg <- dat_tasmin_v2_monthly[,
                                           lapply(.SD, mean),
                                           .(month, vari, ds, rcm)]

dat_tasmin_v2_monthly_avg %>% 
  ggplot(aes(month, mean_ds - mean_crespi, colour = ds))+
  geom_line()+
  facet_wrap(~rcm)+
  theme_bw()

dat_tasmin_v2_monthly_avg %>% 
  ggplot(aes(month, sd_ds/sd_crespi, colour = ds))+
  geom_line()+
  facet_wrap(~rcm)+
  theme_bw()

dat_tasmin_v2_monthly_avg %>% 
  ggplot(aes(month, corr, colour = ds))+
  geom_line()+
  facet_wrap(~rcm)+
  theme_bw()

dat_tasmin_v2_monthly_avg %>% 
  ggplot(aes(ds, mae))+
  geom_boxplot()+
  facet_wrap(~month)+
  theme_bw()


## domain and rcm avg ------------------------------------------------------


dat_tasmin_v2_monthly_avg2 <- dat_tasmin_v2_monthly[,
                                                    lapply(.SD, mean),
                                                    .(month, vari, ds),
                                                    .SDcols = mean_ds:corr]
# dat_tasmin_v2_monthly_avg2[, .(ds, mean_ds - mean_crespi, sd_ds/sd_crespi)]
dat_tasmin_v2_monthly_avg2 %>% 
  ggplot(aes(ds, mean_ds - mean_crespi))+
  geom_point()+
  facet_wrap(~month, scales = "free_y")+
  theme_bw()

dat_tasmin_v2_monthly_avg2 %>% 
  ggplot(aes(ds, corr))+
  geom_point()+
  facet_wrap(~month, scales = "free_y")+
  theme_bw()

dat_tasmin_v2_monthly_avg2 %>% 
  ggplot(aes(ds, mae))+
  geom_point()+
  facet_wrap(~month, scales = "free_y")+
  theme_bw()



## elev and rcm avg ------------------------------------------------------

dat_aux_1km_orog <- dat_aux_1km[!is.na(orog)]
dat_aux_1km_orog$orog %>% summary
dat_aux_1km_orog[, orog_fct := cut(orog, seq(0, 3500, by = 500), dig.lab = 5)]

dat_tasmin_v2_monthly_avg3 <- dat_tasmin_v2_monthly %>% 
  merge(dat_aux_1km_orog) %>% 
  .[,
    lapply(.SD, mean),
    .(month, vari, ds, orog_fct),
    .SDcols = mean_ds:corr]



dat_tasmin_v2_monthly_avg3 %>% 
  ggplot(aes(ds, mean_ds - mean_crespi))+
  geom_point()+
  facet_grid(orog_fct~month, scales = "free_y")+
  theme_bw()

dat_tasmin_v2_monthly_avg3 %>% 
  ggplot(aes(ds, corr))+
  geom_point()+
  facet_grid(orog_fct~month, scales = "free_y")+
  theme_bw()

dat_tasmin_v2_monthly_avg3 %>% 
  ggplot(aes(ds, mae))+
  geom_point()+
  facet_grid(orog_fct~month, scales = "free_y")+
  theme_bw()




## maps --------------------------------------------------------------------

# only 1 month
i_month <- 1


dat_tasmin_v2_monthly[month == i_month] %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = mae))+
  geom_raster()+
  scale_fill_viridis_c()+
  facet_grid(ds ~ rcm)+
  theme_bw()

dat_tasmin_v2_monthly[month == i_month] %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = corr))+
  geom_raster()+
  scale_fill_viridis_c()+
  facet_grid(ds ~ rcm)+
  theme_bw()

dat_tasmin_v2_monthly[month == i_month]  %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = mean_ds - mean_crespi))+
  geom_raster()+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  facet_grid(ds ~ rcm)+
  theme_bw()


# only 1 rcm

i_rcm <- "SMHI-RCA4"

dat_tasmin_v2_monthly[rcm == i_rcm]  %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(lon, lat, fill = mean_ds - mean_crespi))+
  geom_raster()+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  facet_grid(ds ~ month)+
  theme_bw()


# dat_tasmin_v2 %>% 
#   merge(dat_aux_1km) %>% 
#   ggplot(aes(lon, lat, fill = sd_ds/sd_crespi))+
#   geom_raster()+
#   scale_fill_scico(palette = "vik", midpoint = 1)+
#   facet_grid(ds ~ rcm)+
#   theme_bw()
# 
# dat_tasmin_v2$mean_crespi %>% range
# dat_tasmin_v2$mean_ds %>% range
# 
# gg1 <- dat_tasmin_v2[ds %in% c("ked2", "ked4", "lm2", "lm4")] %>%
#   # dat_tasmin_v2 %>% 
#   merge(dat_aux_1km) %>% 
#   ggplot(aes(lon, lat, fill = mean_ds))+
#   geom_raster()+
#   scale_fill_viridis_c(limits = c(-15, 10), oob = scales::oob_squish)+
#   facet_grid(ds ~ rcm)+
#   theme_bw()
# 
# 
# gg2 <- dat_tasmin_v2[ds %in% c("ked2")] %>% 
#   merge(dat_aux_1km) %>% 
#   ggplot(aes(lon, lat, fill = mean_crespi))+
#   geom_raster()+
#   scale_fill_viridis_c(limits = c(-15, 10), oob = scales::oob_squish)+
#   facet_grid("crespi" ~ rcm)+
#   theme_bw()
# gg_out <- (gg2/gg1)+plot_layout(heights = c(1, 4))
# ggsave(gg_out, 
#        filename = "fig/pca-ked/test-results/clim-tasmin_v2.png", 
#        width = 16, height = 8)



