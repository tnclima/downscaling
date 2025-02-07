# example figures downscaling tests

library(terra)
library(ggplot2)
library(patchwork)
library(magrittr)
library(data.table)
setDTthreads(4)
library(mgcv)
library(mgcViz)
library(scico)

date1 <- "1987-03-23" # tasmax nearly-linear
date2 <- "1992-05-25" # tasmax nonlinear
date3 <- "2016-01-20" # pr NW
date4 <- "2034-07-14" # pr SE
date5 <- "2021-12-19" # pr gam extrapol high elev
date6 <- "2087-07-06" # pr gam low elev higher

# rcm ---------------------------------------------------------------------

# rs_tasmax_rcm <- rast("/home/climatedata/downscaling/rcm_lonlat_tnaa/tasmax/tasmax_EUR-11_CNRM-CERFACS-CNRM-CM5_rcp85_r1i1p1_IPSL-WRF381P_v2_day_19510101-21001231.nc")
# rs_pr_rcm <- rast("/home/climatedata/downscaling/rcm_lonlat_tnaa/pr/pr_EUR-11_CNRM-CERFACS-CNRM-CM5_rcp85_r1i1p1_IPSL-WRF381P_v2_day_19510101-21001231.nc")
rs_tasmax_rcm <- rast("/home/climatedata/downscaling/rcm_lonlat_tnaa/tasmax/tasmax_EUR-11_IPSL-IPSL-CM5A-MR_rcp85_r1i1p1_IPSL-WRF381P_v1_day_19510101-21001231.nc")
rs_pr_rcm <- rast("/home/climatedata/downscaling/rcm_lonlat_tnaa/pr/pr_EUR-11_IPSL-IPSL-CM5A-MR_rcp85_r1i1p1_IPSL-WRF381P_v1_day_19510101-21001231.nc")

all_times <- time(rs_tasmax_rcm)
# all_times <- time(rs_pr_rcm)

i_lyr1 <- which(all_times == date1)
i_lyr2 <- which(all_times == date2)
i_lyr3 <- which(all_times == date3)
i_lyr4 <- which(all_times == date4)
i_lyr5 <- which(all_times == date5)
i_lyr6 <- which(all_times == date6)

dt_tasmax_rcm1 <- as.data.table(rs_tasmax_rcm[[i_lyr1]], xy = T)
dt_tasmax_rcm2 <- as.data.table(rs_tasmax_rcm[[i_lyr2]], xy = T)
setnames(dt_tasmax_rcm1, c("x", "y", "tasmax"))
setnames(dt_tasmax_rcm2, c("x", "y", "tasmax"))
dt_tasmax_rcm1[, tasmax := tasmax - 273.15]
dt_tasmax_rcm2[, tasmax := tasmax - 273.15]

dt_pr_rcm3 <- as.data.table(rs_pr_rcm[[i_lyr3]], xy = T)
dt_pr_rcm4 <- as.data.table(rs_pr_rcm[[i_lyr4]], xy = T)
setnames(dt_pr_rcm3, c("x", "y", "pr"))
setnames(dt_pr_rcm4, c("x", "y", "pr"))
dt_pr_rcm3[, pr := pr*86400]
dt_pr_rcm4[, pr := pr*86400]

dt_pr_rcm5 <- as.data.table(rs_pr_rcm[[i_lyr5]], xy = T)
dt_pr_rcm6 <- as.data.table(rs_pr_rcm[[i_lyr6]], xy = T)
setnames(dt_pr_rcm5, c("x", "y", "pr"))
setnames(dt_pr_rcm6, c("x", "y", "pr"))
dt_pr_rcm5[, pr := pr*86400]
dt_pr_rcm6[, pr := pr*86400]

# qdm ---------------------------------------------------------------------

rs_tasmax_qdm <- rast("/home/climatedata/downscaling/zz_temp/loop-test-qdm/tasmax_5_IPSL-WRF381P_IPSL-IPSL-CM5A-MR_rcp85.nc")
rs_pr_qdm <- rast("/home/climatedata/downscaling/zz_temp/loop-test-qdm-nodetrend/pr_5_IPSL-WRF381P_IPSL-IPSL-CM5A-MR_rcp85.nc")


dt_tasmax_qdm1 <- as.data.table(rs_tasmax_qdm[[i_lyr1]], xy = T)
dt_tasmax_qdm2 <- as.data.table(rs_tasmax_qdm[[i_lyr2]], xy = T)
setnames(dt_tasmax_qdm1, c("x", "y", "tasmax"))
setnames(dt_tasmax_qdm2, c("x", "y", "tasmax"))

dt_pr_qdm3 <- as.data.table(rs_pr_qdm[[i_lyr3]], xy = T)
dt_pr_qdm4 <- as.data.table(rs_pr_qdm[[i_lyr4]], xy = T)
setnames(dt_pr_qdm3, c("x", "y", "pr"))
setnames(dt_pr_qdm4, c("x", "y", "pr"))

dt_pr_qdm5 <- as.data.table(rs_pr_qdm[[i_lyr5]], xy = T)
dt_pr_qdm6 <- as.data.table(rs_pr_qdm[[i_lyr6]], xy = T)
setnames(dt_pr_qdm5, c("x", "y", "pr"))
setnames(dt_pr_qdm6, c("x", "y", "pr"))

lim_x <- range(dt_tasmax_rcm1$x, dt_tasmax_qdm1$x)
lim_y <- range(dt_tasmax_rcm1$y, dt_tasmax_qdm1$y)

lims1 <- range(dt_tasmax_rcm1$tasmax, dt_tasmax_qdm1$tasmax)
gg1_rcm <- dt_tasmax_rcm1 %>% 
  ggplot(aes(x, y, fill = tasmax))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims1)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("RCM", date1)
gg1_qdm <- dt_tasmax_qdm1 %>% 
  ggplot(aes(x, y, fill = tasmax))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims1)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("QDM", date1)

lims2 <- range(dt_tasmax_rcm2$tasmax, dt_tasmax_qdm2$tasmax)
gg2_rcm <- dt_tasmax_rcm2 %>% 
  ggplot(aes(x, y, fill = tasmax))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims2)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("RCM", date2)
gg2_qdm <- dt_tasmax_qdm2 %>% 
  ggplot(aes(x, y, fill = tasmax))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims2)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("QDM", date2)

gg_out1 <- gg1_rcm + gg1_qdm + gg2_rcm + gg2_qdm
ggsave("fig/example-downscaling/qdm-01-tasmax.png", gg_out1, width = 10, height = 6)


lims3 <- range(dt_pr_rcm3$pr, dt_pr_qdm3$pr)
gg3_rcm <- dt_pr_rcm3 %>% 
  ggplot(aes(x, y, fill = pr))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims3)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("RCM", date3)
gg3_qdm <- dt_pr_qdm3 %>% 
  ggplot(aes(x, y, fill = pr))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims3)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("QDM", date3)

lims4 <- range(dt_pr_rcm4$pr, dt_pr_qdm4$pr)
gg4_rcm <- dt_pr_rcm4 %>% 
  ggplot(aes(x, y, fill = pr))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims4)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("RCM", date4)
gg4_qdm <- dt_pr_qdm4 %>% 
  ggplot(aes(x, y, fill = pr))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims4)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("QDM", date4)

gg_out2a <- gg3_rcm + gg3_qdm + gg4_rcm + gg4_qdm
ggsave("fig/example-downscaling/qdm-02a-pr.png", gg_out2a, width = 10, height = 6)


lims5 <- range(dt_pr_rcm5$pr, dt_pr_qdm5$pr)
gg5_rcm <- dt_pr_rcm5 %>% 
  ggplot(aes(x, y, fill = pr))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims5)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("RCM", date5)
gg5_qdm <- dt_pr_qdm5 %>% 
  ggplot(aes(x, y, fill = pr))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims5)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("QDM", date5)

lims6 <- range(dt_pr_rcm6$pr, dt_pr_qdm6$pr)
gg6_rcm <- dt_pr_rcm6 %>% 
  ggplot(aes(x, y, fill = pr))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims6)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("RCM", date6)
gg6_qdm <- dt_pr_qdm6 %>% 
  ggplot(aes(x, y, fill = pr))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims6)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("QDM", date6)

gg_out2b <- gg5_rcm + gg5_qdm + gg6_rcm + gg6_qdm
ggsave("fig/example-downscaling/qdm-02b-pr.png", gg_out2b, width = 10, height = 6)



# gam ---------------------------------------------------------------------

kk_basis <- 5

rs_rcm_orog <- rast("/home/climatedata/downscaling/rcm_lonlat_tnaa/orog/orog_EUR-11_MPI-M-MPI-ESM-LR_rcp85_r0i0p0_UHOH-WRF361H_v1_fx.nc")
rs_obs_orog_t <- rast("/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc")
rs_obs_orog_p <- rast("/home/climatedata/obs/orography/crespi_lonlat_1km_precipitation.nc")

dt_rcm_orog <- as.data.table(rs_rcm_orog, xy = T, na.rm = F)
dt_obs_orog_t <- as.data.table(rs_obs_orog_t, xy = T, na.rm = F)
dt_obs_orog_p <- as.data.table(rs_obs_orog_p, xy = T, na.rm = F)


# tasmax 1

dt_tasmax_rcm1g <- dt_tasmax_rcm1 %>% merge(dt_rcm_orog)

gm1 <- gam(tasmax ~ s(x, y, k = kk_basis^2) + s(orog, k = kk_basis),
           data = dt_tasmax_rcm1g)
vals_out1 <- predict(gm1, dt_obs_orog_t, type = "response")
dt_obs1 <- dt_obs_orog_t %>% cbind(tasmax = vals_out1)

lims1 <- range(dt_tasmax_rcm1$tasmax, dt_obs1$tasmax, na.rm = T)
gg1_rcm <- dt_tasmax_rcm1 %>% 
  ggplot(aes(x, y, fill = tasmax))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims1)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("RCM", date1)
gg1_gam <- dt_obs1[!is.na(tasmax)] %>% 
  ggplot(aes(x, y, fill = tasmax))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims1)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("GAM", date1)

gg_smooth_1 <- plot(sm(getViz(gm1), 1))+l_fitRaster()
gg_smooth_2 <- plot(sm(getViz(gm1), 2))+l_ciLine()+l_fitLine()
gg_check <- check(getViz(gm1))

gg_out_gam1 <- (gg1_rcm + gg1_gam + 
                  gg_smooth_1$ggObj + gg_smooth_2$ggObj +
                  gg_check[[1]] + gg_check[[4]])+plot_layout(byrow = F)

ggsave("fig/example-downscaling/gam-1-tasmax.png", gg_out_gam1, width = 12, height = 6)



# tasmax 2

dt_tasmax_rcm2g <- dt_tasmax_rcm2 %>% merge(dt_rcm_orog)

gm2 <- gam(tasmax ~ s(x, y, k = kk_basis^2) + s(orog, k = kk_basis),
           data = dt_tasmax_rcm2g)
vals_out2 <- predict(gm2, dt_obs_orog_t, type = "response")
dt_obs2 <- dt_obs_orog_t %>% cbind(tasmax = vals_out2)

lims2 <- range(dt_tasmax_rcm2$tasmax, dt_obs2$tasmax, na.rm = T)
gg2_rcm <- dt_tasmax_rcm2 %>% 
  ggplot(aes(x, y, fill = tasmax))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims2)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("RCM", date2)
gg2_gam <- dt_obs2[!is.na(tasmax)] %>% 
  ggplot(aes(x, y, fill = tasmax))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims2)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("GAM", date2)

gg_smooth_1 <- plot(sm(getViz(gm2), 1))+l_fitRaster()
gg_smooth_2 <- plot(sm(getViz(gm2), 2))+l_ciLine()+l_fitLine()
gg_check <- check(getViz(gm2))

gg_out_gam2 <- (gg2_rcm + gg2_gam + 
                  gg_smooth_1$ggObj + gg_smooth_2$ggObj +
                  gg_check[[1]] + gg_check[[4]])+plot_layout(byrow = F)

ggsave("fig/example-downscaling/gam-2-tasmax.png", gg_out_gam2, width = 12, height = 6)



# pr 3

dt_pr_rcm3g <- dt_pr_rcm3 %>% merge(dt_rcm_orog)

gm3 <- gam(pr ~ s(x, y, k = kk_basis^2) + s(orog, k = kk_basis),
           data = dt_pr_rcm3g, family = tw())
vals_out3 <- predict(gm3, dt_obs_orog_p, type = "response")
dt_obs3 <- dt_obs_orog_p %>% cbind(pr = vals_out3)

lims3 <- range(dt_pr_rcm3$pr, dt_obs3$pr, na.rm = T)
gg3_rcm <- dt_pr_rcm3 %>% 
  ggplot(aes(x, y, fill = pr))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims3)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("RCM", date3)
gg3_gam <- dt_obs3[!is.na(pr)] %>% 
  ggplot(aes(x, y, fill = pr))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims3)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("GAM", date3)

gg_smooth_1 <- plot(sm(getViz(gm3), 1))+l_fitRaster()
gg_smooth_2 <- plot(sm(getViz(gm3), 2))+l_ciLine()+l_fitLine()
gg_check <- check(getViz(gm3))

gg_out_gam3 <- (gg3_rcm + gg3_gam + 
                  gg_smooth_1$ggObj + gg_smooth_2$ggObj +
                  gg_check[[1]] + gg_check[[4]])+plot_layout(byrow = F)

ggsave("fig/example-downscaling/gam-3-pr.png", gg_out_gam3, width = 12, height = 6)



# pr 4

dt_pr_rcm4g <- dt_pr_rcm4 %>% merge(dt_rcm_orog)

gm4 <- gam(pr ~ s(x, y, k = kk_basis^2) + s(orog, k = kk_basis),
           data = dt_pr_rcm4g, family = tw())
vals_out4 <- predict(gm4, dt_obs_orog_p, type = "response")
dt_obs4 <- dt_obs_orog_p %>% cbind(pr = vals_out4)

lims4 <- range(dt_pr_rcm4$pr, dt_obs4$pr, na.rm = T)
gg4_rcm <- dt_pr_rcm4 %>% 
  ggplot(aes(x, y, fill = pr))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims4)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("RCM", date4)
gg4_gam <- dt_obs4[!is.na(pr)] %>% 
  ggplot(aes(x, y, fill = pr))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims4)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("GAM", date4)

gg_smooth_1 <- plot(sm(getViz(gm4), 1))+l_fitRaster()
gg_smooth_2 <- plot(sm(getViz(gm4), 2))+l_ciLine()+l_fitLine()
gg_check <- check(getViz(gm4))

gg_out_gam4 <- (gg4_rcm + gg4_gam + 
                  gg_smooth_1$ggObj + gg_smooth_2$ggObj +
                  gg_check[[1]] + gg_check[[4]])+plot_layout(byrow = F)

ggsave("fig/example-downscaling/gam-4-pr.png", gg_out_gam4, width = 12, height = 6)




# pr 5

dt_pr_rcm5g <- dt_pr_rcm5 %>% merge(dt_rcm_orog)

gm5 <- gam(pr ~ s(x, y, k = kk_basis^2) + s(orog, k = kk_basis),
           data = dt_pr_rcm5g, family = tw())
vals_out5 <- predict(gm5, dt_obs_orog_p, type = "response")
dt_obs5 <- dt_obs_orog_p %>% cbind(pr = vals_out5)

lims5 <- range(dt_pr_rcm5$pr, dt_obs5$pr, na.rm = T)
gg5_rcm <- dt_pr_rcm5 %>% 
  ggplot(aes(x, y, fill = pr))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims5)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("RCM", date5)
gg5_gam <- dt_obs5[!is.na(pr)] %>% 
  ggplot(aes(x, y, fill = pr))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims5)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("GAM", date5)

gg_smooth_1 <- plot(sm(getViz(gm5), 1))+l_fitRaster()
gg_smooth_2 <- plot(sm(getViz(gm5), 2))+l_ciLine()+l_fitLine()
gg_check <- check(getViz(gm5))

gg_out_gam5 <- (gg5_rcm + gg5_gam + 
                  gg_smooth_1$ggObj + gg_smooth_2$ggObj +
                  gg_check[[1]] + gg_check[[4]])+plot_layout(byrow = F)

ggsave("fig/example-downscaling/gam-5-pr.png", gg_out_gam5, width = 12, height = 6)


# pr 6

dt_pr_rcm6g <- dt_pr_rcm6 %>% merge(dt_rcm_orog)

gm6 <- gam(pr ~ s(x, y, k = kk_basis^2) + s(orog, k = kk_basis),
           data = dt_pr_rcm6g, family = tw())
vals_out6 <- predict(gm6, dt_obs_orog_p, type = "response")
dt_obs6 <- dt_obs_orog_p %>% cbind(pr = vals_out6)

lims6 <- range(dt_pr_rcm6$pr, dt_obs6$pr, na.rm = T)
gg6_rcm <- dt_pr_rcm6 %>% 
  ggplot(aes(x, y, fill = pr))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims6)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("RCM", date6)
gg6_gam <- dt_obs6[!is.na(pr)] %>% 
  ggplot(aes(x, y, fill = pr))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A", limits = lims6)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("GAM", date6)

gg_smooth_1 <- plot(sm(getViz(gm6), 1))+l_fitRaster()
gg_smooth_2 <- plot(sm(getViz(gm6), 2))+l_ciLine()+l_fitLine()
gg_check <- check(getViz(gm6))

gg_out_gam6 <- (gg6_rcm + gg6_gam + 
                  gg_smooth_1$ggObj + gg_smooth_2$ggObj +
                  gg_check[[1]] + gg_check[[4]])+plot_layout(byrow = F)

ggsave("fig/example-downscaling/gam-6-pr.png", gg_out_gam6, width = 12, height = 6)



# comparison qdm gam ------------------------------------------------------

gg_diff1 <-
dt_obs1 %>% 
  merge(dt_tasmax_qdm1[, .(x,y,qdm = tasmax)]) %>% 
  ggplot(aes(x,y, fill = qdm - tasmax))+
  geom_raster()+
  scale_fill_scico("qdm - gam", palette = "vik")+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("QDM - GAM", date1)

lims1 <- range(dt_tasmax_rcm1$tasmax, dt_tasmax_qdm1$tasmax, dt_obs1$tasmax, na.rm = T)
gg_out <- ((gg1_rcm + gg1_qdm + gg1_gam)&
             scale_fill_viridis_c(direction = -1, option = "A", limits = lims1))+
  gg_diff1

ggsave("fig/example-downscaling/comp-1-tasmax.png", gg_out, width = 10, height = 6)



gg_diff2 <-
  dt_obs2 %>% 
  merge(dt_tasmax_qdm2[, .(x,y,qdm = tasmax)]) %>% 
  ggplot(aes(x,y, fill = qdm - tasmax))+
  geom_raster()+
  scale_fill_scico("qdm - gam", palette = "vik")+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("QDM - GAM", date2)

lims2 <- range(dt_tasmax_rcm2$tasmax, dt_tasmax_qdm2$tasmax, dt_obs2$tasmax, na.rm = T)
gg_out <- ((gg2_rcm + gg2_qdm + gg2_gam)&
             scale_fill_viridis_c(direction = -1, option = "A", limits = lims2))+
  gg_diff2

ggsave("fig/example-downscaling/comp-2-tasmax.png", gg_out, width = 10, height = 6)





gg_diff3 <-
  dt_obs3 %>% 
  merge(dt_pr_qdm3[, .(x,y,qdm = pr)]) %>% 
  ggplot(aes(x,y, fill = qdm - pr))+
  geom_raster()+
  scale_fill_scico("qdm - gam", palette = "vik", midpoint = 0)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("QDM - GAM", date3)

lims3 <- range(dt_pr_rcm3$pr, dt_pr_qdm3$pr, dt_obs3$pr, na.rm = T)
gg_out <- ((gg3_rcm + gg3_qdm + gg3_gam)&
             scale_fill_viridis_c(direction = -1, option = "A", limits = lims3))+
  gg_diff3

ggsave("fig/example-downscaling/comp-3-pr.png", gg_out, width = 10, height = 6)


gg_diff4 <-
  dt_obs4 %>% 
  merge(dt_pr_qdm4[, .(x,y,qdm = pr)]) %>% 
  ggplot(aes(x,y, fill = qdm - pr))+
  geom_raster()+
  scale_fill_scico("qdm - gam", palette = "vik", midpoint = 0)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("QDM - GAM", date4)

lims4 <- range(dt_pr_rcm4$pr, dt_pr_qdm4$pr, dt_obs4$pr, na.rm = T)
gg_out <- ((gg4_rcm + gg4_qdm + gg4_gam)&
             scale_fill_viridis_c(direction = -1, option = "A", limits = lims4))+
  gg_diff4

ggsave("fig/example-downscaling/comp-4-pr.png", gg_out, width = 10, height = 6)



gg_diff5 <-
  dt_obs5 %>% 
  merge(dt_pr_qdm5[, .(x,y,qdm = pr)]) %>% 
  ggplot(aes(x,y, fill = qdm - pr))+
  geom_raster()+
  scale_fill_scico("qdm - gam", palette = "vik", midpoint = 0)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("QDM - GAM", date5)

lims5 <- range(dt_pr_rcm5$pr, dt_pr_qdm5$pr, dt_obs5$pr, na.rm = T)
gg_out <- ((gg5_rcm + gg5_qdm + gg5_gam)&
             scale_fill_viridis_c(direction = -1, option = "A", limits = lims5))+
  gg_diff5

ggsave("fig/example-downscaling/comp-5-pr.png", gg_out, width = 10, height = 6)




gg_diff6 <-
  dt_obs6 %>% 
  merge(dt_pr_qdm6[, .(x,y,qdm = pr)]) %>% 
  ggplot(aes(x,y, fill = qdm - pr))+
  geom_raster()+
  scale_fill_scico("qdm - gam", palette = "vik", midpoint = 0)+
  xlim(lim_x)+ylim(lim_y)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)+
  ggtitle("QDM - GAM", date6)

lims6 <- range(dt_pr_rcm6$pr, dt_pr_qdm6$pr, dt_obs6$pr, na.rm = T)
gg_out <- ((gg6_rcm + gg6_qdm + gg6_gam)&
             scale_fill_viridis_c(direction = -1, option = "A", limits = lims6))+
  gg_diff6

ggsave("fig/example-downscaling/comp-6-pr.png", gg_out, width = 10, height = 6)

