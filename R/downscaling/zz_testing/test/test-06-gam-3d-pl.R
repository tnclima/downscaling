# test 3d-interpolation on pressure levels gam (toposcale)


library(data.table)
setDTthreads(4)
library(eurocordexr)
library(magrittr)
library(ggplot2)
library(patchwork)
library(mgcv)
library(stringr)
library(foreach)
library(terra)


pl2alt <- function(pl){
  # pl in hPa or mb
  (1 - (pl/1013.25)^0.190284) * 145366.45 * 0.3048
}

kk_basis <- 5

# data pl --------------------------------------------------------------------

# only 3 temp levels
# fn <- "/home/climatedata/downscaling/rcm_lonlat_tnaa_topo/testdata/ALL_EUR-11_NCC-NorESM1-M_historical_r1i1p1_GERICS-REMO2015_v1_day_19990101-19991231.nc"

# 8 temp levels (and more)
fn <- "/home/climatedata/downscaling/rcm_lonlat_tnaa_topo/testdata/ALL_EUR-11_NCC-NorESM1-M_historical_r1i1p1_ICTP-RegCM4-6_v1_day_19990101-19991231.nc"

vars_ta <- get_varnames(fn) %>% str_subset("ta")
dat_ta <- foreach(
  i_var = vars_ta,
  .final = rbindlist
) %do% {
  dat <- nc_grid_to_dt(fn, i_var)
  setnames(dat, i_var, "value")
  cbind(dat, vv = i_var)
}

dat_aux <- nc_grid_to_dt("/home/climatedata/downscaling/rcm_lonlat_tnaa/orog/orog_EUR-11_CNRM-CERFACS-CNRM-CM5_rcp85_r1i1p1_ICTP-RegCM4-6_v2_fx.nc",
                         add_xy = T)
dat_aux[, date := NULL]

dat_ta[, pl := as.numeric(str_sub(vv, 3))]

dat_ta2 <- merge(dat_ta, dat_aux)
dat_ta2[, pl_alt := pl2alt(pl)]



# data surface ------------------------------------------------------------

dat_tas <- nc_grid_to_dt("/home/climatedata/downscaling/rcm_lonlat_tnaa/tas/tas_EUR-11_NCC-NorESM1-M_rcp85_r1i1p1_ICTP-RegCM4-6_v1_day_19700101-21001230.nc",
                         date_range = c("1999-01-01", "1999-12-31"))
dat_tas <- merge(dat_tas, dat_aux)



# data pred ---------------------------------------------------------------

file_obs_orog <- "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc"
rs_obs_orog <- rast(file_obs_orog)
dt_obs_orog <- as.data.table(rs_obs_orog, xy = T, na.rm = F)

dt_pred_pl <- copy(dt_obs_orog)
setnames(dt_pred_pl, c("lon", "lat", "pl_alt"))

dt_pred_s <- copy(dt_obs_orog)
setnames(dt_pred_s, c("x", "y"), c("lon", "lat"))

# test plots ----------------------------------------------------------------

dat_1day <- dat_ta2[date == "1999-01-01"]
dat_1day[, value_anom := value - mean(value), pl]


dat_1day %>% 
  ggplot(aes(lon, lat, fill = value))+
  geom_raster()+
  scale_fill_viridis_c()+
  facet_wrap(~ pl)

dat_1day %>% 
  ggplot(aes(lon, lat, fill = value_anom))+
  geom_raster()+
  scale_fill_gradient2()+
  facet_wrap(~ pl)



# test gam ----------------------------------------------------------------

i_date <- "1999-01-01"
i_date <- "1999-04-01"
i_date <- "1999-07-01"

dat_pl1 <- dat_ta2[date == i_date]
dat_pl2 <- dat_pl1[pl_alt < 6000]
dat_tas1 <- dat_tas[date == i_date]

gm_pl <- gam(value ~ te(lon, lat, pl_alt, k = kk_basis),
             data = dat_pl2)
summary(gm_pl)
plot(gm_pl)
gam.check(gm_pl)

dt_gam_pl <- cbind(dt_pred_pl, value = predict(gm_pl, dt_pred_pl))


gm_s <- gam(tas ~ s(lon, lat, k = kk_basis^2) + 
              s(orog, k = kk_basis),
            data = dat_tas1)
summary(gm_s)
plot(gm_s)
gam.check(gm_s)

dt_gam_s <- cbind(dt_pred_s, value = predict(gm_s, dt_pred_s))


# plot

lims <- range(dat_tas1$tas, dt_gam_pl$value, dt_gam_s$value) - 273.15
# lims <- range(vals_rcm, vals_out, vals_out2)
lim_x <- range(dat_tas1$lon, dt_gam_pl$lon)
lim_y <- range(dat_tas1$lat, dt_gam_pl$lat)

gg0 <- dat_tas1 %>% 
  ggplot(aes(lon, lat, fill = tas - 273.15))+
  geom_raster()+
  scale_fill_viridis_c()+ 
  xlim(lim_x)+ylim(lim_y)

gg1 <- dat_tas1 %>% 
  ggplot(aes(lon, lat, fill = tas - 273.15))+
  geom_raster()+
  scale_fill_viridis_c(limits = lims)+ 
  xlim(lim_x)+ylim(lim_y)

gg2 <- dt_gam_pl %>% 
  ggplot(aes(lon, lat, fill = value - 273.15))+
  geom_raster()+
  scale_fill_viridis_c(limits = lims)+ 
  xlim(lim_x)+ylim(lim_y)

gg3 <- dt_gam_s %>% 
  ggplot(aes(lon, lat, fill = value - 273.15))+
  geom_raster()+
  scale_fill_viridis_c(limits = lims)+ 
  xlim(lim_x)+ylim(lim_y)


gg0 + gg1 + gg2 + gg3


# not really useful, since too little pressure levels to do interpolation




# test single points ------------------------------------------------------

icell_sample <- sample(unique(dat_pl2$icell), 9)

dat_pl2[icell %in% icell_sample] %>% 
  ggplot(aes(pl_alt, value))+
  geom_point()+
  geom_line()+
  facet_wrap(~icell)+
  theme_bw()


dat_lr_pl <- dat_pl2[,
                     .(lr = coef(lm(value ~ pl_alt))[2]),
                     .(icell, lat, lon, orog)]

dat_lr_pl$lr %>% summary
dat_lr_pl %>% 
  ggplot(aes(lon, lat, fill = lr))+
  geom_raster()+
  scale_fill_viridis_c()

dat_lr_pl %>% 
  ggplot(aes(orog, lr))+
  geom_point()
