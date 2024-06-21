# test pca-ked on reanalysis: 1 time-step

library(sf)
library(stars)
library(gstat)
library(eurocordexr)
library(data.table)
setDTthreads(4)
library(magrittr)
library(ggplot2)
library(patchwork)
library(lubridate)



# 1km aux and pca -------------------------------------------------------------

dat_aux_1km <- nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc", add_xy = T)
dat_aux_1km <- dat_aux_1km[, .(icell, x = longitude, y = latitude, orog)]
# dat_aux_1km %>% ggplot(aes(x,y,fill=orog))+geom_raster()

rs_aux_1km <- read_stars("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc")
names(rs_aux_1km) <- "orog"

dat_pca <- readRDS("/home/climatedata/downscaling/pca-ked/crespi-pca/season-sub1000-tasmin-centerTRUE-scaleTRUE.rds")
# dat_pca <- readRDS("/home/climatedata/downscaling/pca-ked/crespi-pca/season-sub1000-tasmin-centerTRUE-scaleFALSE.rds")

# dat_pca[season == "DJF"] %>% 
#   merge(dat_aux_1km) %>% 
#   # ggplot(aes(x,y,fill=PC1))+geom_raster()
#   ggplot(aes(y,PC1))+geom_point()

dat_obs <- nc_grid_to_dt("/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MinTemp.nc", 
                         date_range = c("2000-01-01", "2000-12-31"))
dat_obs <- dat_obs[!is.na(temperature), .(icell, date, tasmin_crespi = temperature)]

# model -------------------------------------------------------------------

fn_rcm <- "/home/climatedata/downscaling/rcm_lonlat_tnaa_eraint/pr/pr_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_CLMcom-ETH-COSMO-crCLIM-v1-1_v1_day_19790101-20101231.nc"
fn_rcm <- "/home/climatedata/downscaling/rcm_lonlat_tnaa_eraint/tasmin/tasmin_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_CLMcom-ETH-COSMO-crCLIM-v1-1_v1_day_19790101-20101231.nc"

dat_aux_rcm <- nc_grid_to_dt("/home/climatedata/downscaling/rcm_lonlat_tnaa_eraint/orog/orog_EUR-11_ECMWF-ERAINT_evaluation_r0i0p0_CLMcom-ETH-COSMO-crCLIM-v1-1_v1_fx.nc",
                             add_xy = T)
dat_aux_rcm[, date := NULL]

dat_rcm <- nc_grid_to_dt(fn_rcm, date_range = c("2000-01-01", "2000-12-31"))
# dat_rcm[, pr := pr*86400]
dat_rcm[, tasmin := tasmin - 273.15]

# choose dates ------------------------------------------------------------


# check cross cor

dat_day <- dat_obs[, .(tasmin_crespi = mean(tasmin_crespi)), .(date)] %>% 
  merge(dat_rcm[, .(tasmin = mean(tasmin)), .(date)] )

dat_day %>% 
  ggplot(aes(tasmin_crespi, tasmin))+
  geom_point()

dat_day %>% 
  with(ccf(tasmin_crespi, tasmin))

dat_day[, .(tasmin, shift(tasmin_crespi))] %>% pairs
dat_day[, .(tasmin, tasmin_lag = shift(tasmin), tasmin_crespi, tasmin_crespi_lag = shift(tasmin_crespi))] %>% pairs
dat_day[, .(tasmin, tasmin_lag = shift(tasmin), tasmin_crespi, tasmin_crespi_lag = shift(tasmin_crespi))] %>% cor(use = "p")
dat_day[, .(tasmin, tasmin_lag = shift(tasmin), tasmin_crespi, tasmin_crespi_lag = shift(tasmin_crespi))] 

# -> no lag in temp



# 1 date ------------------------------------------------------------------


# dates (crespi) (from precip)
i_date <- "2000-03-27" # W, full region
i_date <- "2000-02-19" # N
i_date <- "2000-01-23" # EW, not central
i_date <- "2000-07-11" # all
i_date <- "2000-10-12" # SW
i_date <- "2000-11-07" # S


sf_rcm <- dat_rcm[date == ymd(i_date)] %>% 
  merge(dat_aux_rcm) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)


gg1 <- dat_obs[date == ymd(i_date)] %>% 
  merge(dat_aux_1km) %>% 
  ggplot(aes(x,y, fill = tasmin_crespi))+
  geom_raster()+
  scale_fill_viridis_c()+
  facet_grid(. ~ date)+
  coord_fixed()

gg2 <- dat_rcm[date == ymd(i_date)] %>% 
  merge(dat_aux_rcm) %>% 
  ggplot(aes(lon, lat, fill = tasmin))+
  geom_raster()+
  scale_fill_viridis_c()+
  facet_grid(. ~ date)+
  coord_fixed()

gg1|gg2

# universal kriging (elev only) --------------------------------------------------------------


v_sample <- variogram(tasmin ~ orog, sf_rcm)
plot(v_sample)

# show.vgms()

# v_fit <- fit.variogram(v_sample, vgm(1, "Exp", 50000, 1))
# v_fit <- fit.variogram(v_sample, vgm("Exp"))
v_fit <- fit.variogram(v_sample, vgm("Sph"))
plot(v_sample, v_fit)

# sf_new <- dat_aux_1km %>% 
#   st_as_sf(coords = c("x", "y"), crs = 4326)
# 
# k_ord <- krige(pr ~ 1, sf_rcm, sf_new, v_fit)


k_fit <- krige(tasmin ~ orog, sf_rcm, rs_aux_1km, v_fit)

gg3 <- ggplot()+
  geom_stars(data = k_fit)+
  scale_fill_viridis_c()+
  coord_sf()

gg1+gg2+gg3




# universal kriging (pcas) --------------------------------------------------------------

sf_new_pca <- dat_pca[season == mitmatmisc::season_fct(month(ymd(i_date)))] %>% 
  merge(dat_aux_1km) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)

setnames(sf_new_pca, "orog", "orog_1km")
setnames(sf_new_pca, "icell", "icell_1km")

sf_tnaa <- readRDS("data/sf-tnaa/tnaa-latlon.rds")
sf_rcm2 <- st_join(sf_rcm, st_as_sf(sf_tnaa), left = F)

sf_rcm_pca <- st_join(sf_rcm2, sf_new_pca, join = st_nearest_feature)

sf_rcm_pca <- sf_rcm_pca %>% dplyr::mutate(orog_diff = orog_1km - orog)
with(sf_rcm_pca, plot(orog, orog_1km))
with(sf_rcm_pca, plot(orog, orog_diff))

lm(tasmin ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + orog, data = sf_rcm_pca) %>% summary
lm(tasmin ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + orog, data = sf_rcm_pca) %>% summary
lm1 <- lm(tasmin ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + orog, data = sf_rcm_pca)
lm2 <- lm(tasmin ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + orog, data = sf_rcm_pca)
lm3 <- lm(tasmin ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
            PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + orog, data = sf_rcm_pca)

setnames(sf_new_pca, "orog_1km", "orog")

sf_new_pca1 <- cbind(sf_new_pca, lmfit = predict(lm1, sf_new_pca))
sf_new_pca2 <- cbind(sf_new_pca, lmfit = predict(lm2, sf_new_pca))
sf_new_pca3 <- cbind(sf_new_pca, lmfit = predict(lm3, sf_new_pca))


v_sample <- variogram(tasmin ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + orog, sf_rcm_pca)
plot(v_sample)

# show.vgms()

# v_fit <- fit.variogram(v_sample, vgm(1, "Exp", 50000, 1))
# v_fit <- fit.variogram(v_sample, vgm("Exp"))
# v_fit <- fit.variogram(v_sample, vgm("Lin"))
v_fit <- fit.variogram(v_sample, vgm("Sph"))
# v_fit <- fit.variogram(v_sample, vgm("Gau"))
plot(v_sample, v_fit)

# sf_new <- dat_aux_1km %>% 
#   st_as_sf(coords = c("x", "y"), crs = 4326)
# 
# k_ord <- krige(pr ~ 1, sf_rcm, sf_new, v_fit)


k_fit2 <- krige(tasmin ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + orog, sf_rcm_pca, sf_new_pca, v_fit)

gg4 <- ggplot()+
  geom_sf(data = k_fit2, aes(colour = var1.pred))+
  scale_colour_viridis_c()+
  coord_sf()

gg5 <- ggplot()+
  geom_sf(data = sf_new_pca1, aes(colour = lmfit))+
  scale_colour_viridis_c()+
  coord_sf()

gg6 <- ggplot()+
  geom_sf(data = sf_new_pca2, aes(colour = lmfit))+
  scale_colour_viridis_c()+
  coord_sf()

gg7 <- ggplot()+
  geom_sf(data = sf_new_pca3, aes(colour = lmfit))+
  scale_colour_viridis_c()+
  coord_sf()


gg1+gg2+gg4+gg5+gg6+gg7
