# test non-contempouraneous merging 

library(ncdf4)
library(eurocordexr)
library(data.table)
setDTthreads(4)
library(magrittr)
library(ggplot2)
library(bestNormalize)

l_file_obs <- list(
  tasmax = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MaxTemp.nc",
  tasmin = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MinTemp.nc",
  pr = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_Prec.nc"
)
l_file_obs_orog <- list(
  tasmax = "/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc",
  tasmin = "/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc",
  pr = "/home/climatedata/obs/orography/crespi_lonlat_1km_precipitation.nc"
)

dat_aux <- nc_grid_to_dt(l_file_obs_orog$tasmax, add_xy = T)
dat_aux <- dat_aux[, .(icell, x = longitude, y = latitude, orog)]
# dat_aux %>% ggplot(aes(x,y,fill=orog))+geom_raster()

file_obs <- l_file_obs$pr
nc_in <- nc_open(file_obs)
# mat_in <- ncvar_get(nc_in, "precipitation")

dat_obs <- nc_grid_to_dt(l_file_obs$pr, date_range = c("1981-01-01", "1990-12-31"))
dat_obs <- nc_grid_to_dt(l_file_obs$pr)
dat_obs <- dat_obs[!is.na(precipitation)]
dat_obs[, month := month(date)]

dat_obs[date == "1982-04-14"] %>% 
  merge(dat_aux) %>% 
  ggplot(aes(x,y,fill = precipitation))+
  geom_raster()

dat_obs2 <- dat_obs[month %in% c(12, 1, 2)]
dat_obs2 <- dat_obs[month %in% c(6:8)]

mat_obs <- dat_obs2 %>% 
  dcast(icell ~ date, value.var = "precipitation") %>% 
  as.matrix

vec_icell <- mat_obs[, 1]
mat_obs <- mat_obs[, -1]

sub_dates <- sample(1:ncol(mat_obs), 100)

mat_obs_sub <- mat_obs[, sub_dates]
mat_obs_sub[mat_obs_sub < 0] <- 0

# remove all 0 columns
zero_col <- apply(mat_obs_sub, 2, \(x) all(x == 0))
mat_obs_sub <- mat_obs_sub[, !zero_col]


# normalize
mat_obs_sub_sc <- sqrt(mat_obs_sub)
# mat_obs_sub_sc <- scale(sqrt(mat_obs_sub), scale = F)

# yj_fit <- yeojohnson(mat_obs_sub)
# mat_obs_sub_sc <- predict(yj_fit)

# mat_obs_sub[which(is.na(mat_obs_sub_sqrt))]
hist(mat_obs_sub, 50)
hist(mat_obs_sub_sc, 50)



pca1 <- prcomp(mat_obs_sub_sc, center = F, scale = F, rank. = 20)
pca2 <- prcomp(mat_obs_sub_sc, center = T, scale = F, rank. = 20)
pca3 <- prcomp(mat_obs_sub_sc, center = T, scale = T, rank. = 20)

pca1 %>% summary
pca2 %>% summary
pca3 %>% summary

dat_pca <- data.table(icell = vec_icell, pca2$x)

dat_pca_plot <- dat_pca %>% 
  melt(id.vars = c("icell"), measure.vars = paste0("PC", 1:10), variable.factor = T) %>% 
  merge(dat_aux)

dat_pca_plot %>% 
  ggplot(aes(x, y, fill = value))+
  geom_raster()+
  facet_wrap(~variable)+
  scale_fill_gradient2()

