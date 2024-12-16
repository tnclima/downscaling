#

library(data.table)
setDTthreads(4)
library(magrittr)
library(sf)
library(eurocordexr)
library(stars)
library(ggplot2)
library(patchwork)

# data mg -----------------------------------------------------------------

dat_mg <- fread("data-raw/CR2to2023_solrad.csv")

sf_mg <- st_as_sf(dat_mg[1, ], coords = c("X", "Y"), crs = 25832)
# mapview::mapview(sf_mg)
sf_mg_lonlat <- st_transform(sf_mg, 4326)
st_coordinates(sf_mg_lonlat)

dat_mg[, date := lubridate::mdy(datetime)]
dat_mg2 <- dat_mg[, .(date, tmin_mg = Tair_min, tmax_mg = Tair_max, 
                      tmean_mg = Tair_avg, prec_mg = Pfilled)]

# elev of crespi / eu-dem -------------------------------------------------

# rs_aux <- read_stars("/home/climatedata/climate_scenarios_appa/dati_topografia/orog_eudem_1km.nc")
rs_aux <- read_stars("/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc")
st_crs(rs_aux) <- 4326
rs_aux[sf_mg_lonlat]

# mg: 2642
# crespi: 2784
# eu-dem: 2787



# crespi data -------------------------------------------------------------

l_files <- list(
  tmin = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MinTemp.nc",
  tmax = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MaxTemp.nc",
  prec = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_Prec.nc"
)

l_data <- lapply(l_files, \(fn){
  
  rs <- read_stars(fn)
  st_crs(rs) <- 4326
  names(rs) <- "value"
  rs1 <- rs[sf_mg_lonlat] %>% st_as_stars
  
  data.table(
    value = as.vector(rs1$value),
    date = st_get_dimension_values(rs1, "time")
  )
  
})
names(l_data) <- names(l_files)

dat_crespi <- rbindlist(l_data, idcol = "variable")
dat_crespi[, date := as.Date(date)]
dat_crespi2 <- dat_crespi %>% dcast(date ~ variable)


# compare -----------------------------------------------------------------

dat_comp <- merge(dat_crespi2, dat_mg2)

gg1 <- dat_comp %>% 
  ggplot(aes(tmin, tmin_mg))+
  geom_bin2d()+
  geom_abline()+
  scale_fill_viridis_c()+
  theme_bw()

gg2 <- dat_comp %>% 
  ggplot(aes(tmax, tmax_mg))+
  geom_bin2d()+
  geom_abline()+
  scale_fill_viridis_c()+
  theme_bw()

gg3 <- dat_comp %>% 
  ggplot(aes(prec, prec_mg))+
  geom_bin2d()+
  geom_abline()+
  scale_fill_viridis_c(trans = "log10")+
  scale_x_sqrt()+
  scale_y_sqrt()+
  theme_bw()



dat_comp[, 
         .(
           bias_tmin = mean(tmin_mg - tmin),
           bias_tmax = mean(tmax_mg - tmax),
           bias_prec = mean(prec_mg - prec),
           
           mae_tmin = mean(abs(tmin_mg - tmin)),
           mae_tmax = mean(abs(tmax_mg - tmax)),
           mae_prec = mean(abs(prec_mg - prec)),
           
           corr_tmin = cor(tmin_mg, tmin),
           corr_tmax = cor(tmax_mg, tmax),
           corr_prec = cor(prec_mg, prec),
           
           wetday_mg = sum(prec_mg >= 1)/.N,
           wetday_crespi = sum(prec >= 1)/.N
           )]


gg_out <- (gg1+gg2+gg3)
ggsave("fig/use-cases/careser-mg/kriging-crespi.png", 
       gg_out, width = 14, height = 4)



dat_comp %>% 
  ggplot(aes(tmean_mg, (tmin_mg + tmax_mg)/2))+
  geom_bin2d()+
  geom_abline()+
  scale_fill_viridis_c(trans = "log10")+
  theme_bw()
  
with(dat_comp, hist(tmean_mg - (tmin_mg + tmax_mg)/2, 100))
with(dat_comp, summary(tmean_mg - (tmin_mg + tmax_mg)/2))
with(dat_comp, summary(tmin_mg - tmin))
with(dat_comp, summary(tmax_mg - tmax))
with(dat_comp, summary(prec_mg - prec))


  
  
  
  