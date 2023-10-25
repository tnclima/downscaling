# calculate local lapse rates for an example year of 1 RCM

library(terra)
library(magrittr)
library(lubridate)
library(stars)
library(ggplot2)
library(data.table)
setDTthreads(4)
library(foreach)

# spatial window width (square), length of one side (in cell units, here 0.11 deg)
# eg. 25 means 12 in each direction from center
# test: >? 9 
ww <- 11


# function to compute lapse rate
fun_lr <- function(x){
  rr_reg <- c(x, rr_orog/1000)
  rr_lapse <- focalReg(rr_reg, w = ww, intercept = T, na.rm = T)
  rr_lapse[is.na(x)] <- NA
  rr_lapse$orog
}



rr_orog <- rast("/home/climatedata/downscaling/rcm_lonlat_tnaa/orog/orog_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r0i0p0_CLMcom-CCLM4-8-17_v1_fx.nc")
dates <- seq(ymd("1950-01-01"), ymd("2005-12-31"), by = "day")
i_dates_1year <- which(year(dates) == "2001")

mitmatmisc::init_parallel_ubuntu(16)

# tmin --------------------------------------------------------------------

rr_rcm <- rast("/home/climatedata/downscaling/rcm_lonlat_tnaa/tasmin/tasmin_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_19500101-20051231.nc",
               lyrs = i_dates_1year)

rr_in <- lapply(rr_rcm, wrap, proxy = TRUE)

rr_out <- foreach(
  # i = 1:100
  i = seq_along(rr_in)
) %dopar% {
  rr_in[[i]] %>% 
    unwrap() %>% 
    fun_lr() %>% 
    wrap(proxy = TRUE)
}

rr_rcm_lr <- lapply(rr_out, unwrap) %>% rast
names(rr_rcm_lr) <- dates[i_dates_1year]

dat_lr <- as.data.table(rr_rcm_lr, xy = T) %>%
  cbind(icell = 1:ncell(rr_rcm_lr)) %>% 
  melt(id.vars = c("x", "y", "icell"), variable.name = "date")

dat_lr[value < -10, value := NA]
lr_lims <- range(dat_lr$value)

pdf(paste0("fig/lapse-rate/rcm-clm-01-tmin-maps-w", ww, ".pdf"), 
    width = 16, height = 8)
for(i in 1:12){
  
  gg <- dat_lr[month(date) == i] %>% 
    ggplot(aes(x, y, fill = value))+
    geom_raster()+
    scale_fill_viridis_c("lapse rate\n[degC/km]", limits = lr_lims)+
    facet_wrap(~date, nrow = 4)+
    coord_equal()+
    theme_minimal()
  print(gg)  
}
dev.off()


# tmax --------------------------------------------------------------------

rr_rcm <- rast("/home/climatedata/downscaling/rcm_lonlat_tnaa/tasmax/tasmax_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_19500101-20051231.nc",
               lyrs = i_dates_1year)

rr_in <- lapply(rr_rcm, wrap, proxy = TRUE)

rr_out <- foreach(
  # i = 1:100
  i = seq_along(rr_in)
) %dopar% {
  rr_in[[i]] %>% 
    unwrap() %>% 
    fun_lr() %>% 
    wrap(proxy = TRUE)
}

rr_rcm_lr <- lapply(rr_out, unwrap) %>% rast
names(rr_rcm_lr) <- dates[i_dates_1year]

dat_lr <- as.data.table(rr_rcm_lr, xy = T) %>%
  cbind(icell = 1:ncell(rr_rcm_lr)) %>% 
  melt(id.vars = c("x", "y", "icell"), variable.name = "date")

dat_lr[value < -10, value := NA]
lr_lims <- range(dat_lr$value)

pdf(paste0("fig/lapse-rate/rcm-clm-02-tmax-maps-w", ww, ".pdf"), 
    width = 16, height = 8)
for(i in 1:12){
  
  gg <- dat_lr[month(date) == i] %>% 
    ggplot(aes(x, y, fill = value))+
    geom_raster()+
    scale_fill_viridis_c("lapse rate\n[degC/km]", limits = lr_lims)+
    facet_wrap(~date, nrow = 4)+
    coord_equal()+
    theme_minimal()
  print(gg)  
}
dev.off()


# prec --------------------------------------------------------------------

rr_rcm <- rast("/home/climatedata/downscaling/rcm_lonlat_tnaa/pr/pr_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_19500101-20051231.nc",
               lyrs = i_dates_1year)

mitmatmisc::init_parallel_ubuntu(16)

rr_in <- lapply(rr_rcm, wrap, proxy = TRUE)

rr_out <- foreach(
  # i = 1:100
  i = seq_along(rr_in)
) %dopar% {
  rr_in[[i]] %>% 
    unwrap() %>% 
    fun_lr() %>% 
    wrap(proxy = TRUE)
}

rr_rcm_lr <- lapply(rr_out, unwrap) %>% rast
names(rr_rcm_lr) <- dates[i_dates_1year]

dat_lr <- as.data.table(rr_rcm_lr, xy = T) %>%
  cbind(icell = 1:ncell(rr_rcm_lr)) %>% 
  melt(id.vars = c("x", "y", "icell"), variable.name = "date")
dat_lr[, value := value*86400]

# dat_lr[value < -10, value := NA]
# lr_lims <- range(dat_lr$value)
# lr_lims <- quantile(dat_lr$value, c(0.05, 0.99))
lr_lims <- c(-10, 50)

pdf(paste0("fig/lapse-rate/rcm-clm-03-prec-maps-w", ww, ".pdf"), 
    width = 16, height = 8)
for(i in 1:12){
  
  gg <- dat_lr[month(date) == i] %>% 
    ggplot(aes(x, y, fill = value))+
    geom_raster()+
    scale_fill_viridis_c("lapse rate\n[mm/km]", 
                         limits = lr_lims)+
    facet_wrap(~date, nrow = 4)+
    coord_equal()+
    theme_minimal()
  print(gg)  
}
dev.off()
