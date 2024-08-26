# visualize maps

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


# settings -------------------------------------------------------

i_var <- "tasmin"
# i_var <- "pr"

tasmin_v <- "_v3" # suffix version for check of tasmin (_v2, _v3, or empty string)

# pr_th <- 0.1 # threshold for zero precip (mm)

date_rcm_sub <- as.Date(c("2000-01-01", "2001-12-31"))
dates_loop <- seq(date_rcm_sub[1], date_rcm_sub[2], by = "day")


dat_inv <- get_inventory("/home/climatedata/downscaling/rcm_lonlat_tnaa_eraint/")
dat_inv[variable == "orog" & institute_rcm == "UHOH-WRF361H", 
        institute_rcm := "IPSL-WRF381P"] # since wrf381 has no fx info
dat_inv_loop <- dat_inv[variable == i_var]
all_rcms <- dat_inv_loop$institute_rcm

ds_all <- c(str_c("lm", 1:4), str_c("ked", 1:4))

l_file_obs <- list(
  tasmax = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MaxTemp.nc",
  tasmin = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MinTemp.nc",
  pr = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_Prec.nc"
)

dat_aux_1km <- nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc",
                             add_xy = T)
dat_aux_1km <- dat_aux_1km[, .(icell, orog, lon = longitude, lat = latitude)]

# fun data ----------------------------------------------------------------

f_plot <- function(i_date, i_rcm, i_var){
  
  dat_rcm <- nc_grid_to_dt(dat_inv_loop[institute_rcm == i_rcm, list_files[[1]]],
                           date_range = c(i_date, i_date), add_xy = T)
  setnames(dat_rcm, i_var, "value")
  if(i_var == "pr"){
    dat_rcm[, value := value*86400]
    dat_obs <- nc_grid_to_dt(l_file_obs[[i_var]], date_range = c(i_date, i_date)+1)
  } else {
    dat_rcm[, value := value-273.15]
    dat_obs <- nc_grid_to_dt(l_file_obs[[i_var]], date_range = c(i_date, i_date))
  }
  
  
  setnames(dat_obs, 3, "value")
  
  dat_ds <- lapply(ds_all, \(i_ds){
    
    file_ds <- str_c("/home/climatedata/downscaling/pca-ked/ds-test/", i_var, tasmin_v, "/",
                     i_ds, "_", i_rcm, ".nc")
    dat_i_ds <- nc_grid_to_dt(file_ds, date_range = c(i_date, i_date))
    dat_i_ds[, ds := i_ds]
    setnames(dat_i_ds, i_var, "value")
    dat_i_ds
  }) %>% rbindlist

  
  lim_cols <- range(dat_rcm$value, dat_obs$value, dat_ds$value, na.rm = T)
  lim_x <- range(dat_rcm$lon, dat_aux_1km$lon)
  lim_y <- range(dat_rcm$lat, dat_aux_1km$lat)
  
  gg_rcm <-
    dat_rcm %>% 
    ggplot(aes(lon, lat, fill = value))+
    geom_raster()+
    scale_fill_viridis_c(i_var, limits = lim_cols, direction = -1, option = "A")+
    xlim(lim_x)+ylim(lim_y)+
    coord_fixed()+
    theme_bw()+
    xlab(NULL)+ylab(NULL)+
    ggtitle(str_c("RCM: ", i_rcm))
  
  
  gg_obs <-
    dat_obs[!is.na(value)] %>% 
      merge(dat_aux_1km) %>% 
    ggplot(aes(lon, lat, fill = value))+
    geom_raster()+
    scale_fill_viridis_c(i_var, limits = lim_cols, direction = -1, option = "A")+
    xlim(lim_x)+ylim(lim_y)+
    coord_fixed()+
    theme_bw()+
    xlab(NULL)+ylab(NULL)+
    ggtitle("Crespi")
  
  gg_ds <- dat_ds[!is.na(value)] %>% 
    merge(dat_aux_1km) %>% 
    ggplot(aes(lon, lat, fill = value))+
    geom_raster()+
    scale_fill_viridis_c(i_var, limits = lim_cols, direction = -1, option = "A")+
    xlim(lim_x)+ylim(lim_y)+
    facet_wrap(~ds, nrow = 2)+
    coord_fixed()+
    theme_bw()+
    xlab(NULL)+ylab(NULL)+
    ggtitle(str_c("DS: ", i_date))
  
  
  gg_ds_diff <-
    dat_ds[!is.na(value)] %>% 
    merge(dat_obs[, .(icell, value_obs = value)]) %>% 
    merge(dat_aux_1km) %>% 
    ggplot(aes(lon, lat, fill = value - value_obs))+
    geom_raster()+
    # scale_fill_viridis_c(i_var, limits = lim_cols, direction = -1, option = "A")+
    scale_fill_scico(i_var, palette = "vik", midpoint = 0)+
    xlim(lim_x)+ylim(lim_y)+
    facet_wrap(~ds, nrow = 2)+
    coord_fixed()+
    theme_bw()+
    xlab(NULL)+ylab(NULL)+
    ggtitle("DS diff")
  
  (gg_rcm + gg_ds + gg_obs + gg_ds_diff)+
    plot_layout(nrow = 2, widths = c(1,2))
  
    
}

mitmatmisc::init_parallel_ubuntu(13)

foreach(
  i_rcm = all_rcms
) %dopar% {
  
  # test
  dates_loop_i <- dates_loop[rep(c(T,F), c(1,9))]
  # dates_loop_i <- dates_loop
  
  file_out <- str_c("fig/pca-ked/test-maps/", i_var, tasmin_v, "_", i_rcm, ".pdf")
  
  pdf(file_out, width = 12, height = 6)
  
  for(i in seq_along(dates_loop_i)){
    
    i_date <- dates_loop_i[i]
    gg_out <- f_plot(i_date, i_rcm, i_var)
    # ggsave("zz.png", gg_out, width = 12, height = 6)
    print(gg_out)
    
  }
  
  dev.off()
  
}


# i_date <- dates_loop[1]
# i_rcm <- all_rcms[1]

