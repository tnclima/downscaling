# evaluate different options (pc number, ked yn)


# library(terra)
library(magrittr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(fs)
library(foreach)
library(stringr)
library(gstat)
library(sf)
library(eurocordexr)
library(purrr)

source("R/functions/create_empty_netcdf.R")
# source("R/functions/get_rcm_values2.R")
# source("R/functions/inv_sub.R")



# settings - variables ----------------------------------------------------

path_out <- "/home/climatedata/downscaling/pca-ked/ds-test/tasmin/"

date_rcm_sub <- as.Date(c("2000-01-01", "2001-12-31"))
dates_loop <- seq(date_rcm_sub[1], date_rcm_sub[2], by = "day")
n_nc_sync <- 200 # intermediate save to nc_out file every n dates

file_obs_orog <- "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc"
# rs_template_tnaa <- rast("/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc")

ds_all <- c(str_c("lm", 1:4), str_c("ked", 1:4))

# inventory ---------------------------------------------------------------

dat_inv <- get_inventory("/home/climatedata/downscaling/rcm_lonlat_tnaa_eraint/")
dat_inv[variable == "orog" & institute_rcm == "UHOH-WRF361H", 
        institute_rcm := "IPSL-WRF381P"] # since wrf381 has no fx info

dat_inv_loop <- dat_inv[variable %in% c("tasmin")]


# data - for all ----------------------------------------------------------------

dat_pca <- readRDS("/home/climatedata/downscaling/pca-ked/crespi-pca/season-sub1000-tasmin-centerTRUE-scaleTRUE.rds")

dat_obs_orog <- nc_grid_to_dt(file_obs_orog, add_xy = T)
dat_obs_orog[, date := NULL]
setnames(dat_obs_orog, c("icell", "orog", "lon", "lat"))
dat_obs_orog[, icell_nc := forcats::fct_inorder(factor(icell))]


sf_pca <- dat_pca %>% 
  merge(dat_obs_orog) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

setnames(sf_pca, "orog", "orog_1km")
setnames(sf_pca, "icell", "icell_1km")

sf_tnaa <- readRDS("data/sf-tnaa/tnaa-latlon.rds") %>% st_as_sf()

# main loop ---------------------------------------------------------------

mitmatmisc::init_parallel_ubuntu(5)

zz <- foreach(
  i_inv = 1:nrow(dat_inv_loop),
  .inorder = F
) %dopar% {
  
  i_rcm_name <- dat_inv_loop[i_inv, institute_rcm]
  i_var <- dat_inv_loop[i_inv, variable]
  
  file_rcm_orog <- dat_inv[variable == "orog" & institute_rcm == i_rcm_name, list_files[[1]]]
  file_rcm <- dat_inv_loop[i_inv, list_files[[1]]]
  
  files_out <- path(path_out,
                    dat_inv_loop[i_inv, 
                                 paste(ds_all, institute_rcm, sep = "_")],
                    ext = "nc")
  names(files_out) <- ds_all
  
  if(all(file_exists(files_out)))  return(NULL)
  
  # data --------------------------------------------------------------------
  
  # rs_rcm_orog <- rast(file_rcm_orog)
  # rs_obs_orog <- rast(file_obs_orog)
  # 
  # rs_orog_rcm_obs <- project(rs_rcm_orog, rs_obs_orog, method = "near")
  # rs_orog_diff_rcm_obs <- rs_orog_rcm_obs - rs_obs_orog
  # 
  # cells_obs <- which(!is.na(rs_template_tnaa[]))
  # cells_obs_na <- which(is.na(rs_template_tnaa[]))
  # 
  # rs_rcm <- rast(file_rcm)
  
  dat_rcm_orog <- nc_grid_to_dt(file_rcm_orog, add_xy = T)
  dat_rcm_orog[, date := NULL]
  
  dat_rcm <- nc_grid_to_dt(file_rcm, date_range = date_rcm_sub)
  setnames(dat_rcm, i_var, "value")
  
  # outfile ---------------------------------------------------------
  
  dir_create(path_out)
  walk(ds_all, \(x){
    create_emtpy_netcdf(file_template = file_obs_orog,
                        file_out = files_out[x],
                        l_varinfo = l_nc_info[[i_var]],
                        date_period = date_rcm_sub,
                        overwrite = F)
  })
  
  # main proc ---------------------------------------------------------------
  
  l_nc_out <- map(files_out, \(x) nc_open(x, write = T))
  i_nc_sync <- 0
  
  for(i_date in seq_along(dates_loop)){
    i_nc_sync <- i_nc_sync + 1
    i_season <- mitmatmisc::season_fct(month(dates_loop[i_date]))
    
    
    sf_rcm_i <- dat_rcm[date == dates_loop[i_date]] %>% 
      merge(dat_rcm_orog) %>% 
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    sf_rcm_i2 <- st_join(sf_rcm_i, sf_tnaa, left = F)
    
    sf_rcm_i2_pca <- st_join(sf_rcm_i2, 
                             sf_pca %>% dplyr::filter(season == i_season), 
                             join = st_nearest_feature)
    
    sf_newdata <- sf_pca %>% 
      dplyr::filter(season == i_season) %>% 
      dplyr::rename(orog = orog_1km)
    
    lm1 <- lm(value ~ orog, data = sf_rcm_i2_pca)
    lm2 <- lm(value ~ orog + PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = sf_rcm_i2_pca)
    lm3 <- lm(value ~ orog + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                PC7 + PC8 + PC9 + PC10, data = sf_rcm_i2_pca)
    lm4 <- lm(value ~ orog + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + 
                PC14 + PC15 + PC16 + PC17 + PC18 + PC19, data = sf_rcm_i2_pca)
    
    # plot(v1_sample, v1_fit)
    # plot(v2_sample, v2_fit)
    # plot(v3_sample, v3_fit)
    
    v1_sample <- variogram(value ~ orog, sf_rcm_i2_pca)
    v1_fit <- fit.variogram(v1_sample, vgm("Sph"))
    k1_fit <- krige(value ~ orog, sf_rcm_i2_pca, sf_newdata, v1_fit)
    
    v2_sample <- variogram(value ~ orog + PC1 + PC2 + PC3 + PC4 + PC5 + PC6, sf_rcm_i2_pca)
    v2_fit <- fit.variogram(v2_sample, vgm("Sph"))
    k2_fit <- krige(value ~ orog + PC1 + PC2 + PC3 + PC4 + PC5 + PC6, 
                    sf_rcm_i2_pca, sf_newdata, v2_fit)
    
    v3_sample <- variogram(value ~ orog + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                             PC7 + PC8 + PC9 + PC10, sf_rcm_i2_pca)
    v3_fit <- fit.variogram(v3_sample, vgm("Sph"))
    k3_fit <- krige(value ~ orog + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                      PC7 + PC8 + PC9 + PC10, 
                    sf_rcm_i2_pca, sf_newdata, v3_fit)
    
    v4_sample <- variogram(value ~ orog + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                             PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + 
                             PC14 + PC15 + PC16 + PC17 + PC18 + PC19, sf_rcm_i2_pca)
    v4_fit <- fit.variogram(v4_sample, vgm("Sph"))
    k4_fit <- krige(value ~ orog + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                      PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + 
                      PC14 + PC15 + PC16 + PC17 + PC18 + PC19, 
                    sf_rcm_i2_pca, sf_newdata, v4_fit)
    
    
    dat_all_pred <- data.table(icell = sf_newdata$icell_1km,
               lm1 = predict(lm1, sf_newdata), 
               lm2 = predict(lm2, sf_newdata), 
               lm3 = predict(lm3, sf_newdata), 
               lm4 = predict(lm4, sf_newdata), 
               ked1 = k1_fit$var1.pred,
               ked2 = k2_fit$var1.pred,
               ked3 = k3_fit$var1.pred,
               ked4 = k4_fit$var1.pred)
    
    
    dat_out <- merge(dat_obs_orog, dat_all_pred, all.x = T)
    setorder(dat_out, "icell_nc")
    
    walk(ds_all, \(x){
      ncvar_put(l_nc_out[[x]], varid = i_var, vals = dat_out[[x]] - 273.15, 
                start = c(1, 1, i_date), count = c(-1, -1, 1))
      if(i_nc_sync %% n_nc_sync == 0) nc_sync(l_nc_out[[x]])
    })
    
    
  }
  
  walk(ds_all, \(x) nc_close(l_nc_out[[x]]))
  
}




