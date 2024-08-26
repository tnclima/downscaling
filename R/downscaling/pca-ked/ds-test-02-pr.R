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

path_out <- "/home/climatedata/downscaling/pca-ked/ds-test/pr/"

date_rcm_sub <- as.Date(c("2000-01-01", "2001-12-31"))
dates_loop <- seq(date_rcm_sub[1], date_rcm_sub[2], by = "day")
n_nc_sync <- 200 # intermediate save to nc_out file every n dates

pr_th <- 0.1 # threshold for zero precip (mm)
p_occur <- 0.5 # probability threshold for prediction of precip occurence

file_obs_orog <- "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc"
# rs_template_tnaa <- rast("/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc")

ds_all <- c(str_c("lm", 1:4), str_c("ked", 1:4))

# inventory ---------------------------------------------------------------

dat_inv <- get_inventory("/home/climatedata/downscaling/rcm_lonlat_tnaa_eraint/")
dat_inv[variable == "orog" & institute_rcm == "UHOH-WRF361H", 
        institute_rcm := "IPSL-WRF381P"] # since wrf381 has no fx info

dat_inv_loop <- dat_inv[variable %in% c("pr")]


# data - for all ----------------------------------------------------------------

dat_pca <- readRDS("/home/climatedata/downscaling/pca-ked/crespi-pca/season-sub1000-pr-centerTRUE-scaleTRUE.rds")

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
  
  dat_rcm_orog <- nc_grid_to_dt(file_rcm_orog, add_xy = T)
  dat_rcm_orog[, date := NULL]
  
  dat_rcm <- nc_grid_to_dt(file_rcm, date_range = date_rcm_sub)
  setnames(dat_rcm, i_var, "value")
  
  if(i_var == "pr") dat_rcm[, value := value*86400]
  
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
    
    sf_rcm_i2_pca$value_wet <- sf_rcm_i2_pca$value > pr_th
    # sf_rcm_i2_pca$value_wet <- as.numeric(sf_rcm_i2_pca$value > pr_th)
    
    sf_newdata <- sf_pca %>% 
      dplyr::filter(season == i_season) %>% 
      dplyr::rename(orog = orog_1km)
    
    if(all(!sf_rcm_i2_pca$value_wet)){
      dat_all_pred <- data.table(icell = sf_newdata$icell_1km,
                                 lm1_o = 0, 
                                 lm2_o = 0, 
                                 lm3_o = 0, 
                                 lm4_o = 0, 
                                 lm1_i = 0, 
                                 lm2_i = 0,
                                 lm3_i = 0,
                                 lm4_i = 0,
                                 ked1_o = 0,
                                 ked2_o = 0,
                                 ked3_o = 0,
                                 ked4_o = 0,
                                 ked1_i = 0,
                                 ked2_i = 0,
                                 ked3_i = 0,
                                 ked4_i = 0)
    } else {
      
      if(all(sf_rcm_i2_pca$value_wet)){
        
        dat_all_pred_o <- data.table(icell = sf_newdata$icell_1km,
                                     lm1_o = 1, 
                                     lm2_o = 1, 
                                     lm3_o = 1, 
                                     lm4_o = 1, 
                                     ked1_o = 1,
                                     ked2_o = 1,
                                     ked3_o = 1,
                                     ked4_o = 1)
        
      } else {
        
        # occurence (only if partially wet)
        glm1 <- glm(value_wet ~ 1, data = sf_rcm_i2_pca, family = "binomial")
        glm2 <- glm(value_wet ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = sf_rcm_i2_pca, family = "binomial")
        glm3 <- glm(value_wet ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                      PC7 + PC8 + PC9 + PC10, data = sf_rcm_i2_pca, family = "binomial")
        glm4 <- glm(value_wet ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                      PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + 
                      PC14 + PC15 + PC16 + PC17 + PC18 + PC19, data = sf_rcm_i2_pca, family = "binomial")
        
        
        v1o_sample <- variogram(value_wet ~ 1, sf_rcm_i2_pca)
        v1o_fit <- fit.variogram(v1o_sample, vgm("Sph"))
        k1o_fit <- krige(value_wet ~ 1, sf_rcm_i2_pca, sf_newdata, v1o_fit,
                         debug.level = 0)
        
        v2o_sample <- variogram(value_wet ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, sf_rcm_i2_pca)
        v2o_fit <- fit.variogram(v2o_sample, vgm("Sph"))
        k2o_fit <- krige(value_wet ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, 
                         sf_rcm_i2_pca, sf_newdata, v2o_fit,
                         debug.level = 0)
        
        v3o_sample <- variogram(value_wet ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                                  PC7 + PC8 + PC9 + PC10, sf_rcm_i2_pca)
        v3o_fit <- fit.variogram(v3o_sample, vgm("Sph"))
        k3o_fit <- krige(value_wet ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                           PC7 + PC8 + PC9 + PC10, 
                         sf_rcm_i2_pca, sf_newdata, v3o_fit,
                         debug.level = 0)
        
        v4o_sample <- variogram(value_wet ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                                  PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + 
                                  PC14 + PC15 + PC16 + PC17 + PC18 + PC19, sf_rcm_i2_pca)
        v4o_fit <- fit.variogram(v4o_sample, vgm("Sph"))
        k4o_fit <- krige(value_wet ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                           PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + 
                           PC14 + PC15 + PC16 + PC17 + PC18 + PC19, 
                         sf_rcm_i2_pca, sf_newdata, v4o_fit,
                         debug.level = 0)
        
        dat_all_pred_o <- data.table(icell = sf_newdata$icell_1km,
                                     lm1_o = predict(glm1, sf_newdata, type = "response"), 
                                     lm2_o = predict(glm2, sf_newdata, type = "response"), 
                                     lm3_o = predict(glm3, sf_newdata, type = "response"), 
                                     lm4_o = predict(glm4, sf_newdata, type = "response"), 
                                     ked1_o = k1o_fit$var1.pred,
                                     ked2_o = k2o_fit$var1.pred,
                                     ked3_o = k3o_fit$var1.pred,
                                     ked4_o = k4o_fit$var1.pred)
        
      }
      
      
      
      # intensity
      lm1 <- lm(value ~ 1, data = sf_rcm_i2_pca)
      lm2 <- lm(value ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = sf_rcm_i2_pca)
      lm3 <- lm(value ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                  PC7 + PC8 + PC9 + PC10, data = sf_rcm_i2_pca)
      lm4 <- lm(value ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                  PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + 
                  PC14 + PC15 + PC16 + PC17 + PC18 + PC19, data = sf_rcm_i2_pca)
      
      # plot(v1_sample, v1_fit)
      # plot(v2_sample, v2_fit)
      # plot(v3_sample, v3_fit)
      
      v1i_sample <- variogram(value ~ 1, sf_rcm_i2_pca)
      v1i_fit <- fit.variogram(v1i_sample, vgm("Sph"))
      k1i_fit <- krige(value ~ 1, sf_rcm_i2_pca, sf_newdata, v1i_fit,
                       debug.level = 0)
      
      v2i_sample <- variogram(value ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, sf_rcm_i2_pca)
      v2i_fit <- fit.variogram(v2i_sample, vgm("Sph"))
      k2i_fit <- krige(value ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, 
                       sf_rcm_i2_pca, sf_newdata, v2i_fit,
                       debug.level = 0)
      
      v3i_sample <- variogram(value ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                                PC7 + PC8 + PC9 + PC10, sf_rcm_i2_pca)
      v3i_fit <- fit.variogram(v3i_sample, vgm("Sph"))
      k3i_fit <- krige(value ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                         PC7 + PC8 + PC9 + PC10, 
                       sf_rcm_i2_pca, sf_newdata, v3i_fit,
                       debug.level = 0)
      
      v4i_sample <- variogram(value ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                                PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + 
                                PC14 + PC15 + PC16 + PC17 + PC18 + PC19, sf_rcm_i2_pca)
      v4i_fit <- fit.variogram(v4i_sample, vgm("Sph"))
      k4i_fit <- krige(value ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                         PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + 
                         PC14 + PC15 + PC16 + PC17 + PC18 + PC19, 
                       sf_rcm_i2_pca, sf_newdata, v4i_fit,
                       debug.level = 0)
      
      
      dat_all_pred <- cbind(dat_all_pred_o,
                            lm1_i = predict(lm1, sf_newdata), 
                            lm2_i = predict(lm2, sf_newdata), 
                            lm3_i = predict(lm3, sf_newdata), 
                            lm4_i = predict(lm4, sf_newdata), 
                            ked1_i = k1i_fit$var1.pred,
                            ked2_i = k2i_fit$var1.pred,
                            ked3_i = k3i_fit$var1.pred,
                            ked4_i = k4i_fit$var1.pred)
      
    }
    
  
    
    dat_all_pred[, lm1 := (lm1_o > p_occur) * lm1_i]
    dat_all_pred[, lm2 := (lm2_o > p_occur) * lm2_i]
    dat_all_pred[, lm3 := (lm3_o > p_occur) * lm3_i]
    dat_all_pred[, lm4 := (lm4_o > p_occur) * lm4_i]
    dat_all_pred[, ked1 := (ked1_o > p_occur) * ked1_i]
    dat_all_pred[, ked2 := (ked2_o > p_occur) * ked2_i]
    dat_all_pred[, ked3 := (ked3_o > p_occur) * ked3_i]
    dat_all_pred[, ked4 := (ked4_o > p_occur) * ked4_i]
    
    
    
    # <0 - > zero
    dat_all_pred[, 
                 c(ds_all) := lapply(.SD, \(x) {x_out <- x; x_out[x < 0] <- 0; x_out} ), 
                 .SDcols = ds_all]
    
    dat_out <- merge(dat_obs_orog, dat_all_pred, all.x = T)
    setorder(dat_out, "icell_nc")
    
    walk(ds_all, \(x){
      vals_out <- dat_out[[x]]
      ncvar_put(l_nc_out[[x]], varid = i_var, vals = vals_out, 
                start = c(1, 1, i_date), count = c(-1, -1, 1))
      if(i_nc_sync %% n_nc_sync == 0) nc_sync(l_nc_out[[x]])
    })
    
    
  }
  
  walk(ds_all, \(x) nc_close(l_nc_out[[x]]))
  
}




