# 

library(terra)
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

source("R/functions/create_empty_netcdf.R")



# settings - variables ----------------------------------------------------

path_out <- "/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v4/"


date_sub <- as.Date(c("1989-01-02", "2008-12-31"))
dates_loop <- seq(date_sub[1], date_sub[2], by = "day")
n_nc_sync <- 200 # intermediate save to nc_out file every n dates
n_cores <- 3 # parallel computation; (reads RCM memory, no need for crespi)

pr_th <- 0.1 # threshold for zero precip (mm)
pr_p_occur <- 0.5 # probability threshold for prediction of precip occurence
pr_sqrt <- TRUE # square-root transform precip (for better normality)

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


sf_tnaa <- readRDS("data/sf-tnaa/tnaa-latlon.rds") %>% st_as_sf()

match_temperature_gridcells <- F # if T, choose high-res gridcell nearest in
# elevation and not horizontal distance (only 
# relevant if not upscale_pcs)
dat_gridcells_temperature <- readRDS("data/ked-gridcells-temperature-obs-011.rds") %>% as.data.table()


upscale_pcs <- T # if T, high-res PCs will be upscaled and then merged to RCM
# otherwise, nearest PC to gridcell center will be matched to RCM

# n_pc <- 6 # number of PCs to use
# tas_orog4pc1 <- T # if T, use orog instead of PC1 for temperature variables

# obs 0.11 

l_file_obs_011 <- list(
  tasmax = "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/tasmax_crespi.nc",
  tasmin = "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/tasmin_crespi.nc",
  pr = "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/pr_crespi.nc"
)

file_obs_011_orog <- "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_crespi_temperature.nc"

rs_template_011 <- rast(file_obs_011_orog)
rs_template_1km <- rast(l_file_obs_orog$tasmax)

# other 
mitmatmisc::init_parallel_ubuntu(n_cores)

files_obs_011 <- unlist(l_file_obs_011)



dat_loop <- expand.grid(n_pc = 2:9, tas_orog4pc1 = c(T,F))


# main loop ---------------------------------------------------------------


foreach(
  n_pc = dat_loop$n_pc,
  tas_orog4pc1 = dat_loop$tas_orog4pc1
) %do% {
  
  path_out_subdir <- str_c("obs-ds-pcalm-nPC", n_pc)
  if(tas_orog4pc1){
    path_out_subdir <- str_c(path_out_subdir, "-orog4PC1")
  }
  
  zz <- foreach(
    i_file = files_obs_011,
    .inorder = F,
    .final = \(x) rbindlist(x, fill = T)
  ) %dopar% {
    
    i_file_split <- i_file %>% 
      path_file() %>% 
      str_split_1("_")
    
    i_var <- i_file_split[1]
    
    file_obs_orog <- l_file_obs_orog[[i_var]]
    
    file_out <- path(path_out,
                     path_out_subdir,
                     path_file(i_file))
    
    if(file_exists(file_out)) return(NULL)
    
    
    # data --------------------------------------------------------------------
    
    dat_obs_011_orog <- nc_grid_to_dt(file_obs_011_orog, add_xy = T)
    dat_obs_011_orog[, date := NULL]
    
    dat_obs_011 <- nc_grid_to_dt(i_file, date_range = date_sub)
    setnames(dat_obs_011, 3, "value")
    
    # if(i_var == "pr"){
    #   dat_obs_011[, value := value * 86400]
    # } else {
    #   dat_obs_011[, value := value - 273.15]
    # }
    
    dat_obs_orog <- nc_grid_to_dt(file_obs_orog, add_xy = T)
    dat_obs_orog[, date := NULL]
    setnames(dat_obs_orog, c("icell", "orog", "lon", "lat"))
    dat_obs_orog[, icell_nc := forcats::fct_inorder(factor(icell))]
    
    
    # pca data
    dat_pca <- readRDS(str_c("/home/climatedata/downscaling/pca-ked/crespi-pca/season-sub1000-",
                             i_var, "-centerTRUE-scaleTRUE.rds"))
    sf_pca <- dat_pca %>%
      merge(dat_obs_orog[!is.na(orog)]) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    setnames(sf_pca, "orog", "orog_1km")
    setnames(sf_pca, "icell", "icell_1km")
    
    # outfile ---------------------------------------------------------
    
    dir_create(path_dir(file_out))
    create_emtpy_netcdf(file_template = file_obs_orog,
                        file_out = file_out,
                        l_varinfo = l_nc_info[[i_var]],
                        date_period = date_sub,
                        overwrite = F)
    
    # main proc ---------------------------------------------------------------
    
    nc_out <- nc_open(file_out, write = T)
    i_nc_sync <- 0
    
    dat_coef_out <- foreach(
      i_date = seq_along(dates_loop),
      .final = \(x) rbindlist(x, fill = T)
    ) %do% {
      
      # i_rcm <- mapped_times[dates_loop[i_date] == dates_full, idx_pcict] # for non-standard cal
      
      i_season <- mitmatmisc::season_fct(month(dates_loop[i_date]))
      
      sf_newdata <- sf_pca %>% 
        dplyr::filter(season == i_season) %>% 
        dplyr::rename(orog = orog_1km)
      
      sf_obs_011 <- dat_obs_011[date == dates_loop[i_date]] %>% 
        merge(dat_obs_011_orog) %>% 
        st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
        st_join(sf_tnaa, left = F)
      
      if(upscale_pcs){
        
        rs_pca_1km <- rast(rs_template_1km, nlyrs = n_pc)
        rs_pca_1km[
          dat_pca[season == i_season, icell]
        ] <- dat_pca[season == i_season, str_c("PC", 1:n_pc), with=F]
        
        rs_pca_1km %>% flip %>% 
          aggregate(fact = 10) %>%
          resample(rs_template_011) -> rs_pca_011
        
        dat_pc_upscaled <- rs_pca_011 %>% as.data.table(cells = T, na.rm = F)
        setnames(dat_pc_upscaled, c("icell", str_c("PC", 1:n_pc)))
      }
      
      if(i_var == "pr"){
        
        if(upscale_pcs){
          sf_obs_011_pca <- merge(sf_obs_011, dat_pc_upscaled)
        } else {
          sf_obs_011_pca <- st_join(sf_obs_011, 
                                    sf_pca %>% dplyr::filter(season == i_season), 
                                    join = st_nearest_feature)
          
        }
        
        sf_obs_011_pca$value_wet <- sf_obs_011_pca$value > pr_th
        
        if(pr_sqrt) sf_obs_011_pca <- dplyr::mutate(sf_obs_011_pca, value = sqrt(value))
        
        # full dry
        if(all(!sf_obs_011_pca$value_wet, na.rm = T)){
          dat_pred <- data.table(icell = sf_newdata$icell_1km,
                                 occurrence = 0,
                                 intensity = 0)
          
          dat_coef <- data.table(pr_type = "full_dry")
          
        } else {
          
          if(all(sf_obs_011_pca$value_wet, na.rm = T)){
            # occurrence full wet
            dat_pred_o <- data.table(icell = sf_newdata$icell_1km,
                                     occurrence = 1)
            
            dat_coef_o <- data.table(pr_type = "full_wet")
            
          } else {
            # occurrence partly wet
            fmla <- as.formula(str_c("value_wet ~ ", str_c("PC", 1:n_pc, collapse = " + ")))
            glm2 <- glm(fmla,
                        data = sf_obs_011_pca, family = "binomial")
            dat_pred_o <- data.table(icell = sf_newdata$icell_1km,
                                     occurrence = predict(glm2, sf_newdata, type = "response"))
            dat_coef_o <- broom::tidy(glm2) %>% cbind(pr_type = "partial_wet")
          }
          
          # intensity
          fmla <- as.formula(str_c("value ~ ", str_c("PC", 1:n_pc, collapse = " + ")))
          lm2 <- lm(fmla, data = sf_obs_011_pca)
          
          dat_pred <- cbind(dat_pred_o,
                            intensity = predict(lm2, sf_newdata))
          
          dat_coef_i <- broom::tidy(lm2)
          
          dat_coef <- rbindlist(fill = T, list(
            dat_coef_o %>% cbind(pr_step = "occurrence"),
            dat_coef_i %>% cbind(pr_step = "intensity")
          ))
          
        }
        
        dat_pred[, pred := (occurrence > pr_p_occur) * intensity]
        
        if(pr_sqrt) dat_pred[, pred := pred*pred]
        
        dat_pred[pred < 0, pred := 0]
        
      } else {
        
        if(upscale_pcs){
          sf_obs_011_pca <- merge(sf_obs_011, dat_pc_upscaled)
        } else {
          
          if(match_temperature_gridcells){
            dat_zz <- sf_obs_011 %>% 
              st_drop_geometry() %>% 
              merge(dat_gridcells_temperature)
            
            sf_obs_011_pca <- sf_pca %>% 
              dplyr::filter(season == i_season) %>% 
              merge(dat_zz)
          } else {
            sf_obs_011_pca <- st_join(sf_obs_011,
                                      sf_pca %>% dplyr::filter(season == i_season),
                                      join = st_nearest_feature)
          }
          
        }
        
        
        if(tas_orog4pc1){
          fmla <- as.formula(str_c("value ~ orog + ", str_c("PC", 2:n_pc, collapse = " + ")))  
        } else {
          fmla <- as.formula(str_c("value ~ ", str_c("PC", 1:n_pc, collapse = " + ")))
        }
        
        lm4 <- lm(fmla, data = sf_obs_011_pca)
        
        
        dat_pred <- data.table(icell = sf_newdata$icell_1km,
                               pred = predict(lm4, sf_newdata))
        
        dat_coef <- broom::tidy(lm4)
        
      }
      
      dat_out <- merge(dat_obs_orog, dat_pred, all.x = T)
      setorder(dat_out, "icell_nc")
      
      
      ncvar_put(nc_out, varid = i_var, vals = dat_out$pred, 
                start = c(1, 1, i_date), count = c(-1, -1, 1))
      
      if(i_nc_sync %% n_nc_sync == 0) nc_sync(nc_out)
      
      data.table(dat_coef) %>% 
        cbind(date = dates_loop[i_date],
              variable = i_var)
      
    }
    
    
    nc_close(nc_out)
    
    dat_coef_out
  }
  
  
  saveRDS(zz, path("data/coef-obs-pcalm/", path_out_subdir, ext = "rds"))
  
  
  
  
  
}
