# 

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

source("R/functions/create_empty_netcdf.R")
source("R/functions/inv_sub_reanalysis.R")

# settings - variables ----------------------------------------------------

path_out <- "/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v2/"
ba_variants <- c("qdm", "mbcn")

date_rcm_sub <- as.Date(c("1989-01-02", "2008-12-31"))
dates_loop <- seq(date_rcm_sub[1], date_rcm_sub[2], by = "day")
n_nc_sync <- 200 # intermediate save to nc_out file every n dates
n_cores <- 1 # parallel computation; (reads RCM memory, no need for crespi)

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

dat_gridcells_temperature <- readRDS("data/ked-gridcells-temperature.rds") %>% as.data.table()


# inventory ---------------------------------------------------------------

dat_inv <- inv_sub_reanalysis()
dat_inv_loop <- dat_inv[variable %in% c("pr", "tasmin", "tasmax")]



# main loop ---------------------------------------------------------------

mitmatmisc::init_parallel_ubuntu(n_cores)

for(i_ba in ba_variants){
  
  files_ba <- dir_ls(path(path_out, str_c("ba-", i_ba)))
  
  zz <- foreach(
    i_file_ba = files_ba,
    .inorder = F
  ) %dopar% {
    
    i_file_ba_split <- i_file_ba %>% 
      path_file() %>% 
      str_split_1("_")
    
    i_rcm_name <- i_file_ba_split[2]
    i_var <- i_file_ba_split[1]
    file_rcm <- i_file_ba
    
    file_rcm_orog <- dat_inv[variable == "orog" & institute_rcm == i_rcm_name, list_files[[1]]]
    # file_obs_orog <- "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc"
    file_obs_orog <- l_file_obs_orog[[i_var]]
    
    file_out <- path(path_out,
                     str_c("ba-", i_ba, "-ds-pcalm"),
                     path_file(i_file_ba))
    
    if(file_exists(file_out)) return(NULL)
    
    
    # data --------------------------------------------------------------------
    
    dat_rcm_orog <- nc_grid_to_dt(file_rcm_orog, add_xy = T)
    dat_rcm_orog[, date := NULL]
    
    dat_rcm <- nc_grid_to_dt(file_rcm, date_range = date_rcm_sub)
    setnames(dat_rcm, i_var, "value")
    
    # if(i_var == "pr"){
    #   dat_rcm[, value := value * 86400]
    # } else {
    #   dat_rcm[, value := value - 273.15]
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
    
    # time stuff --------------------------------------------------------------
    
    # non-standard cal workaround
    nc_rcm <- nc_open(file_rcm)
    raw_times <- ncdf4.helpers::nc.get.time.series(nc_rcm)
    mapped_times <- eurocordexr::map_non_standard_calendar(raw_times)
    dates_rcm <- mapped_times$dates_full
    nc_close(nc_rcm)
    
    # outfile ---------------------------------------------------------
    
    dir_create(path_dir(file_out))
    create_emtpy_netcdf(file_template = file_obs_orog,
                        file_out = file_out,
                        l_varinfo = l_nc_info[[i_var]],
                        date_period = date_rcm_sub,
                        overwrite = F)
    
    # main proc ---------------------------------------------------------------
    
    nc_out <- nc_open(file_out, write = T)
    i_nc_sync <- 0
    
    for(i_date in seq_along(dates_loop)){
      
      # i_rcm <- mapped_times[dates_loop[i_date] == dates_full, idx_pcict] # for non-standard cal
      
      i_season <- mitmatmisc::season_fct(month(dates_loop[i_date]))
      
      sf_newdata <- sf_pca %>% 
        dplyr::filter(season == i_season) %>% 
        dplyr::rename(orog = orog_1km)
      
      sf_rcm <- dat_rcm[date == dates_loop[i_date]] %>% 
        merge(dat_rcm_orog) %>% 
        st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
        st_join(sf_tnaa, left = F)
      
      if(i_var == "pr"){
        
        sf_rcm_pca <- st_join(sf_rcm, 
                              sf_pca %>% dplyr::filter(season == i_season), 
                              join = st_nearest_feature)
        sf_rcm_pca$value_wet <- sf_rcm_pca$value > pr_th
        
        if(pr_sqrt) sf_rcm_pca <- dplyr::mutate(sf_rcm_pca, value = sqrt(value))
        
        # full dry
        if(all(!sf_rcm_pca$value_wet, na.rm = T)){
          dat_pred <- data.table(icell = sf_newdata$icell_1km,
                                 occurrence = 0,
                                 intensity = 0)
        } else {
          
          if(all(sf_rcm_pca$value_wet, na.rm = T)){
            # occurrence full wet
            dat_pred_o <- data.table(icell = sf_newdata$icell_1km,
                                     occurrence = 1)
            
          } else {
            # occurrence partly wet
            glm2 <- glm(value_wet ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6,
                        data = sf_rcm_pca, family = "binomial")
            dat_pred_o <- data.table(icell = sf_newdata$icell_1km,
                                     occurrence = predict(glm2, sf_newdata, type = "response"))
          }
          
          # intensity
          lm2 <- lm(value ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = sf_rcm_pca)
          
          dat_pred <- cbind(dat_pred_o,
                            intensity = predict(lm2, sf_newdata))
          
          
        }
        
        dat_pred[, pred := (occurrence > pr_p_occur) * intensity]
        
        if(pr_sqrt) dat_pred[, pred := pred*pred]
        
        dat_pred[pred < 0, pred := 0]
        
      } else {
        
        dat_zz <- sf_rcm %>% 
          st_drop_geometry() %>% 
          merge(dat_gridcells_temperature[institute_rcm == i_rcm_name])
        
        sf_rcm_pca <- sf_pca %>% 
          dplyr::filter(season == i_season) %>% 
          merge(dat_zz)
        
        lm4 <- lm(value ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = sf_rcm_pca)
        
        
        dat_pred <- data.table(icell = sf_newdata$icell_1km,
                               pred = predict(lm4, sf_newdata))
        
      }
      
      dat_out <- merge(dat_obs_orog, dat_pred, all.x = T)
      setorder(dat_out, "icell_nc")
      
      
      ncvar_put(nc_out, varid = i_var, vals = dat_out$pred, 
                start = c(1, 1, i_date), count = c(-1, -1, 1))
      
      if(i_nc_sync %% n_nc_sync == 0) nc_sync(nc_out)
    }
    
    
    nc_close(nc_out)
    
  }
  
}



