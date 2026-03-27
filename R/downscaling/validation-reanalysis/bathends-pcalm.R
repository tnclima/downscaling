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
source("R/functions/inv_sub_reanalysis.R")

# settings - variables ----------------------------------------------------

path_out <- "/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v2/"
# ba_variants <- c("qdm", "mbcn")
ba_variants <- c("mbcnspat")

date_rcm_sub <- as.Date(c("1989-01-02", "2008-12-31"))
dates_loop <- seq(date_rcm_sub[1], date_rcm_sub[2], by = "day")
n_nc_sync <- 200 # intermediate save to nc_out file every n dates
n_cores <- 7 # parallel computation; (reads RCM memory, no need for crespi)

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
dat_gridcells_temperature <- readRDS("data/ked-gridcells-temperature.rds") %>% as.data.table()


upscale_pcs <- T # if T, high-res PCs will be upscaled and then merged to RCM
# otherwise, nearest PC to gridcell center will be matched to RCM

# n_pc <- 6 # number of PCs to use
# orog_pcalog <- T # if T, use orog instead of PC1 for temperature variables
# pr_pcalog <- T # if T, use results from logisticPCA for occurrence


# dat_settings <- rbind(fill = T,
#                       data.table(variable = "pr", n_pc = 9, orog_pcalog = F, pr_single = T) |> 
#                         cbind(season = c("DJF", "MAM", "JJA", "SON")),
#                       data.table(variable = "tasmin", n_pc = 3, orog_pcalog = F) |> 
#                         cbind(season = c("DJF", "MAM", "JJA", "SON")),
#                       data.table(variable = "tasmax") |> 
#                         cbind(season = c("DJF", "MAM", "JJA", "SON"),
#                               n_pc = c(3, 5, 5, 5), orog_pcalog = c(F, T, T, T))
# )

# tasmax -> while better for obs, not so for RCM!


dat_settings <- rbind(fill = T,
                      data.table(variable = "pr", n_pc = 9, orog_pcalog = F, pr_single = T) |> 
                        cbind(season = c("DJF", "MAM", "JJA", "SON")),
                      data.table(variable = "tasmin", n_pc = 3, orog_pcalog = F) |> 
                        cbind(season = c("DJF", "MAM", "JJA", "SON")),
                      data.table(variable = "tasmax", n_pc = 3, orog_pcalog = F) |> 
                        cbind(season = c("DJF", "MAM", "JJA", "SON"))
)



# inventory ---------------------------------------------------------------

dat_inv <- inv_sub_reanalysis()
dat_inv_loop <- dat_inv[variable %in% c("pr", "tasmin", "tasmax")]



# main loop ---------------------------------------------------------------

mitmatmisc::init_parallel_ubuntu(n_cores)

zz2 <- foreach(
  i_ba = ba_variants,
  .final = \(x) rbindlist(x, fill = T)
) %do% {
  
  files_ba <- dir_ls(path(path_out, str_c("ba-", i_ba)))
  
  zz <- foreach(
    i_file_ba = files_ba,
    .inorder = F,
    .final = \(x) rbindlist(x, fill = T)
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
    
    rs_template_rcm <- rast(file_rcm_orog)
    rs_template_1km <- rast(l_file_obs_orog[[i_var]])
    
    
    # pca data
    dat_pca <- readRDS(str_c("/home/climatedata/downscaling/pca-ked/crespi-pca/final-choice/season-sub0-", 
                             i_var, "-centerTRUE-scaleFALSE.rds"))
    sf_pca <- dat_pca %>%
      merge(dat_obs_orog[!is.na(orog)]) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    setnames(sf_pca, "orog", "orog_1km")
    setnames(sf_pca, "icell", "icell_1km")
    
    # if(orog_pcalog){
    dat_pcalog <- readRDS("/home/climatedata/downscaling/pca-ked/crespi-pca/final-choice/pcalog-season-sub0-pr-th0.1-maineffectsTRUE.rds")
    sf_pcalog <- dat_pca %>%
      merge(dat_obs_orog[!is.na(orog)]) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    setnames(sf_pcalog, "orog", "orog_1km")
    setnames(sf_pcalog, "icell", "icell_1km")
    # }
    
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
    
    
    dat_coef_out <- foreach(
      i_date = seq_along(dates_loop),
      .final = \(x) rbindlist(x, fill = T)
    ) %do% {
      
      # i_rcm <- mapped_times[dates_loop[i_date] == dates_full, idx_pcict] # for non-standard cal
      
      i_season <- mitmatmisc::season_fct(month(dates_loop[i_date]))
      
      # settings
      n_pc <- dat_settings[variable == i_var & season == i_season, n_pc]
      pr_single <- dat_settings[variable == i_var & season == i_season, pr_single]
      orog_pcalog <- dat_settings[variable == i_var & season == i_season, orog_pcalog]
      
      
      sf_newdata <- sf_pca %>% 
        dplyr::filter(season == i_season) %>% 
        dplyr::rename(orog = orog_1km)
      
      if(orog_pcalog){
        sf_newdata_pcalog <- sf_pcalog %>% 
          dplyr::filter(season == i_season) %>% 
          dplyr::rename(orog = orog_1km)
      }
      
      sf_rcm <- dat_rcm[date == dates_loop[i_date]] %>% 
        merge(dat_rcm_orog) %>% 
        st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
        st_join(sf_tnaa, left = F)
      
      if(upscale_pcs){
        
        rs_pca_1km <- rast(rs_template_1km, nlyrs = n_pc)
        rs_pca_1km[
          dat_pca[season == i_season, icell]
        ] <- dat_pca[season == i_season, str_c("PC", 1:n_pc), with=F]
        
        rs_pca_1km %>% flip %>% 
          aggregate(fact = 10) %>%
          resample(rs_template_rcm) -> rs_pca_rcm
        
        dat_pc_upscaled <- rs_pca_rcm %>% as.data.table(cells = T, na.rm = F)
        setnames(dat_pc_upscaled, c("icell", str_c("PC", 1:n_pc)))
        
        if(orog_pcalog){
          rs_pcalog_1km <- rast(rs_template_1km, nlyrs = n_pc)
          rs_pcalog_1km[
            dat_pcalog[season == i_season, icell]
          ] <- dat_pcalog[season == i_season, str_c("PC", 1:n_pc), with=F]
          
          rs_pcalog_1km %>% flip %>% 
            aggregate(fact = 10) %>%
            resample(rs_template_rcm) -> rs_pcalog_rcm
          
          dat_pclog_upscaled <- rs_pcalog_rcm %>% as.data.table(cells = T, na.rm = F)
          setnames(dat_pclog_upscaled, c("icell", str_c("PC", 1:n_pc)))
        }
      }
      
    
      if(i_var == "pr"){
        # precip ----------------------------- # 
        
        if(upscale_pcs){
          sf_rcm_pca <- merge(sf_rcm, dat_pc_upscaled)
          if(orog_pcalog) {
            sf_rcm_pcalog <- merge(sf_rcm, dat_pclog_upscaled)
          }
        } else {
          sf_rcm_pca <- st_join(sf_rcm, 
                                sf_pca %>% dplyr::filter(season == i_season), 
                                join = st_nearest_feature)
          if(orog_pcalog){
            sf_rcm_pcalog <- st_join(sf_rcm, 
                                     sf_pcalog %>% dplyr::filter(season == i_season), 
                                     join = st_nearest_feature)
          }
          
        }
        
        if(pr_sqrt) sf_rcm_pca <- dplyr::mutate(sf_rcm_pca, value = sqrt(value))
        
        if(pr_single){
          # one mod ----------------------------- # 
          
          fmla <- as.formula(str_c("value ~ ", str_c("PC", 1:n_pc, collapse = " + ")))
          lm2 <- lm(fmla, data = sf_rcm_pca)
          
          dat_pred <- data.table(icell = sf_newdata$icell_1km,
                                 pred = predict(lm2, sf_newdata))
          dat_pred[pred < 0, pred := 0]
          dat_coef <- broom::tidy(lm2)
          
        } else {
          
          # separate occurrence and intensity ----------------------------- # 
          sf_rcm_pca$value_wet <- sf_rcm_pca$value > pr_th
          if(orog_pcalog){
            sf_rcm_pcalog$value_wet <- sf_rcm_pcalog$value > pr_th
          }
          
          
          # full dry
          if(all(!sf_rcm_pca$value_wet)){
            dat_pred <- data.table(icell = sf_newdata$icell_1km,
                                   occurrence = 0,
                                   intensity = 0)
            dat_coef <- data.table(pr_type = "full_dry")
          } else {
            
            if(all(sf_rcm_pca$value_wet)){
              # occurrence full wet
              dat_pred_o <- data.table(icell = sf_newdata$icell_1km,
                                       occurrence = 1)
              dat_coef_o <- data.table(pr_type = "full_wet")
              
            } else {
              # occurrence partly wet
              fmla <- as.formula(str_c("value_wet ~ ", str_c("PC", 1:n_pc, collapse = " + ")))
              
              if(orog_pcalog){
                glm2 <- glm(fmla, data = sf_rcm_pcalog, family = "binomial")
                dat_pred_o <- data.table(icell = sf_newdata_pcalog$icell_1km,
                                         occurrence = predict(glm2, sf_newdata_pcalog, type = "response"))
              } else {
                glm2 <- glm(fmla, data = sf_rcm_pca, family = "binomial")
                dat_pred_o <- data.table(icell = sf_newdata$icell_1km,
                                         occurrence = predict(glm2, sf_newdata, type = "response"))
              }
              
              dat_coef_o <- broom::tidy(glm2) %>% cbind(pr_type = "partial_wet")
              
            }
            # intensity
            
            fmla <- as.formula(str_c("value ~ ", str_c("PC", 1:n_pc, collapse = " + ")))
            lm2 <- lm(fmla, data = sf_rcm_pca)
            
            
            dat_pred <- cbind(dat_pred_o,
                              intensity = predict(lm2, sf_newdata))
            dat_coef_i <- broom::tidy(lm2)
            
            dat_coef <- rbindlist(fill = T, list(
              dat_coef_o %>% cbind(pr_step = "occurrence"),
              dat_coef_i %>% cbind(pr_step = "intensity")
            ))
            
          }
          
          dat_pred[, pred := (occurrence > pr_p_occur) * intensity]
          dat_pred[pred < 0, pred := 0]
          
        }
        
        if(pr_sqrt) dat_pred[, pred := pred*pred]
        
      } else {
        # temperature ----------------------------- # 
        
        if(upscale_pcs){
          sf_rcm_pca <- merge(sf_rcm, dat_pc_upscaled)
        } else {
          
          if(match_temperature_gridcells){
            dat_zz <- sf_rcm %>% 
              st_drop_geometry() %>% 
              merge(dat_gridcells_temperature)
            
            sf_rcm_pca <- sf_pca %>% 
              dplyr::filter(season == i_season) %>% 
              merge(dat_zz)
          } else {
            sf_rcm_pca <- st_join(sf_rcm,
                                  sf_pca %>% dplyr::filter(season == i_season),
                                  join = st_nearest_feature)
          }
          
        }
        
        
        if(orog_pcalog){
          fmla <- as.formula(str_c("value ~ orog + ", str_c("PC", 2:n_pc, collapse = " + ")))  
        } else {
          fmla <- as.formula(str_c("value ~ ", str_c("PC", 1:n_pc, collapse = " + ")))
        }
        
        
        lm4 <- lm(fmla, data = sf_rcm_pca)
        
        dat_pred <- data.table(icell = sf_newdata$icell_1km,
                               pred = predict(lm4, sf_newdata))
        
        dat_coef <- broom::tidy(lm4)
      }
      
      
      
      # add data to nc ------------------------- #
      
      
      dat_out <- merge(dat_obs_orog, dat_pred, all.x = T)
      setorder(dat_out, "icell_nc")
      
      
      ncvar_put(nc_out, varid = i_var, vals = dat_out$pred, 
                start = c(1, 1, i_date), count = c(-1, -1, 1))
      
      if(i_nc_sync %% n_nc_sync == 0) nc_sync(nc_out)
      
      data.table(dat_coef) %>% 
        cbind(date = dates_loop[i_date],
              variable = i_var,
              institute_rcm = i_rcm_name,
              ba = i_ba)
      
      
    }
    
    
    nc_close(nc_out)
    
    dat_coef_out
    
  }
  
}

saveRDS(zz2, "data/coef-bathends-pcalm.rds")

