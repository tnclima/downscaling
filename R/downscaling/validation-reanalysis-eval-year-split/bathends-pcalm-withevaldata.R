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
library(purrr)

source("R/functions/create_empty_netcdf.R")
source("R/functions/inv_sub_reanalysis.R")
source("R/functions/snowfall.R")


extended_l_years_train_period <- readRDS("data/random-years-reanalysis-extended.rds")

path_tmp_nc <- "/home/climatedata/downscaling/validation-cv-reanalysis/zz-tmp-dspcalm/"
# if(dir_exists(path_tmp_nc)) stop("tmp directory exists!")
dir_create(path_tmp_nc)

path_in_baqdm <- "/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v6/ba-qdm/"

# settings ba ds ----------------------------------------------------

# ba_variants <- c("qdm", "mbcn")
# ba_variants <- c("mbcnspat")
# ba_variants <- c("qdm")

date_rcm_sub <- as.Date(c("1989-01-02", "2008-12-31"))
dates_loop <- seq(date_rcm_sub[1], date_rcm_sub[2], by = "day")
n_nc_sync <- 200 # intermediate save to nc_out file every n dates
n_cores <- 5 # parallel computation; (reads RCM memory, no need for crespi)

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

dat_inv_loop_mod <- dat_inv_loop[, .(gcm, institute_rcm, experiment, 
                                     ensemble, downscale_realisation)] %>% unique()



# ** settings eval ** -----------------------------------------------------


# crespi data for eval ----------------------------------------------------



date_sub <- as.Date(c("1989-01-02", "2008-12-31"))  

pctl_pr <- c(0.95, 0.99, 1)
pctl_tas <- c(0, 0.01, 0.05, 0.5, 0.95, 0.99, 1)
pctl_ecdf <- seq(0, 1, by=0.01)

elev_breaks <- seq(0, 3500, by = 500)


# elev --------------------------------------------------------------------

# file_orog <- "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc"
file_orog <- "/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc"
# file_orog <- "/home/climatedata/obs/orography/crespi_lonlat_1km_precipitation.nc"

dat_orog <- nc_grid_to_dt(file_orog, add_xy = T)
dat_orog <- dat_orog[!is.na(orog)]

# dat_orog$orog %>% hist(50)
# dat_orog$orog %>% summary

dat_orog[, elev_fct := cut(orog, breaks = elev_breaks, dig.lab = 5)]
# dat_orog %>% ggplot(aes(longitude, latitude, fill = orog))+geom_raster()
# summary(dat_orog$elev_fct)





# common icell upscaled crespi

dat1 <- nc_grid_to_dt("/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v2/ba-qdm-ds-qdm/pr_CLMcom-CCLM4-8-17_ECMWF-ERAINT_evaluation.nc",
                      date_range = c("2000-01-01", "2000-01-01"))
dat2 <- nc_grid_to_dt("/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v1/bads-qdm/pr_CLMcom-CCLM4-8-17_ECMWF-ERAINT_evaluation.nc",
                      date_range = c("2000-01-01", "2000-01-01"))
icell_common <- intersect(dat1[!is.na(pr), icell], dat2[!is.na(pr), icell])


# crespi data 

# defined above
# l_file_obs <- list(
#   tasmax = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MaxTemp.nc",
#   tasmin = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MinTemp.nc",
#   pr = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_Prec.nc"
# )

dat_pr <- nc_grid_to_dt(l_file_obs[["pr"]], date_range = date_sub)
setnames(dat_pr, 3, "pr_crespi")
dat_pr <- dat_pr[!is.na(pr_crespi)]

dat_tasmin <- nc_grid_to_dt(l_file_obs[["tasmin"]], date_range = date_sub)
setnames(dat_tasmin, 3, "tasmin_crespi")
dat_tasmin <- dat_tasmin[!is.na(tasmin_crespi)]

dat_tasmax <- nc_grid_to_dt(l_file_obs[["tasmax"]], date_range = date_sub)
setnames(dat_tasmax, 3, "tasmax_crespi")
dat_tasmax <- dat_tasmax[!is.na(tasmax_crespi)]

dat_crespi <- cbind(dat_pr, 
                    tasmax_crespi = dat_tasmax$tasmax_crespi, 
                    tasmin_crespi = dat_tasmin$tasmin_crespi)

dat_crespi[, hn_crespi := snowfall(pr_crespi, tasmax_crespi, tasmin_crespi)]

rm(dat_pr, dat_tasmax, dat_tasmin); gc();

dat_crespi[pr_crespi < 0, pr_crespi := 0]
dat_crespi[hn_crespi < 0, hn_crespi := 0]
dat_crespi[, season := mitmatmisc::season_fct(month(date))]
dat_crespi <- dat_crespi %>% merge(dat_orog[, .(icell, elev_fct)])

dat_crespi_tnaa <- dat_crespi[icell %in% icell_common,
                              lapply(.SD, mean),
                              .(date, season),
                              .SDcols = str_c(c("pr", "hn", "tasmax", "tasmin"), "_crespi")]

dat_crespi_elev <- dat_crespi[icell %in% icell_common,
                              lapply(.SD, mean),
                              .(date, season, elev_fct),
                              .SDcols = str_c(c("pr", "hn", "tasmax", "tasmin"), "_crespi")]


# ** start loop extended years ** ---------------------------------------------------------------

mitmatmisc::init_parallel_ubuntu(n_cores)


for(i_ext in 1:length(extended_l_years_train_period)){
  
  l_years_train_period <- extended_l_years_train_period[[i_ext]]
  
  
  files_ba <- dir_ls(path(path_in_baqdm, i_ext))
  
  foreach(
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
    
    file_out <- path(path_tmp_nc,
                     i_ext,
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
    
    
    foreach(
      i_date = seq_along(dates_loop)
      # .final = \(x) rbindlist(x, fill = T)
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
        
        # dat_coef <- broom::tidy(lm4)
      }
      
      
      
      # add data to nc ------------------------- #
      
      
      dat_out <- merge(dat_obs_orog, dat_pred, all.x = T)
      setorder(dat_out, "icell_nc")
      
      
      ncvar_put(nc_out, varid = i_var, vals = dat_out$pred, 
                start = c(1, 1, i_date), count = c(-1, -1, 1))
      
      if(i_nc_sync %% n_nc_sync == 0) nc_sync(nc_out)
      
      # data.table(dat_coef) %>% 
      #   cbind(date = dates_loop[i_date],
      #         variable = i_var,
      #         institute_rcm = i_rcm_name,
      #         ba = i_ba)
      
      
    }
    
    
    nc_close(nc_out)
    
    # dat_coef_out
    
  }
  
  
  
  
  
  # ** start eval ** ----------------------------------------------------------------
  
  i_path <- path(path_tmp_nc, i_ext)
  i_bads <- "ba-qdm-ds-pcalm"
  
  path_out <- "/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v6/"
  dir_create(path(path_out, "tnaa", c("mean-pctl", "ecdf", "dist-stat", "metrics", "spatcor")))
  dir_create(path(path_out, "elev", c("mean-pctl", "ecdf", "dist-stat", "metrics")))
  dir_create(path(path_out, "icell", c("mean-pctl", "dist-stat", "etccdi", "metrics")))
  
  
  
  # main loop ---------------------------------------------------------------
  
  files_bads <- dir_ls(i_path)
  
  dir_create(path(path_out, "tnaa", c("mean-pctl", "ecdf", "dist-stat", "metrics", "spatcor"), i_bads, i_ext))
  dir_create(path(path_out, "elev", c("mean-pctl", "ecdf", "dist-stat", "metrics"), i_bads, i_ext))
  dir_create(path(path_out, "icell", c("mean-pctl", "dist-stat", "etccdi", "metrics"), i_bads, i_ext))
  
  n_loop <- nrow(dat_inv_loop_mod)
  n_shift <- -1
  
  foreach(i = 1:n_loop) %do% {
    
    i_rcm_name <-  dat_inv_loop_mod[i, institute_rcm]
    files_read <- str_subset(files_bads, fixed(i_rcm_name))
    
    # if last file exists: skip rest of loop (save reading in a lot of data)
    if(file_exists(path(path_out, "icell", "metrics", i_bads, i_ext, i_rcm_name, ext = "rds"))) return(NULL)
    
    
    lgl_pr <- any(str_detect(files_read, fixed("pr")))
    
    if(lgl_pr){
      dat_pr <- nc_grid_to_dt(str_subset(files_read, "pr"))
      setnames(dat_pr, 3, "pr")
      dat_pr <- dat_pr[!is.na(pr)]
    }      
    
    dat_tasmin <- nc_grid_to_dt(str_subset(files_read, "tasmin"))
    setnames(dat_tasmin, 3, "tasmin")
    dat_tasmin <- dat_tasmin[!is.na(tasmin)]
    
    dat_tasmax <- nc_grid_to_dt(str_subset(files_read, "tasmax"))
    setnames(dat_tasmax, 3, "tasmax")
    dat_tasmax <- dat_tasmax[!is.na(tasmax)]
    
    if(lgl_pr){
      dat_i <- cbind(dat_pr, tasmax = dat_tasmax$tasmax, tasmin = dat_tasmin$tasmin)
      rm(dat_pr, dat_tasmin, dat_tasmax);gc();
      
      dat_i[, hn := snowfall(pr, tasmax, tasmin)]
      
      map_vars <- c("pr", "hn", "tasmax", "tasmin")
    } else {
      dat_i <- cbind(dat_tasmax, tasmin = dat_tasmin$tasmin)
      rm(dat_tasmin, dat_tasmax);gc();
      
      map_vars <- c("tasmax", "tasmin")
    }
    
    dat_i[, season := mitmatmisc::season_fct(month(date))]
    dat_i <- dat_i %>% merge(dat_orog[, .(icell, elev_fct)])
    
    
    dat_i_tnaa <- dat_i[icell %in% icell_common,
                        lapply(.SD, mean),
                        .(date, season),
                        .SDcols = map_vars]
    
    dat_i_elev <- dat_i[icell %in% icell_common,
                        lapply(.SD, mean),
                        .(date, season, elev_fct),
                        .SDcols = map_vars]
    
    dat_i <- merge(dat_i, dat_crespi, by = c("icell", "date", "season", "elev_fct"))
    dat_i_tnaa <- merge(dat_i_tnaa, dat_crespi_tnaa)
    dat_i_elev <- merge(dat_i_elev, dat_crespi_elev)
    
    if(lgl_pr){
      dat_i_tnaa[, pr_crespi := data.table::shift(pr_crespi, n_shift)]
      dat_i_tnaa[, hn_crespi := data.table::shift(hn_crespi, n_shift)]
      dat_i_tnaa <- dat_i_tnaa[!is.na(pr_crespi)]
      
      dat_i_elev[, pr_crespi := data.table::shift(pr_crespi, n_shift), .(elev_fct)]
      dat_i_elev[, hn_crespi := data.table::shift(hn_crespi, n_shift), .(elev_fct)]
      dat_i_elev <- dat_i_elev[!is.na(pr_crespi)]
      
      dat_i[, pr_crespi := data.table::shift(pr_crespi, n_shift), .(icell)]
      dat_i[, hn_crespi := data.table::shift(hn_crespi, n_shift), .(icell)]
      dat_i <- dat_i[!is.na(pr_crespi)]
      
    }      
    
    # ** tnaa --------------------------------------------------------------------
    
    if(!file_exists(path(path_out, "tnaa", "mean-pctl", i_bads, i_ext, i_rcm_name, ext = "rds"))){
      
      if(lgl_pr){
        dat_i_tnaa_out1 <- dat_i_tnaa[, c(
          mitmatmisc::calc_pctl(pr, pctl_pr, "pr_p"),
          pr_mean = mean(pr),
          mitmatmisc::calc_pctl(hn, pctl_pr, "hn_p"),
          hn_mean = mean(hn),
          mitmatmisc::calc_pctl(tasmin, pctl_tas, "tasmin_p"),
          mitmatmisc::calc_pctl(tasmax, pctl_tas, "tasmax_p")
        ), .(season)]
      } else {
        dat_i_tnaa_out1 <- dat_i_tnaa[, c(
          # mitmatmisc::calc_pctl(pr, pctl_pr, "pr_p"),
          # pr_mean = mean(pr),
          # mitmatmisc::calc_pctl(hn, pctl_pr, "hn_p"),
          # hn_mean = mean(hn),
          mitmatmisc::calc_pctl(tasmin, pctl_tas, "tasmin_p"),
          mitmatmisc::calc_pctl(tasmax, pctl_tas, "tasmax_p")
        ), .(season)]
      }
      
      saveRDS(dat_i_tnaa_out1, path(path_out, "tnaa", "mean-pctl", i_bads, i_ext, i_rcm_name, ext = "rds"))
    }
    
    
    if(!file_exists(path(path_out, "tnaa", "ecdf", i_bads, i_ext, i_rcm_name, ext = "rds"))){
      
      dat_i_tnaa_out2 <- map(map_vars, \(x){
        dat_i_tnaa[, 
                   .(qval = quantile(value, pctl_ecdf),
                     pctl = pctl_ecdf,
                     variable = x),
                   .(season),
                   env = list(value = x)]
      }) %>% rbindlist  
      
      saveRDS(dat_i_tnaa_out2, path(path_out, "tnaa", "ecdf", i_bads, i_ext, i_rcm_name, ext = "rds"))
    }          
    # 
    # if(!file_exists(path(path_out, "tnaa", "dist-stat", i_bads, i_ext, i_rcm_name, ext = "rds"))){
    #   dat_i_tnaa_out3 <-  map(map_vars, \(x){
    #     map(c("ad_test", "cvm_test", "ks_test", "wass_test"), \(y){
    #       dat_i_tnaa[, 
    #                  as.list(f_test(value, value_crespi, nboots = dist_stat_nboots, keep.boots = F)) %>% 
    #                    setNames(c("dist_stat", "pval")) %>% 
    #                    c(dist_test = y, variable = x),
    #                  .(season),
    #                  env = list(f_test = y, value = x, value_crespi = str_c(x, "_crespi"))]
    #     }) %>% rbindlist
    #   }) %>% rbindlist   
    #   saveRDS(dat_i_tnaa_out3, path(path_out, "tnaa", "dist-stat", i_bads, i_rcm_name, ext = "rds"))
    # }
    
    
    if(!file_exists(path(path_out, "tnaa", "metrics", i_bads, i_ext, i_rcm_name, ext = "rds"))){
      
      dat_i_tnaa_out6 <- map(map_vars, \(x){
        dat_i_tnaa[, 
                   .(mae = mean(abs(value - value_crespi)),
                     bias = mean(value - value_crespi),
                     bias_rel = mean(value - value_crespi)/mean(value_crespi),
                     corr = cor(value, value_crespi),
                     variable = x),
                   .(season),
                   env = list(value = x, value_crespi = str_c(x, "_crespi"))]
      }) %>% rbindlist  
      
      saveRDS(dat_i_tnaa_out6, path(path_out, "tnaa", "metrics", i_bads, i_ext, i_rcm_name, ext = "rds"))
    } 
    
    
    if(!file_exists(path(path_out, "tnaa", "spatcor", i_bads, i_ext, i_rcm_name, ext = "rds"))){
      
      dat_i_tnaa_out7 <- map(map_vars, \(x){
        dat_i[, 
              .(spatcor = suppressWarnings(cor(value, value_crespi)),
                spatcor_nonzero = suppressWarnings(
                  cor(value[value > 0 & value_crespi > 0], 
                      value_crespi[value > 0 & value_crespi > 0])
                ),
                variable = x),
              .(season, date),
              env = list(value = x, value_crespi = str_c(x, "_crespi"))] %>% 
          .[,
            .(spatcor = mean(spatcor, na.rm = T),
              spatcor_nonzero = mean(spatcor_nonzero, na.rm = T)),
            .(season, variable)]
      }) %>% rbindlist   
      
      saveRDS(dat_i_tnaa_out7, path(path_out, "tnaa", "spatcor", i_bads, i_ext, i_rcm_name, ext = "rds"))
    } 
    
    
    # ** elev --------------------------------------------------------------------
    
    if(!file_exists(path(path_out, "elev", "mean-pctl", i_bads, i_ext, i_rcm_name, ext = "rds"))){
      
      if(lgl_pr){
        dat_i_elev_out1 <- dat_i_elev[, c(
          mitmatmisc::calc_pctl(pr, pctl_pr, "pr_p"),
          pr_mean = mean(pr),
          mitmatmisc::calc_pctl(hn, pctl_pr, "hn_p"),
          hn_mean = mean(hn),
          mitmatmisc::calc_pctl(tasmin, pctl_tas, "tasmin_p"),
          mitmatmisc::calc_pctl(tasmax, pctl_tas, "tasmax_p")
        ), .(season, elev_fct)]
      } else {
        dat_i_elev_out1 <- dat_i_elev[, c(
          # mitmatmisc::calc_pctl(pr, pctl_pr, "pr_p"),
          # pr_mean = mean(pr),
          # mitmatmisc::calc_pctl(hn, pctl_pr, "hn_p"),
          # hn_mean = mean(hn),
          mitmatmisc::calc_pctl(tasmin, pctl_tas, "tasmin_p"),
          mitmatmisc::calc_pctl(tasmax, pctl_tas, "tasmax_p")
        ), .(season, elev_fct)]
      }
      
      saveRDS(dat_i_elev_out1, path(path_out, "elev", "mean-pctl", i_bads, i_ext, i_rcm_name, ext = "rds"))
    }
    
    if(!file_exists(path(path_out, "elev", "ecdf", i_bads, i_ext, i_rcm_name, ext = "rds"))){
      dat_i_elev_out2 <- map(map_vars, \(x){
        dat_i_elev[, 
                   .(qval = quantile(value, pctl_ecdf),
                     pctl = pctl_ecdf,
                     variable = x),
                   .(season, elev_fct),
                   env = list(value = x)]
      }) %>% rbindlist    
      
      saveRDS(dat_i_elev_out2, path(path_out, "elev", "ecdf", i_bads, i_ext, i_rcm_name, ext = "rds"))
    }
    # 
    # if(!file_exists(path(path_out, "elev", "dist-stat", i_bads, i_rcm_name, ext = "rds"))){
    #   dat_i_elev_out3 <-  map(map_vars, \(x){
    #     map(c("ad_test", "cvm_test", "ks_test", "wass_test"), \(y){
    #       dat_i_elev[, 
    #                  as.list(f_test(value, value_crespi, nboots = dist_stat_nboots, keep.boots = F)) %>% 
    #                    setNames(c("dist_stat", "pval")) %>% 
    #                    c(dist_test = y, variable = x),
    #                  .(season, elev_fct),
    #                  env = list(f_test = y, value = x, value_crespi = str_c(x, "_crespi"))]
    #     }) %>% rbindlist
    #   }) %>% rbindlist  
    #   
    #   saveRDS(dat_i_elev_out3, path(path_out, "elev", "dist-stat", i_bads, i_rcm_name, ext = "rds"))
    # }
    # 
    if(!file_exists(path(path_out, "elev", "metrics", i_bads, i_ext, i_rcm_name, ext = "rds"))){
      
      dat_i_elev_out6 <- map(map_vars, \(x){
        dat_i_elev[, 
                   .(mae = mean(abs(value - value_crespi)),
                     bias = mean(value - value_crespi),
                     bias_rel = mean(value - value_crespi)/mean(value_crespi),
                     corr = cor(value, value_crespi),
                     variable = x),
                   .(season, elev_fct),
                   env = list(value = x, value_crespi = str_c(x, "_crespi"))]
      }) %>% rbindlist  
      
      saveRDS(dat_i_elev_out6, path(path_out, "elev", "metrics", i_bads, i_ext, i_rcm_name, ext = "rds"))
    }
    
    
    
    # ** icell ----------------------------------------------------------------
    
    if(!file_exists(path(path_out, "icell", "mean-pctl", i_bads, i_ext, i_rcm_name, ext = "rds"))){
      
      if(lgl_pr){
        dat_i_icell_out1 <- dat_i[, c(
          mitmatmisc::calc_pctl(pr, pctl_pr, "pr_p"),
          pr_mean = mean(pr),
          mitmatmisc::calc_pctl(hn, pctl_pr, "hn_p"),
          hn_mean = mean(hn),
          mitmatmisc::calc_pctl(tasmin, pctl_tas, "tasmin_p"),
          mitmatmisc::calc_pctl(tasmax, pctl_tas, "tasmax_p")
        ), .(season, icell)]
      } else {
        dat_i_icell_out1 <- dat_i[, c(
          # mitmatmisc::calc_pctl(pr, pctl_pr, "pr_p"),
          # pr_mean = mean(pr),
          # mitmatmisc::calc_pctl(hn, pctl_pr, "hn_p"),
          # hn_mean = mean(hn),
          mitmatmisc::calc_pctl(tasmin, pctl_tas, "tasmin_p"),
          mitmatmisc::calc_pctl(tasmax, pctl_tas, "tasmax_p")
        ), .(season, icell)]
      }
      
      saveRDS(dat_i_icell_out1, path(path_out, "icell", "mean-pctl", i_bads, i_ext, i_rcm_name, ext = "rds"))
    }
    
    
    if(!file_exists(path(path_out, "icell", "metrics", i_bads, i_ext, i_rcm_name, ext = "rds"))){
      
      dat_i_icell_out6 <- map(map_vars, \(x){
        dat_i[, 
              .(mae = mean(abs(value - value_crespi)),
                bias = mean(value - value_crespi),
                bias_rel = mean(value - value_crespi)/mean(value_crespi),
                corr = cor(value, value_crespi),
                variable = x),
              .(season, icell),
              env = list(value = x, value_crespi = str_c(x, "_crespi"))]
      }) %>% rbindlist  
      
      saveRDS(dat_i_icell_out6, path(path_out, "icell", "metrics", i_bads, i_ext, i_rcm_name, ext = "rds"))
    }
    
    
  }
  
  
  
  
  # end i_ext ---------------------------------------------------------------
  
  
  cat(format(Sys.time()), "- done ", i_ext, "\n")
  # dir_delete(path_tmp_nc)
  
  
  
  
  
  
  
}
