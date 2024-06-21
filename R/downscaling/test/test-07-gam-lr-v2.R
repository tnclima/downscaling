# gam test lr example

library(terra)
library(magrittr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(fs)
library(ggplot2)
library(patchwork)
library(mgcViz)
library(forcats)
library(scico)

source("R/functions/create_empty_netcdf.R")
# source("R/functions/lapse_rate_varying.R")
source("R/functions/inv_sub.R")

f_cons <- function(vals_out, ratio = T){
  
  if(conserve_largescale){
    rs_rcm_i <- rast(rs_rcm_orog)
    rs_rcm_i[] <- vals_rcm
    rs_rcm_i2 <- resample(rs_rcm_i, rs_obs_orog, "near")
    if(conserve_largescale_cell){
      rs_ds <- rast(rs_obs_orog)
      rs_ds[] <- vals_out
      rs_ds_agg <- zonal(rs_ds, rs_cells_rcm_obs, as.raster = T)
      if(ratio){
        rs_ds_scaled <- rs_ds/rs_ds_agg * rs_rcm_i2
      } else {
        rs_ds_scaled <- rs_ds - rs_ds_agg + rs_rcm_i2
      }
      vals_out <- rs_ds_scaled[]
    } else {
      if(ratio){
        vals_out <- vals_out / mean(vals_out) * mean(rs_rcm_i2[]) 
      } else {
        vals_out <- vals_out - mean(vals_out) + mean(rs_rcm_i2[]) 
      }
    }
  }
  vals_out
}

# settings - variables ----------------------------------------------------

# path_out <- "/home/climatedata/downscaling/zz_temp/loop-01-test-gam/"
path_out <- "fig/test-gam-lr-v2/"
kk_basis <- 5
pr_min_nonzero <- 20 # (precip-only) minimum number of non-zero values to fit 
# the gam model (total ncell_rcm = 391); 
# if below large-scale values are uniformly replicated
pr_min_nonzero_th <- 0.1 # threshold (mm) to count actual zeros
conserve_largescale <- T # ensure large-scale values match sum/means of downscaled values?
conserve_largescale_cell <- F # conservation at grid cell level (T) or for whole extent (F)

date_rcm_sub <- as.Date(c("1981-01-01", "2020-12-31"))
dates_loop <- seq(date_rcm_sub[1], date_rcm_sub[2], by = "day")

file_obs_orog <- "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc"
rs_template_tnaa <- rast("/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc")

# inventory ---------------------------------------------------------------

dat_inv <- inv_sub()
dat_inv_loop <- dat_inv[experiment == "rcp85" & variable %in% c("tasmin", "tasmax", "pr")]

# main loop ---------------------------------------------------------------


for(i_inv in 1:nrow(dat_inv_loop)){
  
  i_rcm_name <- dat_inv_loop[i_inv, institute_rcm]
  i_var <- dat_inv_loop[i_inv, variable]
  
  
  file_rcm_orog <- dat_inv[variable == "orog" & institute_rcm == i_rcm_name, list_files[[1]]]
  file_rcm <- dat_inv_loop[i_inv, list_files[[1]]]
  
  file_out <- path(path_out,
                   dat_inv_loop[i_inv, 
                                paste(variable, centers, institute_rcm, 
                                      gcm, experiment, sep = "_")],
                   ext = "pdf")
  
  
  if(file_exists(file_out)) next
  
  
  
  
  
  # data --------------------------------------------------------------------
  
  rs_rcm_orog <- rast(file_rcm_orog)
  rs_obs_orog <- rast(file_obs_orog)
  
  cells_obs <- which(!is.na(rs_template_tnaa[]))
  cells_obs_na <- which(is.na(rs_template_tnaa[]))
  
  rs_rcm <- rast(file_rcm)
  mat_rcm <- values(rs_rcm, mat = T)
  # vals_rcm_orog <- values(rs_rcm_orog, mat = F)
  # vals_obs_orog <- values(rs_obs_orog, mat = F)
  
  dt_rcm_orog <- as.data.table(rs_rcm_orog, xy = T, na.rm = F)
  dt_obs_orog <- as.data.table(rs_obs_orog, xy = T, na.rm = F)
  
  if(conserve_largescale & conserve_largescale_cell){
    # create xy lookup rasters crespi-rcm
    rs_rcm_cells <- rs_rcm_orog
    rs_rcm_cells[] <- 1:ncell(rs_rcm_cells)
    rs_cells_rcm_obs <- resample(rs_rcm_cells, rs_obs_orog, method = "near")
    names(rs_cells_rcm_obs) <- "rcm_cell"
  }
  
  # time stuff --------------------------------------------------------------
  
  # non-standard cal workaround
  nc_rcm <- nc_open(file_rcm)
  raw_times <- ncdf4.helpers::nc.get.time.series(nc_rcm)
  mapped_times <- eurocordexr::map_non_standard_calendar(raw_times)
  dates_rcm <- mapped_times$dates_full
  nc_close(nc_rcm)
  
  
  
  # outfile ---------------------------------------------------------
  
  pdf(file_out, width = 18, height = 10)
  # create_emtpy_netcdf(file_template = file_obs_orog, 
  #                     file_out = file_out, 
  #                     l_varinfo = l_nc_info[[i_var]], 
  #                     date_period = range(dates_rcm),
  #                     overwrite = F)
  
  
  # main proc ---------------------------------------------------------------
  
  # nc_out <- nc_open(file_out, write = T)
  
  # i_date_loop <- seq(1, length(dates_rcm), by = 90)[rep(c(T,F), c(4, 20))]
  # i_date_loop <- seq(1, length(dates_loop), by = 90)[rep(c(T,F), c(4, 20))]
  i_date_loop <- seq_along(dates_loop)[rep(c(T,F), 365*c(1,9))][rep(c(T,F), c(7, 83))]
  # dates_rcm[i_date_loop]
  # dates_loop[i_date_loop]
  
  for(i_date in i_date_loop){
    
    i_rcm <- mapped_times[dates_loop[i_date] == dates_full, idx_pcict] # for non-standard cal
    vals_rcm <- mat_rcm[, i_rcm]
    
    if(i_var == "pr"){
      vals_rcm <- vals_rcm*24*3600
    } else {
      vals_rcm <- vals_rcm-273.15
    }
    
    if(i_var == "pr" & length(which(vals_rcm > pr_min_nonzero_th)) < pr_min_nonzero) next
    
    gm_family <- if(i_var == "pr") tw() else gaussian()
    
    dt_rcm <- cbind(dt_rcm_orog, vals_rcm)
    
    

## fit models --------------------------------------------------------------

    
    gm_fit1 <- gam(vals_rcm ~ s(x, y, k = kk_basis^2),
                  family = gm_family,
                  data = dt_rcm)
    
    gm_fit2 <- gam(vals_rcm ~ s(x, y, k = kk_basis^2) + orog,
                   family = gm_family,
                   data = dt_rcm)
    
    gm_fit3 <- gam(vals_rcm ~ s(x, y, k = kk_basis^2) + s(orog, k = kk_basis),
                   family = gm_family,
                   data = dt_rcm)
    
    gm_fit4 <- gam(vals_rcm ~ te(x, y, orog, k = kk_basis),
                  family = gm_family,
                  data = dt_rcm)
    
    vals_out1 <- predict(gm_fit1, dt_obs_orog, type = "response")
    vals_out2 <- predict(gm_fit2, dt_obs_orog, type = "response")
    vals_out3 <- predict(gm_fit3, dt_obs_orog, type = "response")
    vals_out4 <- predict(gm_fit4, dt_obs_orog, type = "response")
    
    vals_out1 <- f_cons(vals_out1, ratio = i_var == "pr")
    vals_out2 <- f_cons(vals_out2, ratio = i_var == "pr")
    vals_out3 <- f_cons(vals_out3, ratio = i_var == "pr")
    vals_out4 <- f_cons(vals_out4, ratio = i_var == "pr")
    
    
    ## plot fields --------------------------------------------------------------
    
    dt_obs <- dt_obs_orog %>% cbind(vals_out1, vals_out2, vals_out3, vals_out4)
    
    lim_cols <- range(vals_rcm, vals_out1, vals_out2, vals_out3, vals_out4, na.rm = T)
    lim_x <- range(dt_rcm$x, dt_obs$x)
    lim_y <- range(dt_rcm$y, dt_obs$y)
    
    gg0 <- ggplot(dt_rcm, aes(x,y,fill = vals_rcm))+
      geom_raster()+
      scale_fill_viridis_c(i_var, limits = lim_cols, direction = -1, option = "A")+
      xlim(lim_x)+ylim(lim_y)+
      theme_bw()+
      # coord_fixed()+
      xlab(NULL)+ylab(NULL)+
      ggtitle("RCM", 
              sprintf("(%0.1f, %0.1f)", min(vals_rcm, na.rm = T), max(vals_rcm, na.rm = T)))
    gg1 <- ggplot(dt_obs[!is.na(vals_out1)], aes(x,y,fill = vals_out1))+
      geom_raster()+
      scale_fill_viridis_c(i_var, limits = lim_cols, direction = -1, option = "A")+
      xlim(lim_x)+ylim(lim_y)+
      theme_bw()+
      # coord_fixed()+
      xlab(NULL)+ylab(NULL)+
      ggtitle("DS1 s(x,y)",
              sprintf("(%0.1f, %0.1f)", min(vals_out1, na.rm = T), max(vals_out1, na.rm = T)))
    gg2 <- ggplot(dt_obs[!is.na(vals_out2)], aes(x,y,fill = vals_out2))+
      geom_raster()+
      scale_fill_viridis_c(i_var, limits = lim_cols, direction = -1, option = "A")+
      xlim(lim_x)+ylim(lim_y)+
      theme_bw()+
      # coord_fixed()+
      xlab(NULL)+ylab(NULL)+
      ggtitle("DS2 s(x,y) + orog",
              sprintf("(%0.1f, %0.1f)", min(vals_out2, na.rm = T), max(vals_out2, na.rm = T)))
    gg3 <- ggplot(dt_obs[!is.na(vals_out3)], aes(x,y,fill = vals_out3))+
      geom_raster()+
      scale_fill_viridis_c(i_var, limits = lim_cols, direction = -1, option = "A")+
      xlim(lim_x)+ylim(lim_y)+
      theme_bw()+
      # coord_fixed()+
      xlab(NULL)+ylab(NULL)+
      ggtitle("DS3 s(x,y)+s(orog)",
              sprintf("(%0.1f, %0.1f)", min(vals_out3, na.rm = T), max(vals_out3, na.rm = T)))
    
    gg4 <- ggplot(dt_obs[!is.na(vals_out4)], aes(x,y,fill = vals_out4))+
      geom_raster()+
      scale_fill_viridis_c(i_var, limits = lim_cols, direction = -1, option = "A")+
      xlim(lim_x)+ylim(lim_y)+
      theme_bw()+
      # coord_fixed()+
      xlab(NULL)+ylab(NULL)+
      ggtitle("DS4 te(x,y,orog)",
              sprintf("(%0.1f, %0.1f)", min(vals_out4, na.rm = T), max(vals_out4, na.rm = T)))
    
    ## plod diff fields --------------------------------------------------------------
    
    gg21 <- ggplot(dt_obs[!is.na(vals_out2) & !is.na(vals_out1)],
                  aes(x,y,fill = vals_out2 - vals_out1))+
      geom_raster()+
      scale_fill_scico("", palette = "vik", midpoint = 0)+
      xlim(lim_x)+ylim(lim_y)+
      theme_bw()+
      # coord_fixed()+
      xlab(NULL)+ylab(NULL)+
      ggtitle("DS2 - DS1")
    
    gg31 <- ggplot(dt_obs[!is.na(vals_out3) & !is.na(vals_out1)],
                  aes(x,y,fill = vals_out3 - vals_out1))+
      geom_raster()+
      scale_fill_scico("", palette = "vik", midpoint = 0)+
      xlim(lim_x)+ylim(lim_y)+
      theme_bw()+
      # coord_fixed()+
      xlab(NULL)+ylab(NULL)+
      ggtitle("DS3 - DS1")
    
    gg41 <- ggplot(dt_obs[!is.na(vals_out4) & !is.na(vals_out1)],
                  aes(x,y,fill = vals_out4 - vals_out1))+
      geom_raster()+
      scale_fill_scico("", palette = "vik", midpoint = 0)+
      xlim(lim_x)+ylim(lim_y)+
      theme_bw()+
      # coord_fixed()+
      xlab(NULL)+ylab(NULL)+
      ggtitle("DS4 - DS1")
    
    gg32 <- ggplot(dt_obs[!is.na(vals_out3) & !is.na(vals_out2)],
                   aes(x,y,fill = vals_out2 - vals_out3))+
      geom_raster()+
      scale_fill_scico("", palette = "vik", midpoint = 0)+
      xlim(lim_x)+ylim(lim_y)+
      theme_bw()+
      # coord_fixed()+
      xlab(NULL)+ylab(NULL)+
      ggtitle("DS3 - DS2")
    
    # gg_out_pred <- gg1 | gg2 | gg3
    
    ## plot smooths --------------------------------------------------------------
    
    gg_smooth1_1 <- plot(sm(getViz(gm_fit1), 1))+l_fitRaster()
    gg_out_smooths1 <- gg_smooth1_1$ggObj 
    
    
    gg_smooth2_1 <- plot(sm(getViz(gm_fit2), 1))+l_fitRaster()
    dt_pred <- data.table(x = mean(dt_obs_orog$x), y = mean(dt_obs_orog$y), 
                          orog = seq(min(dt_obs_orog$orog), max(dt_obs_orog$orog), length.out = 100))
    dt_pred[, fit := predict(gm_fit2, dt_pred)]
    gg_smooth2_2 <- ggplot(dt_pred, aes(orog, fit))+
      geom_line()+
      theme_bw()
    gg_out_smooths2 <- gg_smooth2_1$ggObj | gg_smooth2_2
    
   
    gg_smooth3_1 <- plot(sm(getViz(gm_fit3), 1))+l_fitRaster()
    gg_smooth3_2 <- plot(sm(getViz(gm_fit3), 2))+l_ciLine()+l_fitLine()
    gg_out_smooths3 <- gg_smooth3_2$ggObj | gg_smooth3_1$ggObj

    pl <- plotSlice(sm(getViz(gm_fit4), 1), 
                    fix = list(orog = c(500, 1500, 2500)),
                    a.facet = list(nrow = 1))
    gg_smooth4_1 <- pl+
      l_fitRaster()+
      coord_fixed()
    
    gg_out_check1 <- check(getViz(gm_fit1))
    gg_out_check2 <- check(getViz(gm_fit2))
    gg_out_check3 <- check(getViz(gm_fit3))
    gg_out_check4 <- check(getViz(gm_fit4))
    
    
    txt <- paste(i_var, dates_loop[i_date], i_rcm_name, sep = "\n")
 
    gg_final <- ((gg0 | gg1 | gg2 | gg3 | gg4) /
                   (wrap_elements(grid::textGrob(txt)) | gg21 | gg31 | gg41 | gg32) / 
                   (gg_out_smooths1 | gg_smooth2_1$ggObj | gg_smooth2_2 |
                      gg_smooth3_1$ggObj | gg_smooth3_2$ggObj | gg_smooth4_1$ggObj) /
                   (wrap_plots(gg_out_check1[c(1, 4)]) | 
                      wrap_plots(gg_out_check2[c(1, 4)]) | 
                      wrap_plots(gg_out_check3[c(1, 4)]) | 
                      wrap_plots(gg_out_check4[c(1, 4)])) &
                   theme_bw())#+plot_annotation(title = txt)
    
   
    
    print(gg_final)
    
    cat(sprintf("%s - date done: %s", date(), dates_loop[i_date]), "\n")
    
  }
  
  # nc_close(nc_out)
  dev.off()
  
  
  
  
}
