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
source("R/functions/lapse_rate_varying.R")
source("R/functions/inv_sub.R")

# settings - variables ----------------------------------------------------

# path_out <- "/home/climatedata/downscaling/zz_temp/loop-01-test-gam/"
path_out <- "fig/test-gam-lr/"
kk_basis <- 5


# inventory ---------------------------------------------------------------

dat_inv <- inv_sub()
dat_inv_loop <- dat_inv[experiment == "rcp85" & variable %in% c("tasmin", "tasmax", "pr")]

# main loop ---------------------------------------------------------------


for(i_inv in 1:nrow(dat_inv_loop)){
  
  i_rcm_name <- dat_inv_loop[i_inv, institute_rcm]
  i_var <- dat_inv_loop[i_inv, variable]
  
  
  file_rcm_orog <- dat_inv[variable == "orog" & institute_rcm == i_rcm_name, list_files[[1]]]
  file_rcm <- dat_inv_loop[i_inv, list_files[[1]]]
  if(i_var == "pr"){
    file_obs_orog <- "/home/climatedata/obs/orography/crespi_lonlat_1km_precipitation.nc"
  } else {
    file_obs_orog <- "/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc"
  }
  
  file_out <- path(path_out,
                   dat_inv_loop[i_inv, 
                                paste(variable, centers, institute_rcm, 
                                      gcm, experiment, sep = "_")],
                   ext = "pdf")
  
  
  if(file_exists(file_out)) next
  
  
  
  
  
  # data --------------------------------------------------------------------
  
  rs_rcm_orog <- rast(file_rcm_orog)
  rs_obs_orog <- rast(file_obs_orog)
  
  cells_obs <- which(!is.na(rs_obs_orog[]))
  cells_obs_na <- which(is.na(rs_obs_orog[]))
  
  rs_rcm <- rast(file_rcm)
  mat_rcm <- values(rs_rcm, mat = T)
  # vals_rcm_orog <- values(rs_rcm_orog, mat = F)
  # vals_obs_orog <- values(rs_obs_orog, mat = F)
  
  dt_rcm_orog <- as.data.table(rs_rcm_orog, xy = T, na.rm = F)
  dt_obs_orog <- as.data.table(rs_obs_orog, xy = T, na.rm = F)
  
  # time stuff --------------------------------------------------------------
  
  # non-standard cal workaround
  nc_rcm <- nc_open(file_rcm)
  raw_times <- ncdf4.helpers::nc.get.time.series(nc_rcm)
  mapped_times <- eurocordexr::map_non_standard_calendar(raw_times)
  dates_rcm <- mapped_times$dates_full
  nc_close(nc_rcm)
  
  
  
  # outfile ---------------------------------------------------------
  
  pdf(file_out, width = 14, height = 9)
  # create_emtpy_netcdf(file_template = file_obs_orog, 
  #                     file_out = file_out, 
  #                     l_varinfo = l_nc_info[[i_var]], 
  #                     date_period = range(dates_rcm),
  #                     overwrite = F)
  
  
  # main proc ---------------------------------------------------------------
  
  # nc_out <- nc_open(file_out, write = T)
  
  i_date_loop <- seq(1, length(dates_rcm), by = 90)[rep(c(T,F), c(4, 20))]
  # dates_rcm[i_date_loop]
  for(i_date in i_date_loop){
    
    i_rcm <- mapped_times[i_date, idx_pcict] # for non-standard cal
    vals_rcm <- mat_rcm[, i_rcm]
    
    if(i_var == "pr"){
      vals_rcm <- vals_rcm*24*3600
    } else {
      vals_rcm <- vals_rcm-273.15
    }
    
    if(length(unique(vals_rcm)) < 10) next
    
    # vals_out <- lapse_rate_varying(dt_rcm_orog, dt_obs_orog, vals_rcm, kk_basis)
    # 
    # ncvar_put(nc_out, varid = i_var, vals = vals_out, 
    #           start = c(1, 1, i_date), count = c(-1, -1, 1))
    
    # nc_sync(nc_out)
    
    gm_family <- if(i_var == "pr") tw() else gaussian()
    
    yj_fit <- bestNormalize::yeojohnson(vals_rcm)
    yj_fit <- bestNormalize::orderNorm(vals_rcm)
    vals_rcm_yj <- predict(yj_fit)
    dt_rcm <- cbind(dt_rcm_orog, vals_rcm, vals_rcm_yj)
    
    gm_fit <- gam(vals_rcm_yj ~ s(x, k = kk_basis) + 
                    s(y, k = kk_basis) + 
                    s(orog, k = kk_basis),
                  family = gm_family,
                  data = dt_rcm)
    
    gm_fit2 <- gam(vals_rcm_yj ~ te(x, y, orog, k = c(kk_basis, kk_basis, 2*kk_basis)),
                  family = gm_family,
                  data = dt_rcm)
    
    # vis.gam(gm_fit2, ticktype="detailed")
    # vis.gam(gm_fit2, c("x", "orog"), ticktype="detailed", theta = 120)
    # vis.gam(gm_fit2, c("y", "orog"), ticktype="detailed", theta = 220)
    
    gm_fit3 <- gam(vals_rcm_yj ~ s(x, y, k = kk_basis^2) + 
                    s(orog, k = kk_basis),
                  family = gm_family,
                  data = dt_rcm)
    
    # vals_out <- predict(gm_fit, dt_obs_orog, type = "response")
    # vals_out2 <- predict(gm_fit2, dt_obs_orog, type = "response")
    # vals_out3 <- predict(gm_fit3, dt_obs_orog, type = "response")
    vals_out <- predict(gm_fit, dt_obs_orog, type = "response") %>% 
      predict(yj_fit, ., inverse = T)
    vals_out2 <- predict(gm_fit2, dt_obs_orog, type = "response") %>% 
      predict(yj_fit, ., inverse = T)
    vals_out3 <- predict(gm_fit3, dt_obs_orog, type = "response") %>% 
      predict(yj_fit, ., inverse = T)
    dt_obs <- dt_obs_orog %>% cbind(vals_out, vals_out2, vals_out3)
    
    lim_cols <- range(vals_rcm, vals_out, vals_out2, vals_out3, na.rm = T)
    lim_x <- range(dt_rcm$x, dt_obs$x)
    lim_y <- range(dt_rcm$y, dt_obs$y)
    
    gg1 <- ggplot(dt_rcm, aes(x,y,fill = vals_rcm))+
      geom_raster()+
      scale_fill_viridis_c(i_var, limits = lim_cols, direction = -1, option = "A")+
      xlim(lim_x)+ylim(lim_y)+
      theme_bw()+
      # coord_fixed()+
      xlab(NULL)+ylab(NULL)+
      ggtitle("RCM", 
              sprintf("(%0.1f, %0.1f)", min(vals_rcm, na.rm = T), max(vals_rcm, na.rm = T)))
    gg2 <- ggplot(dt_obs[!is.na(vals_out)], aes(x,y,fill = vals_out))+
      geom_raster()+
      scale_fill_viridis_c(i_var, limits = lim_cols, direction = -1, option = "A")+
      xlim(lim_x)+ylim(lim_y)+
      theme_bw()+
      # coord_fixed()+
      xlab(NULL)+ylab(NULL)+
      ggtitle("DS1 s(x)+s(y)+s(orog)",
              sprintf("(%0.1f, %0.1f)", min(vals_out, na.rm = T), max(vals_out, na.rm = T)))
    gg3 <- ggplot(dt_obs[!is.na(vals_out2)], aes(x,y,fill = vals_out2))+
      geom_raster()+
      scale_fill_viridis_c(i_var, limits = lim_cols, direction = -1, option = "A")+
      xlim(lim_x)+ylim(lim_y)+
      theme_bw()+
      # coord_fixed()+
      xlab(NULL)+ylab(NULL)+
      ggtitle("DS2 te(x,y,orog)",
              sprintf("(%0.1f, %0.1f)", min(vals_out2, na.rm = T), max(vals_out2, na.rm = T)))
    gg4 <- ggplot(dt_obs[!is.na(vals_out2)], aes(x,y,fill = vals_out3))+
      geom_raster()+
      scale_fill_viridis_c(i_var, limits = lim_cols, direction = -1, option = "A")+
      xlim(lim_x)+ylim(lim_y)+
      theme_bw()+
      # coord_fixed()+
      xlab(NULL)+ylab(NULL)+
      ggtitle("DS3 s(x,y)+s(orog)",
              sprintf("(%0.1f, %0.1f)", min(vals_out3, na.rm = T), max(vals_out3, na.rm = T)))
    
    (gg1 | gg2) / (gg3 | gg4)
    
    gg5 <- ggplot(dt_obs[!is.na(vals_out2) & !is.na(vals_out)],
                  aes(x,y,fill = vals_out2 - vals_out))+
      geom_raster()+
      scale_fill_scico("", palette = "vik", midpoint = 0)+
      xlim(lim_x)+ylim(lim_y)+
      theme_bw()+
      # coord_fixed()+
      xlab(NULL)+ylab(NULL)+
      ggtitle("DS2 - DS1")
    
    gg6 <- ggplot(dt_obs[!is.na(vals_out3) & !is.na(vals_out)],
                  aes(x,y,fill = vals_out3 - vals_out))+
      geom_raster()+
      scale_fill_scico("", palette = "vik", midpoint = 0)+
      xlim(lim_x)+ylim(lim_y)+
      theme_bw()+
      # coord_fixed()+
      xlab(NULL)+ylab(NULL)+
      ggtitle("DS3 - DS1")
    
    gg7 <- ggplot(dt_obs[!is.na(vals_out3) & !is.na(vals_out2)],
                  aes(x,y,fill = vals_out3 - vals_out2))+
      geom_raster()+
      scale_fill_scico("", palette = "vik", midpoint = 0)+
      xlim(lim_x)+ylim(lim_y)+
      theme_bw()+
      # coord_fixed()+
      xlab(NULL)+ylab(NULL)+
      ggtitle("DS3 - DS2")
    
    # gg_out_pred <- gg1 | gg2 | gg3
    
    pl <- plotSlice(sm(getViz(gm_fit2), 1), 
                    fix = list(orog = c(500, 1500, 2500)),
                    a.facet = list(nrow = 1))
    gg_out_smooths2 <- pl+
      l_fitRaster()+
      coord_fixed()
    
    gg_smooth3_1 <- plot(sm(getViz(gm_fit3), 1))+l_fitRaster()
    gg_smooth3_2 <- plot(sm(getViz(gm_fit3), 2))+l_ciLine()+l_fitLine()
    gg_out_smooths3 <- gg_smooth3_2$ggObj | gg_smooth3_1$ggObj

    gg_out_check1 <- check(getViz(gm_fit))
    gg_out_check2 <- check(getViz(gm_fit2))
    gg_out_check3 <- check(getViz(gm_fit3))
    
    dt_visreg <- 
      visreg::visreg(gm_fit, plot = F, type = "conditional") %>%
      lapply(function(vr){
        dt_out <- as.data.table(vr$fit)
        dt_out[, xx := vr$fit[[vr$meta$x]]]
        dt_out[, xx_name := vr$meta$x]
        dt_out
      }) %>% rbindlist()
    
    dt_visreg[, visregLwr := visregLwr - mean(visregFit), xx_name]
    dt_visreg[, visregUpr := visregUpr - mean(visregFit), xx_name]
    dt_visreg[, visregFit := visregFit - mean(visregFit), xx_name]
    gg_out_smooths1 <- dt_visreg %>% 
      ggplot(aes(xx, visregFit))+
      geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.3)+
      geom_line()+
      facet_wrap(~ fct_inorder(xx_name), scales = "free_x")+
      theme_bw()+
      xlab(NULL)+ylab("s(x) / s(y) / s(orog)")
    
    txt <- paste(i_var, dates_rcm[i_date], i_rcm_name, sep = "\n")
    # txt <- paste(i_var, dates_rcm[i_date], i_rcm_name, sep = ", ")
 
    gg_final <- ((gg1 | gg2 | gg3 | gg4) /
                   (wrap_elements(grid::textGrob(txt)) | gg5 | gg6 | gg7) / 
                   (gg_out_smooths1 | gg_out_smooths2$ggObj | gg_out_smooths3) /
                   (wrap_plots(gg_out_check1[c(1, 4)]) | 
                      wrap_plots(gg_out_check2[c(1, 4)]) | 
                      wrap_plots(gg_out_check3[c(1, 4)])) &
                   theme_bw())#+plot_annotation(title = txt)
    
   
    
    print(gg_final)
    
    cat(sprintf("%s - date done: %s", date(), dates_rcm[i_date]), "\n")
    
  }
  
  # nc_close(nc_out)
  dev.off()
  
  
  
  
}
