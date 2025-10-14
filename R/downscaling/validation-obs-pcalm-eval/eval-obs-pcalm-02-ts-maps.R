# 

library(eurocordexr)
library(data.table)
setDTthreads(4)
library(ggplot2)
library(magrittr)
library(fs)
library(stringr)
library(purrr)
library(scico)
library(forcats)
library(patchwork)
library(foreach)

source("R/functions/snowfall.R")

dat_aux <- nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc",
                         add_xy = T)
dat_aux <- dat_aux[!is.na(orog), .(icell, x = longitude, y = latitude, orog)]

dat_aux_011 <- nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eobs.nc",
                             add_xy = T)
dat_aux_011[, date := NULL]

date_sub <- c("2003-01-01", "2003-12-31") %>% as.Date
date_loop <- seq(date_sub[1], date_sub[2], by = "day")

path_in <- "/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v3/"


l_file_crespi_011 <- list(
  tasmax = "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/tasmax_crespi.nc",
  tasmin = "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/tasmin_crespi.nc",
  pr = "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/pr_crespi.nc"
)

l_file_crespi <- list(
  tasmax = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MaxTemp.nc",
  tasmin = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MinTemp.nc",
  pr = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_Prec.nc"
)


# helper fun --------------------------------------------------------------

f_read <- function(l_files, date1 = i_date1, date2 = i_date2, raw = F){
  
  if("pr" %in% names(l_files)){
    
    dat_pr <- nc_grid_to_dt(l_files$pr, date_range = c(date1, date2))
    setnames(dat_pr, 3, "pr")
    
    dat_tasmin <- nc_grid_to_dt(l_files$tasmin, date_range = c(date1, date2))
    setnames(dat_tasmin, 3, "tasmin")
    
    dat_tasmax <- nc_grid_to_dt(l_files$tasmax, date_range = c(date1, date2))
    setnames(dat_tasmax, 3, "tasmax")
    
    dat_i <- cbind(dat_pr, tasmax = dat_tasmax$tasmax, tasmin = dat_tasmin$tasmin)
    dat_i <- dat_i[!is.na(pr)]
    
    if(raw){
      dat_i[, pr := pr*86400]
      dat_i[, tasmax := tasmax-273.15]
      dat_i[, tasmin := tasmin-273.15]
    }
    
    dat_i[, hn := snowfall(pr, tasmax, tasmin)]
    
  } else {
    
    dat_tasmin <- nc_grid_to_dt(l_files$tasmin, date_range = c(date1, date2))
    setnames(dat_tasmin, 3, "tasmin")
    
    dat_tasmax <- nc_grid_to_dt(l_files$tasmax, date_range = c(date1, date2))
    setnames(dat_tasmax, 3, "tasmax")
    
    dat_i <- cbind(dat_tasmax, tasmin = dat_tasmin$tasmin)
    dat_i <- dat_i[!is.na(tasmax)]
    
  }
  
  
  return(dat_i) 
}



# obs-pcalm ---------------------------------------------------------------


path_bads <- dir_ls(path_in) 

for(i in seq_along(date_loop)){
  
  i_date <- date_loop[i]
  
  # crespi 011
  dat_crespi_011 <- f_read(l_file_crespi_011, i_date, i_date)
  
  # crespi 1km
  dat_crespi <- f_read(l_file_crespi, i_date, i_date)
  # crespi day+1 for pr and hn
  # dat_crespi1 <- f_read(l_file_crespi, i_date + 1, i_date + 1)
  
  # bads
  dat_bads <- foreach(i_path = path_bads) %do% {
    
    files_bads <- dir_ls(i_path)
    i_bads <- path_file(i_path)
    
    files_read <- files_bads %>% sort %>% as.list
    
    if(length(files_read) == 2){
      # no pr
      names(files_read) <- c("tasmax", "tasmin")
      dat_i <- f_read(files_read, i_date, i_date)
      
    } else {
      # with pr
      names(files_read) <- c("pr", "tasmax", "tasmin")
      dat_i <- f_read(files_read, i_date, i_date)
      
    }
    
    cbind(dat_i, bads = i_bads)
  } %>% rbindlist(fill = T)
  
  
  dat_bads[, bads_fct := str_remove(bads, "obs-ds-pcalm-")]
  # dat_bads$bads_fct %>% unique
  dat_bads[, bads_fct := fct_relevel(
    bads_fct,
    "nPC6-nomatchtas", "nPC6", "nPC6-PCupscaled", "nPC3-PCupscaled", "nPC9-PCupscaled"
  )]
  levels(dat_bads$bads_fct) <-  str_replace(levels(dat_bads$bads_fct), "-", "\n")
  
  

  for(i_var in c("pr", "tasmin", "tasmax", "hn")){
    
    fn_out <- path("fig/validation-obs-pcalm/ts-maps/",
                   i_var,
                   str_c(i_date, sep = "_"),
                   ext = "png")
    
    if(file_exists(fn_out)) next
    
    lims_col <- range(dat_bads[[i_var]], dat_crespi_011[[i_var]], dat_crespi[[i_var]],
                      na.rm = T)
    
    gg_bads <- dat_bads %>% 
      merge(dat_aux) %>% 
      ggplot(aes(x, y, fill = !!sym(i_var)))+
      geom_raster()+
      scale_fill_viridis_c(limits = lims_col)+
      facet_grid("ds-pcalm" ~ bads_fct)+
      theme_bw()+
      coord_fixed()+
      xlab(NULL)+ylab(NULL)
    
    gg_011 <- 
      dat_crespi_011 %>% 
      merge(dat_aux_011) %>% 
      ggplot(aes(lon, lat, fill = !!sym(i_var)))+
      geom_raster()+
      scale_fill_viridis_c(limits = lims_col)+
      facet_grid(. ~ "0.11°")+
      theme_bw()+
      coord_fixed()+
      xlab(NULL)+ylab(NULL)
    
    gg_crespi <- dat_crespi %>% 
      merge(dat_aux) %>% 
      ggplot(aes(x, y, fill = !!sym(i_var)))+
      geom_raster()+
      scale_fill_viridis_c(limits = lims_col)+
      facet_grid(. ~ "1km")+
      theme_bw()+
      coord_fixed()+
      xlab(NULL)+ylab(NULL)
    
    dat1 <- dat_bads[, c("bads_fct", "icell", "date", i_var), with = F]
    setnames(dat1, i_var, "value_ds")
    dat2 <- dat_crespi[, c("icell", "date", i_var), with = F]
    setnames(dat2, i_var, "value_obs")
    dat_diff <- merge(dat1, dat2)
    
    gg_diff <-
      dat_diff %>% 
      merge(dat_aux, by = "icell") %>% 
      ggplot(aes(x, y, fill = value_ds - value_obs))+
      geom_raster()+
      scale_fill_scico("diff", palette = "vik", midpoint = 0, direction = -1)+
      facet_grid("ds - obs" ~ bads_fct)+
      theme_bw()+
      coord_fixed()+
      xlab(NULL)+ylab(NULL)
    
    gg_out <- wrap_plots(gg_crespi, gg_011, nrow = 1) %>% 
      wrap_plots(gg_bads, gg_diff, ncol = 1, guides = "collect")
    
    ggsave(fn_out, gg_out, width = 12, height = 6)
    
    
    
  }
  
  
}






