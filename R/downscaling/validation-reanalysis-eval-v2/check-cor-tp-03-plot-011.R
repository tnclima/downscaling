#

library(lubridate)
library(data.table)
setDTthreads(4)
library(fs)
library(purrr)
library(foreach)
library(stringr)
library(ggplot2)
library(scico)
library(eurocordexr)
library(patchwork)


dat_aux_011 <- nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eobs.nc",
                             add_xy = T)
dat_aux_011[, date := NULL]

dat <- readRDS("data/corr-tp-rcm-011.rds")

icell_sub <- dat[ff == "obs" & !is.na(corr_pr_tasmin), unique(icell)]

for(i_var in c("corr_tasmin_tasmax", "corr_pr_tasmin", "corr_pr_tasmax")){
  
  for(i_month in 1:12){
    
    dat_i <- dat[icell %in% icell_sub & month == i_month]
    setnames(dat_i, i_var, "value")
    
    col_lim <- range(dat_i$value, na.rm = T)
    
    
    gg1_corr <- dat_i[ff != "obs"] |> 
      merge(dat_aux_011) |> 
      ggplot(aes(lon, lat, fill = value))+
      geom_raster()+
      facet_grid(ff ~ rcm)+
      theme_bw()+
      scale_fill_scico("corr", palette = "roma", midpoint = 0, limits = col_lim)+
      coord_fixed()
    
    gg2_obs <- dat_i[ff == "obs"] |> 
      merge(dat_aux_011) |> 
      ggplot(aes(lon, lat, fill = value))+
      geom_raster()+
      facet_grid(ff ~ rcm)+
      theme_bw()+
      scale_fill_scico("corr", palette = "roma", midpoint = 0, limits = col_lim)+
      coord_fixed()
    
    
    dat_i_diff <- dat_i[ff == "obs", .(icell, value_obs = value)] |> 
      merge(dat_i[ff != "obs"], by = "icell")

    gg3_diff <- dat_i_diff |> 
      merge(dat_aux_011) |> 
      ggplot(aes(lon, lat, fill = value - value_obs))+
      geom_raster()+
      facet_grid(ff ~ rcm)+
      theme_bw()+
      scale_fill_scico("corr diff", palette = "vik", midpoint = 0)+
      coord_fixed()
    
    gg_out1 <- (gg1_corr | gg2_obs)+
      plot_layout(widths = c(3, 1), guides = "collect")
    
    gg_out <- (gg_out1 / gg3_diff)+
      plot_annotation(title = str_c(i_var, ", ", month.name[i_month]))
    
    
  
    fn_out <- path("fig/corr-tp-011/", 
                   sprintf("%s_%02i_%s.png", i_var, i_month, month.abb[i_month]))
    ggsave(fn_out, gg_out, width = 16, height = 7)
  }
}

