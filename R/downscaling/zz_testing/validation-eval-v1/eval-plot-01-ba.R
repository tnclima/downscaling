# 

# library(eurocordexr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(magrittr)
library(ggplot2)
library(fs)
library(stringr)
library(purrr)
library(foreach)
library(patchwork)
library(forcats)
library(scico)

dat <- readRDS("/home/climatedata/downscaling/validation-cv/rdata-summary-v1/eval-01-ba-only.rds")
dat_clim_rcm <- readRDS("/home/climatedata/downscaling/validation-cv/rdata-summary-v1/eval-07-trends-rcm.rds")

dat_eobs <- readRDS("/home/climatedata/downscaling/validation-cv/rdata-summary-v1/eval-06-trends-eobs.rds")
# dat_icell <- readRDS("/home/climatedata/downscaling/validation-cv/rdata-summary-v1/eval-08-aux-icell-1km-011deg.rds")
# dat_aux <- dat_icell[!is.na(orog), .(icell = icell_1km, x, y, orog)]

dat_aux <- eurocordexr::nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eobs.nc",
                                      add_xy = T)
dat_aux[, date := NULL]
# dat_aux$elevation %>% hist(30)
# dat_aux$elevation %>% summary
# dat_aux[, elev_fct := cut(elevation, breaks = seq(0, 3000, by = 500), dig.lab = 5)]



# raw and bias plots ------------------------------------------------------


for(i_center in 1:6){
  
  
  
  dat_1mod <- dat[centers == i_center]
  
  
  dat_o <- rbind(
    dat_1mod[, 
             .(icell, month, variable, period20, value = mean_value, ff = str_c("rcm_ba_", ba))],
    dat_eobs[, 
             .(icell, month, variable, period20, value = mean_value, ff = "eobs")],
    dat_clim_rcm[centers == i_center,
                 .(icell, month, variable, period20, value = mean_value, ff = "rcm_raw")]) %>% 
    merge(dat_aux, by = "icell")
  
  dat_o[, ff_fct := fct_relevel(ff, "eobs", "rcm_raw")]
  
  dat_o2 <- dat_o[,
                  .(value = mean(value)),
                  .(icell, month, variable, ff, lon, lat, elevation, ff_fct)]
  
  for(i_var in c("pr", "tasmin", "tasmax", "hn")){
    
    file_out <- path("fig/validation-eval/03-test-ba-period-avg/",
                     str_c("raw_", i_center, "_", i_var),
                     ext = "png")
    
    gg_raw <-
      dat_o2[variable == i_var] %>% 
      ggplot(aes(lon, lat, fill = value))+
      geom_raster()+
      scale_fill_viridis_c()+
      facet_grid(ff_fct ~ month)+
      theme_bw()+
      xlab(NULL)+ylab(NULL)
    
    
    
    ggsave(file_out, gg_raw, width = 16, height = 8)
    
    
    # bias before-after ba
    
    file_out <- path("fig/validation-eval/03-test-ba-period-avg/",
                     str_c("bias_", i_center, "_", i_var),
                     ext = "png")
    
    dat_o3 <- dat_o2[ff != "eobs", ] %>% 
      merge(dat_o2[ff == "eobs", .(icell, month, variable, value_eobs = value)])
    
    
    gg_bias <-
      dat_o3[variable == i_var] %>% 
      ggplot(aes(lon, lat, fill = value - value_eobs))+
      geom_raster()+
      scale_fill_scico("...-eobs", palette = "vik", midpoint = 0)+
      facet_grid(ff_fct ~ month)+
      theme_bw()+
      xlab(NULL)+ylab(NULL)
    
    ggsave(file_out, gg_bias, width = 16, height = 8)
    
    file_out <- path("fig/validation-eval/03-test-ba-period-avg/",
                     str_c("bias-noraw_", i_center, "_", i_var),
                     ext = "png")
    
    gg_bias2 <-
      dat_o3[variable == i_var & ff != "rcm_raw"] %>% 
      ggplot(aes(lon, lat, fill = value - value_eobs))+
      geom_raster()+
      scale_fill_scico("...-eobs", palette = "vik", midpoint = 0)+
      facet_grid(ff_fct ~ month)+
      theme_bw()+
      xlab(NULL)+ylab(NULL)
    
    ggsave(file_out, gg_bias2, width = 16, height = 6)
      
    
  }
  
  
}




# adjusted by rcm and eobs ------------------------------------------------






