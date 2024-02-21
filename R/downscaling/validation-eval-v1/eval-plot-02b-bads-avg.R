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

path_in <- "/home/climatedata/downscaling/validation-cv/rdata-summary/eval-bads/"


dat_clim_crespi <- readRDS("/home/climatedata/downscaling/validation-cv/rdata-summary/eval-05-trends-crespi.rds")
# dat_clim_eobs <- readRDS("/home/climatedata/downscaling/validation-cv/rdata-summary/eval-06-trends-eobs.rds")
dat_clim_rcm <- readRDS("/home/climatedata/downscaling/validation-cv/rdata-summary/eval-07-trends-rcm.rds")

dat_icell <- readRDS("/home/climatedata/downscaling/validation-cv/rdata-summary/eval-08-aux-icell-1km-011deg.rds")
dat_aux <- dat_icell[!is.na(orog), .(icell = icell_1km, x, y, orog)]
# dat_aux <- dat_aux[!is.na(orog), .(icell, lon = longitude, lat = latitude, orog)]
# ggplot(dat_aux, aes(x, y, fill = orog))+geom_raster()

setnames(dat_clim_rcm, "icell", "icell_011deg")
dat_clim_rcm_1km <- dat_clim_rcm %>% 
  merge(dat_icell[!is.na(orog), .(icell = icell_1km, icell_011deg)], 
        by = "icell_011deg", allow.cartesian = T)



dat_files <- data.table(filename = dir_ls(path_in))
dat_files[, c("period20", "ba", "centers") := tstrsplit(path_file(filename) %>% path_ext_remove(), "_")]
dat_files


for(i_center in 1:6){
  
  
  dat_files_1mod <- dat_files[centers == i_center]
  
  dat_1mod <- foreach(
    i = 1:nrow(dat_files_1mod),
    .final = rbindlist
  ) %do% {
    dat_i <- readRDS(dat_files_1mod[i, filename])
    dat_i[, .(icell, month, variable, mean_value, p5, p50, p95, wassersteindist, 
              centers, ba, period20)]
  }
  
  
  dat_o <- rbind(
    dat_1mod[, 
             .(icell, month, variable, period20, value = mean_value, ff = str_c("rcm_ba_", ba))],
    dat_clim_crespi[, 
                    .(icell, month, variable, period20, value = mean_value, ff = "crespi")],
    dat_clim_rcm_1km[centers == i_center,
                     .(icell, month, variable, period20, value = mean_value, ff = "rcm_raw")]) %>% 
    merge(dat_aux, by = "icell")
  
  dat_o[, ff_fct := fct_relevel(ff, "crespi", "rcm_raw")]
  
  dat_o2 <- dat_o[, .(value = mean(value)), .(icell, month, variable, ff, x, y, orog, ff_fct)]
  
  for(i_var in c("pr", "tasmin", "tasmax", "hn")){
    
    file_out <- path("fig/validation-eval/02-test-bads-period-avg/",
                     str_c("raw_", i_center, "_", i_var),
                     ext = "png")
    
    gg_raw <-
      dat_o2[variable == i_var] %>% 
      ggplot(aes(x, y, fill = value))+
      geom_raster()+
      scale_fill_viridis_c()+
      facet_grid(ff_fct ~ month)+
      theme_bw()+
      xlab(NULL)+ylab(NULL)
    
    
    
    ggsave(file_out, gg_raw, width = 16, height = 8)
    
    
    # bias before-after ba
    
    file_out <- path("fig/validation-eval/02-test-bads-period-avg/",
                     str_c("bias_", i_center, "_", i_var),
                     ext = "png")
    
    dat_o3 <- dat_o2[ff != "crespi", ] %>% 
      merge(dat_o2[ff == "crespi", .(icell, month, variable, value_crespi = value)])
    
    
    gg_bias <-
      dat_o3[variable == i_var] %>% 
      ggplot(aes(x, y, fill = value - value_crespi))+
      geom_raster()+
      scale_fill_scico("...-crespi", palette = "vik", midpoint = 0)+
      facet_grid(ff_fct ~ month)+
      theme_bw()+
      xlab(NULL)+ylab(NULL)
    
    
    ggsave(file_out, gg_bias, width = 16, height = 8)
    
    
    
  }
  
  
}



