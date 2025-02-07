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

dat_trend_crespi <- dat_clim_crespi %>% 
  melt(id.vars = c("icell", "month", "variable", "period20"), variable.name = "vv") %>% 
  dcast(... ~ period20, value.var = "value") %>% 
  .[, trend := `2001-2020` - `1981-2000`] %>% 
  dcast(icell + month + variable ~ vv, value.var = "trend")

dat_trend_rcm <- dat_clim_rcm %>% 
  melt(id.vars = c("icell", "month", "variable", "centers", "period20"), variable.name = "vv") %>% 
  dcast(... ~ period20, value.var = "value") %>% 
  .[, trend := `2001-2020` - `1981-2000`] %>% 
  dcast(icell + month + variable + centers ~ vv, value.var = "trend")
setnames(dat_trend_rcm, "icell", "icell_011deg")
dat_trend_rcm_1km <- dat_trend_rcm %>% 
  merge(dat_icell[!is.na(orog), .(icell = icell_1km, icell_011deg)], 
        by = "icell_011deg", allow.cartesian = T)

dat_clim_rcm2 <- dat_clim_rcm %>% 
  dcast(icell + month + variable + centers ~ period20, value.var = "mean_value")
setnames(dat_clim_rcm2, c("1981-2000", "2001-2020"), str_c("rcm_", c("1981_2000", "2001_2020")))
setnames(dat_clim_rcm2, "icell", "icell_011deg")
dat_clim_rcm2_1km <- dat_clim_rcm2 %>% 
  merge(dat_icell[!is.na(orog), .(icell = icell_1km, icell_011deg)], 
        by = "icell_011deg", allow.cartesian = T)

dat_clim_crespi2 <- dat_clim_crespi %>% 
  dcast(icell + month + variable ~ period20, value.var = "mean_value")
setnames(dat_clim_crespi2, c("1981-2000", "2001-2020"), str_c("crespi_", c("1981_2000", "2001_2020")))

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
  
  
  for(i_month in 1:12){
    
    for(i_var in c("pr", "tasmin", "tasmax", "hn")){
      
      file_out <- path("fig/validation-eval/01-test-bads/",
                       str_c(i_center, "_", i_var, "_", i_month),
                       ext = "png")
      
      
      if(file_exists(file_out)) next
      
      gg_raw <- dat_o[month == i_month & variable == i_var] %>% 
        ggplot(aes(x, y, fill = value))+
        geom_raster()+
        scale_fill_viridis_c()+
        facet_grid(period20 ~ ff_fct)+
        theme_bw()+
        xlab(NULL)+ylab(NULL)
      
      # period diff
      dat_o_trend <- dat_o %>% 
        dcast(icell + month + variable + ff_fct + x + y + orog ~ period20, value.var = "value")
      dat_o_trend[, trend := `2001-2020` - `1981-2000`]
      
      gg_raw_trend <- dat_o_trend[month == i_month & variable == i_var] %>% 
        ggplot(aes(x, y, fill = trend))+
        geom_raster()+
        scale_fill_scico("trend", palette = "vik", midpoint = 0)+
        facet_grid(. ~ ff_fct)+
        theme_bw()+
        xlab(NULL)+ylab(NULL)
      
      
      # bias before-after ba
      
      dat_o2 <- rbind(
        dat_1mod[, 
                 .(icell, month, variable, period20, value = mean_value, ff = str_c("rcm_ba_", ba))],
        dat_clim_rcm_1km[centers == i_center,
                         .(icell, month, variable, period20, value = mean_value, ff = "rcm_raw")]) %>% 
        merge(dat_clim_crespi[, 
                              .(icell, month, variable, period20, value_crespi = mean_value)]) %>% 
        merge(dat_aux, by = "icell")
      
      dat_o2[, ff_fct := fct_relevel(ff, "rcm_raw")]
      
      gg_bias <- dat_o2[month == i_month & variable == i_var] %>% 
        ggplot(aes(x, y, fill = value - value_crespi))+
        geom_raster()+
        scale_fill_scico("...-crespi", palette = "vik", midpoint = 0)+
        facet_grid(period20 ~ ff_fct)+
        theme_bw()+
        xlab(NULL)+ylab(NULL)
      
      
      
      # trend diff crespi
      
      dat_o2[, bias := value - value_crespi]
      dat_o2_trend <- dat_o2 %>% 
        dcast(icell + month + variable + ff_fct + x + y + orog ~ period20, value.var = "bias")
      dat_o2_trend[, trend := `2001-2020` - `1981-2000`]
      # dat_o2_trend[, trend := `1981-2000` - `2001-2020`]
      
      gg_bias_trend <- dat_o2_trend[month == i_month & variable == i_var] %>% 
        ggplot(aes(x, y, fill = trend))+
        geom_raster()+
        scale_fill_scico("trend-crespi_trend", palette = "vik", midpoint = 0)+
        facet_grid(. ~ ff_fct)+
        theme_bw()+
        xlab(NULL)+ylab(NULL)
      
      
      # diff between ba*3 (qm - qdm, mbcn - qdm)
      # dat_1mod_diff <- dat_1mod %>% 
      #   dcast(icell + month + variable + centers + period20 ~ ba, value.var = "mean_value") %>% 
      #   merge(dat_aux, by = "icell")
      # 
      # gg_diff1 <- dat_1mod_diff[month == 1 & variable == "tasmax"] %>% 
      #   ggplot(aes(x, y, fill = qm - qdm))+
      #   geom_raster()+
      #   scale_fill_scico("qm-qdm", palette = "vik", midpoint = 0)+
      #   facet_grid(period20 ~ .)+
      #   theme_bw()+
      #   xlab(NULL)+ylab(NULL)
      # 
      # gg_diff2 <- dat_1mod_diff[month == 1 & variable == "tasmax"] %>% 
      #   ggplot(aes(x, y, fill = qdm - mbcn))+
      #   geom_raster()+
      #   scale_fill_scico("qdm-mbcn", palette = "vik", midpoint = 0)+
      #   facet_grid(period20 ~ .)+
      #   theme_bw()+
      #   xlab(NULL)+ylab(NULL)
      
      
      # combine plots
      
      
      # (gg_raw / gg_raw_trend) | (gg_bias / gg_bias_trend)+
      # gg_out <- (gg_raw + gg_raw_trend + gg_bias + gg_bias_trend + gg_diff1 + gg_diff2)+
      #   plot_layout(ncol = 3, heights = c(2,1), byrow = F)
      
      gg_out <- (gg_raw + gg_raw_trend + gg_bias + gg_bias_trend)+
        plot_layout(nrow = 2, heights = c(2,1), byrow = F)
      
      ggsave(file_out, gg_out, width = 16, height = 6)
      
      
      
    }
    
    
  }
  
  
  
}
