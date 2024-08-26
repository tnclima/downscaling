#

library(data.table)
setDTthreads(4)
library(ggplot2)
library(magrittr)
library(fs)
library(stringr)
library(purrr)
library(scico)

dat_aux <- eurocordexr::nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc",
                                      add_xy = T)
dat_aux <- dat_aux[!is.na(orog), .(icell, x = longitude, y = latitude, orog)]

dat_aux_011 <- eurocordexr::nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eobs.nc",
                                          add_xy = T)
dat_aux_011[, date := NULL]

# mean-pctl ---------------------------------------------------------------

dat_crespi <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/icell/mean-pctl/crespi.rds")

dat_raw <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/icell/mean-pctl/raw"),
  \(x){
    rcm <- x %>% path_file %>% path_ext_remove
    readRDS(x) %>% 
      cbind(institute_rcm = rcm)
  }
) %>% rbindlist()

dat_ba <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/icell/mean-pctl/", glob = "*/ba*"),
  \(path_ba){
    map(dir_ls(path_ba),
        \(x){
          rcm <- x %>% path_file %>% path_ext_remove
          readRDS(x) %>% 
            cbind(institute_rcm = rcm)
        }) %>% rbindlist(fill = T) %>% cbind(bads = path_file(path_ba))
  }
) %>% rbindlist(fill = T)


# ** ba-raw ---------------------------------------------------------------
# 
# dat_plot <- rbind(
#   dat_raw %>% cbind(bads = "raw"),
#   dat_ba[bads %in% c("ba-qdm", "ba-mbcn")]
# )
# 
# dat_plot[season == "DJF"] %>% 
#   merge(dat_aux_011) %>% 
#   ggplot(aes(lon, lat, fill = tasmin_p50))+
#   geom_raster()+
#   scale_fill_viridis_c()+
#   facet_grid(bads ~ institute_rcm)+
#   coord_fixed()+
#   theme_bw()
# 
# dat_plot[season == "JJA"] %>% 
#   merge(dat_aux_011) %>% 
#   ggplot(aes(lon, lat, fill = tasmax_p50))+
#   geom_raster()+
#   scale_fill_viridis_c()+
#   facet_grid(bads ~ institute_rcm)+
#   coord_fixed()+
#   theme_bw()
# 
# dat_plot[season == "DJF"] %>%
#   merge(dat_aux_011) %>%
#   ggplot(aes(lon, lat, fill = pr_p95))+
#   geom_raster()+
#   # scale_fill_viridis_c(limits = c(0, 5), oob = scales::oob_squish)+
#   facet_grid(bads ~ institute_rcm)+
#   coord_fixed()+
#   theme_bw()



# ** bads tasmin -----------------------------------------------------------------

for(i_var in c("tasmin", "tasmax", "pr", "hn")){
  
  
  dat_plot_crespi <- dat_crespi %>%  
    melt(id.vars = c("season", "icell"),
         measure.vars = patterns(str_c("^", i_var)),
         variable.name = "variable_pctl") %>% 
    .[, c("variable", "zz", "pctl") := tstrsplit(variable_pctl, "_")]
  setnames(dat_plot_crespi, "value", "value_crespi")
  
  dat_plot <-
    dat_ba[!bads %in% c("ba-qdm", "ba-mbcn")] %>% 
    melt(id.vars = c("season", "icell", "institute_rcm", "bads"),
         measure.vars = patterns(str_c("^", i_var)),
         variable.name = "variable_pctl") %>% 
    .[, c("variable", "pctl") := tstrsplit(variable_pctl, "_")] %>% 
    merge(dat_plot_crespi, by = c("season", "icell", "variable", "pctl"))
  
  if(!i_var %in% c("tasmin", "tasmax")) dat_plot <- dat_plot[!is.na(value)]
  
  dat_plot[, bads_multi := str_detect(bads, "mbcn")]
  dat_plot[, bads_multi_chr := ifelse(bads_multi, "multi", "uni")]
  dat_plot[, bads_ds := bads %>% forcats::fct_recode(
    "ds-pcalm" = "ba-qdm-ds-pcalm",
    "ds-pcalm" = "ba-mbcn-ds-pcalm",
    "ds-qdm" = "ba-qdm-ds-qdm",
    "ds-qdm" = "ba-mbcn-ds-qdm",
    "ds-gam" = "ba-qdm-ds-gam",
    "ds-gam" = "ba-mbcn-ds-gam",
    "ds-lr" = "ba-qdm-ds-lr",
    "ds-lr" = "ba-mbcn-ds-lr",
    "bads" = "bads-qdm",
    "bads" = "bads-mbcn"   
  )]
  
  
  
  i_bads_multi <- if(i_var %in% c("tasmin", "tasmax")) c(F) else c(T, F)
  
  all_pctl <- unique(dat_plot$pctl)
  
  for(i_seas in levels(dat_plot$season)){
    
    for(i_pctl in all_pctl){
      
      fn_out <- path("fig/validation-eval-reanalysis/icell/", 
                     i_var,
                     str_c(i_seas, i_pctl, sep = "_"),
                     ext = "png")
      
      gg1 <- dat_plot[season == i_seas & pctl == i_pctl & bads_multi %in% i_bads_multi] %>% 
        merge(dat_aux, by = "icell") %>% 
        ggplot(aes(x,y))+
        facet_grid(bads_ds+bads_multi_chr ~ institute_rcm)+
        theme_bw()+
        coord_fixed()+
        xlab(NULL)+ylab(NULL)+
        ggtitle(str_c(i_var, i_seas, i_pctl, sep = " / "))
      
      if(i_var %in% c("tasmin", "tasmax")){
        gg1 <- gg1+
          geom_raster(aes(fill = value - value_crespi))+
          scale_fill_scico("diff abs", palette = "vik", midpoint = 0, limits = c(-5, 5), oob = scales::oob_squish)
      }
      if(i_var %in% c("pr", "hn")){
        gg1 <- gg1+
          geom_raster(aes(fill = (value - value_crespi)/value_crespi))+
          scale_fill_scico("diff rel", palette = "vik", midpoint = 0, direction = -1,
                           limits = c(-1, 2), oob = scales::oob_squish)
      }
      
      ggsave(fn_out,
             gg1,
             width = 20, height = 8)
      
    }
    
    
    
    
  }
  
  
  
  
}




