#

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

dat_aux <- eurocordexr::nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc",
                                      add_xy = T)
dat_aux <- dat_aux[!is.na(orog), .(icell, x = longitude, y = latitude, orog)]


# tnaa --------------------------------------------------------------------



# ** ecdf --------------------------------------------------------------------

# not sure if necessary


# ** dist-stat ---------------------------------------------------------------

# not sure if necessary


# ** metrics -----------------------------------------------------------------



dat_ba <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v4/tnaa/metrics/"),
  \(path_ba){
    map(dir_ls(path_ba),
        \(x){
          rcm <- x %>% path_file %>% path_ext_remove
          readRDS(x) %>% 
            cbind(institute_rcm = rcm)
        }) %>% rbindlist(fill = T) %>% cbind(bads = path_file(path_ba))
  }
) %>% rbindlist(fill = T)



dat_plot <- dat_ba %>% 
  melt(measure.vars = c("mae", "bias", "bias_rel", "corr"), variable.name = "metric")


dat_plot[, bads_fct := str_remove(bads, "obs-ds-pcalm-")]
dat_plot$bads_fct %>% unique

dat_plot[, n_pc := str_split_i(bads_fct, "-", 1) |> str_remove("nPC") |> as.numeric()]
dat_plot[, orog_pc1 := ifelse(is.na(str_split_i(bads_fct, "-", 2)), "PC1", "orog")]

for(i_var in c("tasmin", "tasmax", "pr", "hn")){
  
  fn_out <- path("fig/validation-obs-pcalm/v4/tnaa/metrics/",
                 i_var,
                 ext = "png")
  
  gg <-
    dat_plot[variable == i_var] %>% 
    ggplot(aes(n_pc, value, colour = orog_pc1))+
    geom_point()+
    facet_grid(metric ~ season, scales = "free_y")+
    theme_bw()+
    ggtitle(i_var)
  
  ggsave(fn_out, gg, width = 12, height = 6)
  
}




# ** spatcor -----------------------------------------------------------------




dat_ba <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v4/tnaa/spatcor/"),
  \(path_ba){
    map(dir_ls(path_ba),
        \(x){
          rcm <- x %>% path_file %>% path_ext_remove
          readRDS(x) %>% 
            cbind(institute_rcm = rcm)
        }) %>% rbindlist(fill = T) %>% cbind(bads = path_file(path_ba))
  }
) %>% rbindlist(fill = T)


dat_plot <- dat_ba %>% 
  melt(measure.vars = c("spatcor", "spatcor_nonzero"), variable.name = "spatcor")

dat_plot[, bads_fct := str_remove(bads, "obs-ds-pcalm-")]
dat_plot$bads_fct %>% unique

dat_plot[, n_pc := str_split_i(bads_fct, "-", 1) |> str_remove("nPC") |> as.numeric()]
dat_plot[, orog_pc1 := ifelse(is.na(str_split_i(bads_fct, "-", 2)), "PC1", "orog")]



for(i_var in c("tasmin", "tasmax", "pr", "hn")){
  
  fn_out <- path("fig/validation-obs-pcalm/v4/tnaa/spatcor/",
                 i_var,
                 ext = "png")
  
  gg <-
    dat_plot[variable == i_var] %>% 
    ggplot(aes(n_pc, value, colour = orog_pc1))+
    geom_point()+
    facet_grid(spatcor ~ season, scales = "free_y")+
    theme_bw()+
    ggtitle(i_var)
  
  ggsave(fn_out, gg, width = 12, height = 6)
  
}




# elev --------------------------------------------------------------------


# todo, if needed



# icell -------------------------------------------------------------------


# ** mean-pctl ---------------------------------------------------------------

dat_crespi <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/icell/mean-pctl/crespi.rds")

dat_ba <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v4/icell/mean-pctl/"),
  \(path_ba){
    map(dir_ls(path_ba),
        \(x){
          rcm <- x %>% path_file %>% path_ext_remove
          readRDS(x) %>% 
            cbind(institute_rcm = rcm)
        }) %>% rbindlist(fill = T) %>% cbind(bads = path_file(path_ba))
  }
) %>% rbindlist(fill = T)


for(i_var in c("tasmin", "tasmax", "pr", "hn")){
  
  
  dat_plot_crespi <- dat_crespi %>%  
    melt(id.vars = c("season", "icell"),
         measure.vars = patterns(str_c("^", i_var)),
         variable.name = "variable_pctl") %>% 
    .[, c("variable", "pctl") := tstrsplit(variable_pctl, "_", keep = c(1,3))]
  setnames(dat_plot_crespi, "value", "value_crespi")
  
  dat_plot <-
    dat_ba %>% 
    melt(id.vars = c("season", "icell", "bads"),
         measure.vars = patterns(str_c("^", i_var)),
         variable.name = "variable_pctl") %>% 
    .[, c("variable", "pctl") := tstrsplit(variable_pctl, "_")] %>% 
    merge(dat_plot_crespi, by = c("season", "icell", "variable", "pctl"))
  
  dat_plot[, bads_fct := str_remove(bads, "obs-ds-pcalm-")]
  # dat_plot$bads_fct %>% unique
  dat_plot[, n_pc := str_split_i(bads_fct, "-", 1) |> str_remove("nPC") |> as.numeric()]
  dat_plot[, orog_pc1 := ifelse(is.na(str_split_i(bads_fct, "-", 2)), "PC1", "orog")]
  
  
  
  if(!i_var %in% c("tasmin", "tasmax")) dat_plot <- dat_plot[!is.na(value)]
  
  if(i_var  %in% c("tasmin", "tasmax")){
    dat_plot[, pctl_ff := fct_relevel(pctl, str_c("p", c(0,1,5,50,95,99,100)))]
  } else {
    dat_plot[, pctl_ff := fct_relevel(pctl, "mean", "p95", "p99", "p100")]
  }
  
  for(i_seas in levels(dat_plot$season)){
    
    if(i_var %in% c("tasmin", "tasmax")){
      
      for(i_pctl in unique(dat_plot$pctl_ff)){
      
        gg1 <-
        dat_plot[season == i_seas & pctl_ff == i_pctl] %>% 
          merge(dat_aux, by = "icell") %>% 
          ggplot(aes(x,y))+
          facet_grid(orog_pc1 ~ n_pc)+
          theme_bw()+
          coord_fixed()+
          xlab(NULL)+ylab(NULL)+
          geom_raster(aes(fill = value - value_crespi))+
          scale_fill_scico("diff abs", palette = "vik", midpoint = 0, limits = c(-5, 5), oob = scales::oob_squish)
        # scale_fill_scico("diff abs", palette = "vik", midpoint = 0)
      
        # gg1
        
        fn_out <- path("fig/validation-obs-pcalm/v4/icell/mean-pctl/", 
                       str_c(i_var, i_seas, i_pctl, sep = "_"),
                       ext = "png")
        
        ggsave(fn_out,
               gg1,
               width = 18, height = 5)
        
        
      }
      
      
        
      
    } else {
      
      gg1 <-
        dat_plot[season == i_seas & orog_pc1 == "PC1"] %>% 
        merge(dat_aux, by = "icell") %>% 
        ggplot(aes(x,y))+
        facet_grid(pctl_ff ~ n_pc)+
        theme_bw()+
        coord_fixed()+
        xlab(NULL)+ylab(NULL)
      
      if(i_var %in% c("pr")){
        gg1 <- gg1+
          geom_raster(aes(fill = (value - value_crespi)/value_crespi))+
          scale_fill_scico("diff rel", palette = "vik", midpoint = 0, direction = -1, limits = c(-1, 2), oob = scales::oob_squish)
          # scale_fill_scico("diff rel", palette = "vik", midpoint = 0, direction = -1)
      }
      if(i_var %in% c("hn")){
        gg1 <- gg1+
          geom_raster(aes(fill = (value - value_crespi)/value_crespi))+
          scale_fill_scico("diff rel", palette = "vik", midpoint = 0, direction = -1, limits = c(-1, 2), oob = scales::oob_squish)
        # scale_fill_scico("diff rel", palette = "vik", midpoint = 0, direction = -1)
      }
      
      gg1
      
      fn_out <- path("fig/validation-obs-pcalm/v4/icell/mean-pctl/", 
                     str_c(i_var, i_seas, sep = "_"),
                     ext = "png")
      
      ggsave(fn_out,
             gg1,
             width = 18, height = 8)
      
      
    }
    
    
    
  }
  
    
  
}





# ** metrics -----------------------------------------------------------------



dat_ba <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v4/icell/metrics/"),
  \(path_ba){
    map(dir_ls(path_ba),
        \(x){
          rcm <- x %>% path_file %>% path_ext_remove
          readRDS(x) %>% 
            cbind(institute_rcm = rcm)
        }) %>% rbindlist(fill = T) %>% cbind(bads = path_file(path_ba))
  }
) %>% rbindlist(fill = T)

dat_plot <- dat_ba  %>% 
  melt(measure.vars = c("mae", "bias", "bias_rel", "corr"), variable.name = "metric")


dat_plot[, bads_fct := str_remove(bads, "obs-ds-pcalm-")]
# dat_plot$bads_fct %>% unique

dat_plot[, n_pc := str_split_i(bads_fct, "-", 1) |> str_remove("nPC") |> as.numeric()]
dat_plot[, orog_pc1 := ifelse(is.na(str_split_i(bads_fct, "-", 2)), "PC1", "orog")]




for(i_var in c("tasmin", "tasmax", "pr", "hn")){
  
  for(i_metric in levels(dat_plot$metric)){
    
    gg1 <-
      dat_plot[variable == i_var & metric == i_metric] %>% 
      merge(dat_aux, by = "icell") %>% 
      ggplot(aes(x,y, fill = value))+
      geom_raster()+
      facet_grid(season+orog_pc1 ~ n_pc)+
      theme_bw()+
      coord_fixed()+
      xlab(NULL)+ylab(NULL)
    
    if(i_metric == "bias" | i_metric == "bias_rel"){
      gg1 <- gg1+
        scale_fill_scico(i_metric, palette = "vik", midpoint = 0)
    } else {
      gg1 <- gg1+
        scale_fill_viridis_c(i_metric)
    }
    
    # gg1
    
    fn_out <- path("fig/validation-obs-pcalm/v4/icell/metrics/", 
                   str_c(i_var, i_metric, sep = "_"),
                   ext = "png")
    
    ggsave(fn_out,
           gg1,
           width = 16, height = 12)
   
    
  }
  
  
  gg1 <- dat_plot[variable == i_var] |> 
    ggplot(aes(as.factor(n_pc), value, fill = orog_pc1))+
    geom_violin()+
    facet_grid(metric ~ season, scales = "free_y")+
    theme_bw()+
    xlab("# PC")+
    ylab(NULL)
  
  fn_out <- path("fig/validation-obs-pcalm/v4/icell/metrics-summary/", 
                 str_c(i_var, sep = "_"),
                 ext = "png")
  
  ggsave(fn_out,
         gg1,
         width = 12, height = 6)
  
  

    
}
