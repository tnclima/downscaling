#

library(data.table)
setDTthreads(4)
library(ggplot2)
library(magrittr)
library(fs)
library(stringr)
library(purrr)



# mean-pctl ---------------------------------------------------------------
# 
# dat_crespi <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/tnaa/mean-pctl/crespi.rds")
# 
# dat_raw <- map(
#   dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/tnaa/mean-pctl/raw"),
#   \(x){
#     rcm <- x %>% path_file %>% path_ext_remove
#     readRDS(x) %>% 
#       cbind(institute_rcm = rcm)
#   }
# ) %>% rbindlist()
# 
# dat_ba <- map(
#   dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/tnaa/mean-pctl/", glob = "*/ba-*"),
#   \(path_ba){
#     map(dir_ls(path_ba),
#         \(x){
#           rcm <- x %>% path_file %>% path_ext_remove
#           readRDS(x) %>% 
#             cbind(institute_rcm = rcm)
#         }) %>% rbindlist(fill = T) %>% cbind(bads = path_file(path_ba))
#   }
# ) %>% rbindlist(fill = T)



# ecdf --------------------------------------------------------------------


dat_crespi <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/tnaa/ecdf/crespi.rds")
dat_crespi_011 <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/tnaa/ecdf/crespi-011.rds")

dat_raw <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/tnaa/ecdf/raw"),
  \(x){
    rcm <- x %>% path_file %>% path_ext_remove
    readRDS(x) %>% 
      cbind(institute_rcm = rcm)
  }
) %>% rbindlist()

dat_ba <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/tnaa/ecdf/", glob = "*/ba*"),
  \(path_ba){
    map(dir_ls(path_ba),
        \(x){
          rcm <- x %>% path_file %>% path_ext_remove
          readRDS(x) %>% 
            cbind(institute_rcm = rcm)
        }) %>% rbindlist(fill = T) %>% cbind(bads = path_file(path_ba))
  }
) %>% rbindlist(fill = T)



# ** ba -------------------------------------------------------------------

dat_plot <- rbind(
  dat_raw %>% cbind(bads = "raw"),
  dat_ba[bads %in% c("ba-qdm", "ba-mbcn")]
) %>% 
  merge(dat_crespi_011[, .(season, qval_crespi = qval, pctl, variable = str_remove(variable, "_crespi"))])


# ecdf standard

for(i_var in c("tasmin", "tasmax", "pr", "hn")){
  
  for(i_seas in levels(dat_plot$season)){
    
    fn_out <- path("fig/validation-eval-reanalysis/tnaa-ba/ecdf-raw-011/",
                   str_c(i_var, i_seas, sep = "_"),
                   ext = "png")
    
    gg <- dat_plot[variable == i_var & season == i_seas] %>% 
      ggplot(aes(qval, pctl))+
      geom_step(aes(colour = bads))+
      geom_step(data = dat_crespi[variable == str_c(i_var, "_crespi") & season == i_seas],
                aes(linetype = "crespi_1km"))+
      geom_step(data = dat_crespi_011[variable == str_c(i_var, "_crespi") & season == i_seas],
                aes(linetype = "crespi_011deg"))+
      facet_wrap(~institute_rcm)+
      theme_bw()+
      ggtitle(str_c(i_var, i_seas, sep = " / "))
    
    if(i_var %in% c("pr", "hn")) gg <- gg+scale_x_sqrt()
    
    ggsave(fn_out, gg, width = 12, height = 6)
    
  }
  
}


# ecdf diff

for(i_var in c("tasmin", "tasmax", "pr", "hn")){
  
  for(i_seas in levels(dat_plot$season)){
    
    if(i_var %in% c("tasmin", "tasmax")){
      
      fn_out <- path("fig/validation-eval-reanalysis/tnaa-ba/ecdf/",
                     str_c(i_var, i_seas, sep = "_"),
                     ext = "png")
      
      gg <- dat_plot[pctl != 0 & pctl != 1 & variable == i_var & season == i_seas] %>%
        ggplot(aes(qval, qval-qval_crespi, colour = bads, shape = bads))+
        geom_hline(yintercept = 0, linetype = "dashed")+
        geom_point(alpha = 0.7)+
        geom_line()+
        facet_wrap( ~ institute_rcm, scales = "free")+
        scale_color_brewer(palette = "Set1")+
        theme_bw()+
        ggtitle(str_c(i_var, i_seas, sep = " / "))
      
      ggsave(fn_out, gg, width = 12, height = 6)
      
    }
    
    
    if(i_var %in% c("pr", "hn")){
      
      # abs
      fn_out <- path("fig/validation-eval-reanalysis/tnaa-ba/ecdf/",
                     str_c(i_var, i_seas, sep = "_"),
                     ext = "png")
      
      gg <- dat_plot[pctl != 1 & variable == i_var & season == i_seas] %>% 
        ggplot(aes(qval, qval-qval_crespi, colour = bads))+
        geom_hline(yintercept = 0, linetype = "dashed")+
        geom_point(alpha = 0.7)+
        geom_line()+
        scale_x_sqrt()+
        facet_wrap( ~ institute_rcm, scales = "free")+
        scale_color_brewer(palette = "Set1")+
        theme_bw()+
        ggtitle(str_c(i_var, i_seas, sep = " / "))
      
      ggsave(fn_out, gg, width = 12, height = 6)
      
      # rel
      fn_out <- path("fig/validation-eval-reanalysis/tnaa-ba/ecdf/",
                     str_c(i_var, "rel", i_seas, sep = "_"),
                     ext = "png")
      
      gg <- dat_plot[pctl != 1 & variable == i_var & season == i_seas & pctl > 0.7] %>% 
        ggplot(aes(qval, (qval-qval_crespi)/qval_crespi, 
                   colour = bads))+
        geom_hline(yintercept = 0, linetype = "dashed")+
        geom_point(alpha = 0.7)+
        geom_line()+
        scale_x_sqrt()+
        scale_y_continuous(labels = scales::label_percent(), limits = c(-1, 2), oob = scales::oob_squish)+
        facet_wrap( ~ institute_rcm)+
        scale_color_brewer(palette = "Set1")+
        theme_bw()+
        ggtitle(str_c(i_var, i_seas, sep = " / "))
      
      ggsave(fn_out, gg, width = 12, height = 6)
      
    }
    
    
    
  }
}




# ** bads --------------------------------------------------------------------

dat_plot <- dat_ba[!bads %in% c("ba-qdm", "ba-mbcn")] %>% 
  merge(dat_crespi[, .(season, qval_crespi = qval, pctl, variable = str_remove(variable, "_crespi"))]) %>% 
  .[, bads_multi := str_detect(bads, "mbcn")] %>% 
  .[, bads_ds := bads %>% forcats::fct_recode(
    "ds-pcalm" = "ba-qdm-ds-pcalm",
    "ds-pcalm" = "ba-mbcn-ds-pcalm",
    "ds-qdm" = "ba-qdm-ds-qdm",
    "ds-qdm" = "ba-mbcn-ds-qdm",
    "ds-qdm2" = "ba-qdm-ds-qdm2",
    "ds-qdm2" = "ba-mbcn-ds-qdm2",
    "ds-gam" = "ba-qdm-ds-gam",
    "ds-gam" = "ba-mbcn-ds-gam",
    "ds-lr" = "ba-qdm-ds-lr",
    "ds-lr" = "ba-mbcn-ds-lr",
    "bads" = "bads-qdm",
    "bads" = "bads-mbcn"   
  )]

for(i_var in c("tasmin", "tasmax", "pr", "hn")){
  
  for(i_seas in levels(dat_plot$season)){
 
    if(i_var %in% c("tasmin", "tasmax")){

      fn_out <- path("fig/validation-eval-reanalysis/tnaa/ecdf-bads/",
                     str_c(i_var, i_seas, sep = "_"),
                     ext = "png")
            
      gg <- dat_plot[bads_multi == F & pctl != 0 & pctl != 1 & variable == i_var & season == i_seas] %>%
        ggplot(aes(qval, qval-qval_crespi, colour = bads_ds, shape = bads_ds))+
        geom_hline(yintercept = 0, linetype = "dashed")+
        geom_point(alpha = 0.7)+
        geom_line()+
        facet_wrap( ~ institute_rcm, scales = "free")+
        scale_color_brewer(palette = "Set1")+
        theme_bw()+
        ggtitle(str_c(i_var, i_seas, sep = " / "))
      
      ggsave(fn_out, gg, width = 12, height = 6)
      
    }
    
    
    if(i_var %in% c("pr", "hn")){
      
      # abs
      fn_out <- path("fig/validation-eval-reanalysis/tnaa/ecdf-bads/",
                     str_c(i_var, i_seas, sep = "_"),
                     ext = "png")
      
      gg <- dat_plot[pctl != 1 & variable == i_var & season == i_seas] %>% 
        ggplot(aes(qval, qval-qval_crespi, colour = bads_ds, shape = bads_multi, linetype = bads_multi))+
        geom_hline(yintercept = 0, linetype = "dashed")+
        geom_point(alpha = 0.7)+
        geom_line()+
        scale_x_sqrt()+
        facet_wrap( ~ institute_rcm, scales = "free")+
        scale_color_brewer(palette = "Set1")+
        theme_bw()+
        ggtitle(str_c(i_var, i_seas, sep = " / "))
      
      ggsave(fn_out, gg, width = 12, height = 6)
      
      # rel
      fn_out <- path("fig/validation-eval-reanalysis/tnaa/ecdf-bads/",
                     str_c(i_var, "rel", i_seas, sep = "_"),
                     ext = "png")
      
      gg <- dat_plot[pctl != 1 & variable == i_var & season == i_seas & pctl > 0.7] %>% 
        ggplot(aes(qval, (qval-qval_crespi)/qval_crespi, 
                   colour = bads_ds, shape = bads_multi, linetype = bads_multi))+
        geom_hline(yintercept = 0, linetype = "dashed")+
        geom_point(alpha = 0.7)+
        geom_line()+
        scale_x_sqrt()+
        scale_y_continuous(labels = scales::label_percent(), limits = c(-1, 2), oob = scales::oob_squish)+
        facet_wrap( ~ institute_rcm)+
        scale_color_brewer(palette = "Set1")+
        theme_bw()+
        ggtitle(str_c(i_var, i_seas, sep = " / "))
      
      ggsave(fn_out, gg, width = 12, height = 6)
      
    }
    
    
    
  }
}



# dist-stat ---------------------------------------------------------------

dat_ba <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/tnaa/dist-stat/", glob = "*/ba*"),
  \(path_ba){
    map(dir_ls(path_ba),
        \(x){
          rcm <- x %>% path_file %>% path_ext_remove
          readRDS(x) %>% 
            cbind(institute_rcm = rcm)
        }) %>% rbindlist(fill = T) %>% cbind(bads = path_file(path_ba))
  }
) %>% rbindlist(fill = T)


# ** ba -----------------------------------------------------------------

dat_plot <- dat_ba[bads %in% c("ba-mbcn", "ba-qdm")]
dat_plot[, pval_sig := pval < 0.05]

for(i_dist_test in unique(dat_plot$dist_test)){
  
  for(i_var in c("tasmin", "tasmax", "pr", "hn")){
    
    fn_out <- path("fig/validation-eval-reanalysis/tnaa-ba/dist-stat/",
                   str_c(i_dist_test, i_var, sep = "-"),
                   ext = "png")
    
    gg <- dat_plot[dist_test == i_dist_test & variable == i_var] %>% 
      ggplot(aes(bads, dist_stat))+
      geom_boxplot()+
      facet_wrap(~ season, scales = "free_y")+
      theme_bw()+
      ggtitle(str_c(i_dist_test, i_var, sep = " / "))
    
    ggsave(fn_out, gg, width = 9, height = 6)
    
  }
  
}




# ** bads -----------------------------------------------------------------


dat_plot <- dat_ba[!bads %in% c("ba-mbcn", "ba-qdm")] %>% 
  .[, bads_multi := str_detect(bads, "mbcn")] %>% 
  .[, bads_ds := bads %>% forcats::fct_recode(
    "ds-pcalm" = "ba-qdm-ds-pcalm",
    "ds-pcalm" = "ba-mbcn-ds-pcalm",
    "ds-qdm" = "ba-qdm-ds-qdm",
    "ds-qdm" = "ba-mbcn-ds-qdm",
    "ds-qdm2" = "ba-qdm-ds-qdm2",
    "ds-qdm2" = "ba-mbcn-ds-qdm2",
    "ds-gam" = "ba-qdm-ds-gam",
    "ds-gam" = "ba-mbcn-ds-gam",
    "ds-lr" = "ba-qdm-ds-lr",
    "ds-lr" = "ba-mbcn-ds-lr",
    "bads" = "bads-qdm",
    "bads" = "bads-mbcn"   
  )]
dat_plot[, pval_sig := pval < 0.05]

for(i_dist_test in unique(dat_plot$dist_test)){
  
  
  for(i_var in c("tasmin", "tasmax", "pr", "hn")){
    
    fn_out <- path("fig/validation-eval-reanalysis/tnaa/dist-stat/",
                   str_c(i_dist_test, i_var, sep = "-"),
                   ext = "png")
    
    gg <- dat_plot[dist_test == i_dist_test & variable == i_var] %>% 
      ggplot(aes(bads_ds, dist_stat, fill = bads_multi))+
      geom_boxplot()+
      facet_wrap(~ season, scales = "free_y")+
      theme_bw()+
      ggtitle(str_c(i_dist_test, i_var, sep = " / "))
    
    ggsave(fn_out, gg, width = 9, height = 6)
    
  }
  
}



# metrics -----------------------------------------------------------------


dat_raw <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/tnaa/metrics/raw"),
  \(x){
    rcm <- x %>% path_file %>% path_ext_remove
    readRDS(x) %>% 
      cbind(institute_rcm = rcm)
  }
) %>% rbindlist()



dat_ba <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/tnaa/metrics/", glob = "*/ba*"),
  \(path_ba){
    map(dir_ls(path_ba),
        \(x){
          rcm <- x %>% path_file %>% path_ext_remove
          readRDS(x) %>% 
            cbind(institute_rcm = rcm)
        }) %>% rbindlist(fill = T) %>% cbind(bads = path_file(path_ba))
  }
) %>% rbindlist(fill = T)


# ** ba -----------------------------------------------------------------

dat_plot <-  rbind(
  dat_raw %>% cbind(bads = "raw"),
  dat_ba[bads %in% c("ba-qdm", "ba-mbcn")]
) %>% 
  melt(measure.vars = c("mae", "bias", "corr"), variable.name = "metric")

for(i_var in c("tasmin", "tasmax", "pr", "hn")){
  
  fn_out <- path("fig/validation-eval-reanalysis/tnaa-ba/metrics-raw/",
                 i_var,
                 ext = "png")
  
  gg <-
    dat_plot[variable == i_var] %>% 
    ggplot(aes(bads, value))+
    geom_boxplot()+
    facet_grid(metric ~ season, scales = "free_y")+
    theme_bw()+
    ggtitle(i_var)
  
  ggsave(fn_out, gg, width = 9, height = 6)
  
}
  


# ** bads -----------------------------------------------------------------


dat_plot <- dat_ba[!bads %in% c("ba-qdm", "ba-mbcn")] %>% 
  .[, bads_multi := str_detect(bads, "mbcn")] %>% 
  .[, bads_ds := bads %>% forcats::fct_recode(
    "ds-pcalm" = "ba-qdm-ds-pcalm",
    "ds-pcalm" = "ba-mbcn-ds-pcalm",
    "ds-qdm" = "ba-qdm-ds-qdm",
    "ds-qdm" = "ba-mbcn-ds-qdm",
    "ds-qdm2" = "ba-qdm-ds-qdm2",
    "ds-qdm2" = "ba-mbcn-ds-qdm2",
    "ds-gam" = "ba-qdm-ds-gam",
    "ds-gam" = "ba-mbcn-ds-gam",
    "ds-lr" = "ba-qdm-ds-lr",
    "ds-lr" = "ba-mbcn-ds-lr",
    "bads" = "bads-qdm",
    "bads" = "bads-mbcn"   
  )] %>% 
  melt(measure.vars = c("mae", "bias", "corr"), variable.name = "metric")




for(i_var in c("tasmin", "tasmax", "pr", "hn")){
  
  fn_out <- path("fig/validation-eval-reanalysis/tnaa/metrics/",
                 i_var,
                 ext = "png")
  
  gg <-
    dat_plot[variable == i_var] %>% 
    ggplot(aes(bads_ds, value, linetype = bads_multi))+
    geom_boxplot()+
    facet_grid(metric ~ season, scales = "free_y")+
    theme_bw()+
    ggtitle(i_var)
  
  ggsave(fn_out, gg, width = 16, height = 6)
  
}


