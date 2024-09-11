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
# dat_crespi <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/elev/mean-pctl/crespi.rds")
# 
# dat_raw <- map(
#   dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/elev/mean-pctl/raw"),
#   \(x){
#     rcm <- x %>% path_file %>% path_ext_remove
#     readRDS(x) %>% 
#       cbind(institute_rcm = rcm)
#   }
# ) %>% rbindlist()
# 
# dat_ba <- map(
#   dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/elev/mean-pctl/", glob = "*/ba-*"),
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


dat_crespi <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/elev/ecdf/crespi.rds")
dat_crespi_011 <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/elev/ecdf/crespi-011.rds")

dat_raw <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/elev/ecdf/raw"),
  \(x){
    rcm <- x %>% path_file %>% path_ext_remove
    readRDS(x) %>% 
      cbind(institute_rcm = rcm)
  }
) %>% rbindlist()

dat_ba <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/elev/ecdf/", glob = "*/ba*"),
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
)

# ecdf standard


for(i_var in c("tasmin", "tasmax", "pr", "hn")){
  
  for(i_seas in levels(dat_plot$season)){
    
    for(i_rcm in unique(dat_plot$institute_rcm)){
      
      fn_out <- path("fig/validation-eval-reanalysis/elev-ba/ecdf-raw-011/",
                     str_c(i_var, i_seas, i_rcm, sep = "_"),
                     ext = "png")
      
      gg <- dat_plot[variable == i_var & season == i_seas & institute_rcm == i_rcm] %>% 
        ggplot(aes(qval, pctl))+
        geom_step(aes(colour = bads))+
        geom_step(data = dat_crespi[elev_fct != "(0,500]" & elev_fct != "(3000,3500]" &
                                      variable == str_c(i_var, "_crespi") & season == i_seas],
                  aes(linetype = "crespi_1km"))+
        geom_step(data = dat_crespi_011[elev_fct != "(0,500]" & elev_fct != "(3000,3500]" &
                                      variable == str_c(i_var, "_crespi") & season == i_seas],
                  aes(linetype = "crespi_011deg"))+
        facet_wrap(~elev_fct, scales = "free_x")+
        theme_bw()+
        ggtitle(str_c(i_var, i_seas, i_rcm, sep = " / "))
      
      if(i_var %in% c("pr", "hn")) gg <- gg+scale_x_sqrt()
      
      ggsave(fn_out, gg, width = 8, height = 4)
      
      
    }
    
  }
  
}


# ecdf diff
dat_plot <- rbind(
  dat_raw %>% cbind(bads = "raw"),
  dat_ba[bads %in% c("ba-qdm", "ba-mbcn")]
) %>% 
  merge(dat_crespi_011[, .(elev_fct, season, qval_crespi = qval, pctl, variable = str_remove(variable, "_crespi"))])

for(i_var in c("tasmin", "tasmax", "pr", "hn")){
  
  for(i_seas in levels(dat_plot$season)){
    
    if(i_var %in% c("tasmin", "tasmax")){
      
      fn_out <- path("fig/validation-eval-reanalysis/elev-ba/ecdf/",
                     str_c(i_var, i_seas, sep = "_"),
                     ext = "png")
      
      gg <- dat_plot[pctl != 0 & pctl != 1 & variable == i_var & season == i_seas &
                       elev_fct != "(2500,3000]"] %>%
        ggplot(aes(qval, qval-qval_crespi, colour = bads, shape = bads))+
        geom_hline(yintercept = 0, linetype = "dashed")+
        geom_point(alpha = 0.7)+
        geom_line()+
        # facet_grid(elev_fct ~ institute_rcm, scales = "free")+
        facet_wrap(elev_fct ~ institute_rcm, scales = "free", nrow = 4)+
        scale_color_brewer(palette = "Set1")+
        theme_bw()+
        ggtitle(str_c(i_var, i_seas, sep = " / "))
      
      ggsave(fn_out, gg, width = 20, height = 9)
      
    }
    
    
    if(i_var %in% c("pr", "hn")){
      
      # abs
      fn_out <- path("fig/validation-eval-reanalysis/elev-ba/ecdf/",
                     str_c(i_var, i_seas, sep = "_"),
                     ext = "png")
      
      gg <- dat_plot[pctl != 1 & variable == i_var & season == i_seas &
                       elev_fct != "(2500,3000]"] %>% 
        ggplot(aes(qval, qval-qval_crespi, colour = bads))+
        geom_hline(yintercept = 0, linetype = "dashed")+
        geom_point(alpha = 0.7)+
        geom_line()+
        scale_x_sqrt()+
        facet_wrap(elev_fct ~ institute_rcm, scales = "free", nrow = 4)+
        scale_color_brewer(palette = "Set1")+
        theme_bw()+
        ggtitle(str_c(i_var, i_seas, sep = " / "))
      
      ggsave(fn_out, gg, width = 20, height = 9)
      
      
      # rel
      
      fn_out <- path("fig/validation-eval-reanalysis/elev-ba/ecdf/",
                     str_c(i_var, "rel", i_seas, sep = "_"),
                     ext = "png")
      
      gg <- dat_plot[pctl != 1 & variable == i_var & season == i_seas &
                       elev_fct != "(2500,3000]" & 
                       pctl > 0.7] %>% 
        ggplot(aes(qval, (qval-qval_crespi)/qval_crespi, colour = bads))+
        geom_hline(yintercept = 0, linetype = "dashed")+
        geom_point(alpha = 0.7)+
        geom_line()+
        scale_x_sqrt()+
        scale_y_continuous(labels = scales::label_percent(), limits = c(-1, 2), oob = scales::oob_squish)+
        facet_grid(elev_fct ~ institute_rcm)+
        scale_color_brewer(palette = "Set1")+
        theme_bw()+
        ggtitle(str_c(i_var, i_seas, sep = " / "))
      
      ggsave(fn_out, gg, width = 20, height = 9)
    }
    
    
    
    
  }
}






# ** bads --------------------------------------------------------------------

dat_plot <- dat_ba[!bads %in% c("ba-qdm", "ba-mbcn")] %>% 
  merge(dat_crespi[, .(elev_fct, season, qval_crespi = qval, pctl, variable = str_remove(variable, "_crespi"))]) %>% 
  .[, bads_multi := str_detect(bads, "mbcn")] %>% 
  .[, bads_ds := bads %>% forcats::fct_recode(
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



for(i_var in c("tasmin", "tasmax", "pr", "hn")){
  
  for(i_seas in levels(dat_plot$season)){
    
    if(i_var %in% c("tasmin", "tasmax")){
      
      fn_out <- path("fig/validation-eval-reanalysis/elev/ecdf-bads/",
                     str_c(i_var, i_seas, sep = "_"),
                     ext = "png")
      
      gg <- dat_plot[bads_multi == F & pctl != 0 & pctl != 1 & variable == i_var & season == i_seas &
                       elev_fct %in% levels(elev_fct)[c(T,F)]] %>%
        ggplot(aes(qval, qval-qval_crespi, colour = bads_ds, shape = bads_ds))+
        geom_hline(yintercept = 0, linetype = "dashed")+
        geom_point(alpha = 0.7)+
        geom_line()+
        # facet_grid(elev_fct ~ institute_rcm, scales = "free")+
        facet_wrap(elev_fct ~ institute_rcm, scales = "free", nrow = 4)+
        scale_color_brewer(palette = "Set1")+
        theme_bw()+
        ggtitle(str_c(i_var, i_seas, sep = " / "))
      
      ggsave(fn_out, gg, width = 20, height = 9)
      
    }
    
    
    if(i_var %in% c("pr", "hn")){
      
      # abs
      
      fn_out <- path("fig/validation-eval-reanalysis/elev/ecdf-bads/",
                     str_c(i_var, i_seas, sep = "_"),
                     ext = "png")
      
      gg <- dat_plot[pctl != 1 & variable == i_var & season == i_seas &
                       elev_fct %in% levels(elev_fct)[c(T,F)]] %>% 
        ggplot(aes(qval, qval-qval_crespi, colour = bads_ds, shape = bads_multi, linetype = bads_multi))+
        geom_hline(yintercept = 0, linetype = "dashed")+
        geom_point(alpha = 0.7)+
        geom_line()+
        scale_x_sqrt()+
        facet_wrap(elev_fct ~ institute_rcm, scales = "free", nrow = 4)+
        scale_color_brewer(palette = "Set1")+
        theme_bw()+
        ggtitle(str_c(i_var, i_seas, sep = " / "))
      
      ggsave(fn_out, gg, width = 20, height = 9)
      
      
      # rel
      fn_out <- path("fig/validation-eval-reanalysis/elev/ecdf-bads/",
                     str_c(i_var, "rel", i_seas, sep = "_"),
                     ext = "png")
      
      gg <- dat_plot[pctl != 1 & variable == i_var & season == i_seas &
                       elev_fct %in% levels(elev_fct)[c(T,F)] &
                       pctl > 0.7] %>% 
        ggplot(aes(qval, (qval-qval_crespi)/qval_crespi, 
                   colour = bads_ds, shape = bads_multi, linetype = bads_multi))+
        geom_hline(yintercept = 0, linetype = "dashed")+
        geom_point(alpha = 0.7)+
        geom_line()+
        scale_x_sqrt()+
        scale_y_continuous(labels = scales::label_percent(), limits = c(-1, 2), oob = scales::oob_squish)+
        facet_grid(elev_fct ~ institute_rcm)+
        scale_color_brewer(palette = "Set1")+
        theme_bw()+
        ggtitle(str_c(i_var, i_seas, sep = " / "))
      
      ggsave(fn_out, gg, width = 20, height = 9)
    }
    
    
    
    
  }
}




# dist-stat ---------------------------------------------------------------

dat_ba <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/elev/dist-stat/", glob = "*/ba*"),
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
    
    fn_out <- path("fig/validation-eval-reanalysis/elev-ba/dist-stat/",
                   str_c(i_dist_test, i_var, sep = "-"),
                   ext = "png")
    
    if(i_var %in% c("tasmin", "tasmax")){
      
      gg <- dat_plot[dist_test == i_dist_test & variable == i_var] %>% 
        ggplot(aes(bads, dist_stat, fill = elev_fct))+
        geom_boxplot()+
        scale_fill_brewer()+
        facet_wrap(~ season, scales = "free_y")+
        # facet_grid(elev_fct ~ season, scales = "free_y")+
        theme_bw()+
        ggtitle(str_c(i_dist_test, i_var, sep = " / "))
      
      ggsave(fn_out, gg, width = 12, height = 6)
      
    }
    
    if(i_var %in% c("pr", "hn")){
      
      gg <- dat_plot[dist_test == i_dist_test & variable == i_var] %>% 
        ggplot(aes(bads, dist_stat, fill = elev_fct))+
        geom_boxplot()+
        scale_fill_brewer()+
        facet_wrap(~ season, scales = "free_y")+
        # facet_grid(elev_fct ~ season, scales = "free_y")+
        theme_bw()+
        ggtitle(str_c(i_dist_test, i_var, sep = " / "))
      
      ggsave(fn_out, gg, width = 12, height = 6)
      
    }
    
    
    
    
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
    
    fn_out <- path("fig/validation-eval-reanalysis/elev/dist-stat/",
                   str_c(i_dist_test, i_var, sep = "-"),
                   ext = "png")
    
    if(i_var %in% c("tasmin", "tasmax")){
      
      gg <- dat_plot[bads_multi == F & dist_test == i_dist_test & variable == i_var & bads_ds != "ds-gam"] %>% 
        ggplot(aes(bads_ds, dist_stat, fill = elev_fct))+
        geom_boxplot()+
        scale_fill_brewer()+
        facet_wrap(~ season, scales = "free_y")+
        # facet_grid(elev_fct ~ season, scales = "free_y")+
        theme_bw()+
        ggtitle(str_c(i_dist_test, i_var, sep = " / "))
      
      ggsave(fn_out, gg, width = 12, height = 6)
      
    }
    
    if(i_var %in% c("pr", "hn")){
      
      gg <- dat_plot[dist_test == i_dist_test & variable == i_var] %>% 
        ggplot(aes(bads_ds, dist_stat, fill = elev_fct, linetype = bads_multi))+
        geom_boxplot()+
        scale_fill_brewer()+
        facet_wrap(~ season, scales = "free_y")+
        # facet_grid(elev_fct ~ season, scales = "free_y")+
        theme_bw()+
        ggtitle(str_c(i_dist_test, i_var, sep = " / "))
      
      ggsave(fn_out, gg, width = 16, height = 8)
      
    }
    
    
    
    
  }
  
}

