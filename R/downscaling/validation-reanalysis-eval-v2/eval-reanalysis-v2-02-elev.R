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


# ** raw, ba, crespi -------------------------------------------------------------------
# 
# dat_plot <- rbind(
#   dat_raw %>% cbind(bads = "raw"),
#   dat_ba[bads %in% c("ba-qdm", "ba-mbcn")]
# ) %>% 
#   melt(id.vars = c("season", "elev_fct", "institute_rcm", "bads"),
#        variable.name = "variable_pctl") %>% 
#   .[, c("variable", "pctl") := tstrsplit(variable_pctl, "_")]
# 
# dat_plot_crespi <- dat_crespi %>% 
#   melt(id.vars = c("season", "elev_fct"),
#        variable.name = "variable_pctl") %>% 
#   .[, c("variable", "zz", "pctl") := tstrsplit(variable_pctl, "_")]
# 
# dat_plot[variable == "tasmin" & season == "DJF" & pctl == "p50"] %>% 
#   ggplot(aes(elev_fct, value))+
#   geom_point(aes(colour = bads))+
#   geom_point(data = dat_plot_crespi[variable == "tasmin" & season == "DJF"  & pctl == "p50"])+
#   facet_wrap(~institute_rcm)+
#   theme_bw()



# ecdf --------------------------------------------------------------------


dat_crespi <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/elev/ecdf/crespi.rds")

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



# ** raw, ba, crespi -------------------------------------------------------------------

# dat_plot <- rbind(
#   dat_raw %>% cbind(bads = "raw"),
#   dat_ba[bads %in% c("ba-qdm", "ba-mbcn")]
# )
# 
# dat_plot[variable == "tasmin" & season == "DJF"] %>% 
#   ggplot(aes(qval, pctl))+
#   geom_step(aes(colour = bads))+
#   geom_step(data = dat_crespi[variable == "tasmin_crespi" & season == "DJF"])+
#   facet_grid(elev_fct ~ institute_rcm)+
#   theme_bw()
# 
# dat_plot[variable == "tasmax" & season == "JJA"] %>% 
#   ggplot(aes(qval, pctl))+
#   geom_step(aes(colour = bads))+
#   geom_step(data = dat_crespi[variable == "tasmax_crespi" & season == "JJA"])+
#   facet_grid(elev_fct ~ institute_rcm)+
#   theme_bw()
# 
# dat_plot[variable == "pr" & season == "JJA"] %>% 
#   ggplot(aes(qval, pctl))+
#   geom_step(aes(colour = bads))+
#   geom_step(data = dat_crespi[variable == "pr_crespi" & season == "JJA"])+
#   scale_x_sqrt()+
#   facet_grid(elev_fct ~ institute_rcm)+
#   theme_bw()
# 
# dat_plot[variable == "hn" & season == "MAM"] %>% 
#   ggplot(aes(qval, pctl))+
#   geom_step(aes(colour = bads))+
#   geom_step(data = dat_crespi[variable == "hn_crespi" & season == "MAM"])+
#   scale_x_sqrt()+
#   facet_grid(elev_fct ~ institute_rcm)+
#   theme_bw()


dat_plot <- rbind(
  dat_raw %>% cbind(bads = "raw"),
  dat_ba[bads %in% c("ba-qdm", "ba-mbcn")]
)

for(i_var in c("tasmin", "tasmax", "pr", "hn")){
  
  for(i_seas in levels(dat_plot$season)){
    
    for(i_rcm in unique(dat_plot$institute_rcm)){
      
      fn_out <- path("fig/validation-eval-reanalysis/elev/ba-raw-crespi/",
                     str_c(i_var, i_seas, i_rcm, sep = "_"),
                     ext = "png")
      
      gg <- dat_plot[variable == i_var & season == i_seas & institute_rcm == i_rcm] %>% 
        ggplot(aes(qval, pctl))+
        geom_step(aes(colour = bads))+
        geom_step(data = dat_crespi[elev_fct != "(0,500]" & elev_fct != "(3000,3500]" &
                                      variable == str_c(i_var, "_crespi") & season == i_seas])+
        facet_wrap(~elev_fct, scales = "free_x")+
        theme_bw()+
        ggtitle(str_c(i_var, i_seas, i_rcm, sep = " / "))
      
      if(i_var %in% c("pr", "hn")) gg <- gg+scale_x_sqrt()
      
      ggsave(fn_out, gg, width = 8, height = 4)
      
      
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
    
    fn_out <- path("fig/validation-eval-reanalysis/elev/ecdf-bads/",
                   str_c(i_var, i_seas, sep = "_"),
                   ext = "png")
    
    if(i_var %in% c("tasmin", "tasmax")){
      
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
      
    }
    
    
    if(i_var %in% c("pr", "hn")){
      
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
    }
    
    ggsave(fn_out, gg, width = 20, height = 9)
    
    
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


# ** ba vs raw -----------------------------------------------------------------
# 
# dat_plot <- dat_ba[bads %in% c("ba-mbcn", "ba-qdm")]
# dat_plot[, pval_sig := pval < 0.05]
# 
# dat_plot[variable == "hn"] %>% 
#   ggplot(aes(season, dist_stat, fill = bads))+
#   geom_boxplot()+
#   facet_grid(dist_test ~ elev_fct, scales = "free_y")+
#   theme_bw()


# dat_plot[, .(sig_perc = sum(pval_sig)/.N), .(season, dist_test, variable, bads)] %>% 
#   ggplot(aes(season, sig_perc, colour = bads))+
#   geom_point()+
#   facet_grid(dist_test ~ variable)+
#   theme_bw()

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

