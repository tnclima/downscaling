#

library(data.table)
setDTthreads(4)
library(ggplot2)
library(magrittr)
library(fs)
library(stringr)


# data --------------------------------------------------------------------

dat_crespi <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-crespi-season-elev.rds")
dat_only_ba <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-ba-season-elev.rds")
dat_rcm_raw <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-rcm-raw-season-elev.rds")

dat_crespi[variable %in% c("pr", "hn") & qval < 0, qval := 0]

dat_bads <- dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-season-elev/") %>% 
  lapply(\(i_path){
    dir_ls(i_path) %>% 
      lapply(\(x){
        readRDS(x) %>% 
          cbind(institute_rcm = x %>% path_file() %>% path_ext_remove(),
                bads = i_path %>% path_file)
      }) %>% rbindlist()
  }) %>% rbindlist



# ba only -----------------------------------------------------------------

# does not work easily since different elevation bands rcm/crespi&ds



# bads ----------------------------------------------------------------------

bads_sub <- c("ba-qdm-ds-gam", "ba-qdm-ds-lr", "ba-qdm-ds-pcalm", "ba-qdm-ds-qdm", "bads-qdm")
bads_sub_hn <- c("ba-qdm-ds-pcalm", "ba-mbcn-ds-pcalm", 
                 "ba-qdm-ds-qdm", "ba-mbcn-ds-qdm", 
                 "bads-qdm", "bads-mbcn")

dat_bads$bads %>% table

dat_bads$elev_fct %>% levels

# sub_elev_fct <- c("(250,500]", "(1000,1250]", "(1750,2000]", "(2500,2750]", "(3250,3500]")
sub_elev_fct <- c("(250,500]", "(1000,1250]", "(1750,2000]", "(2500,2750]")

dat_bads[variable == "tasmin" & season == "DJF" & institute_rcm == "SMHI-RCA4" & 
           elev_fct %in% c("(250,500]", "(1000,1250]", "(1750,2000]", "(2500,2750]", "(3250,3500]")] %>% 
  ggplot(aes(qval, pctl))+
  geom_step(colour = "blue")+ #aes(colour = bads)
  geom_step(data = dat_crespi[variable == "tasmin" & season == "DJF" & 
                                elev_fct %in% c("(250,500]", "(1000,1250]", "(1750,2000]", "(2500,2750]", "(3250,3500]")],
            linetype = "dashed")+
  # facet_grid(season ~ institute_rcm)+
  facet_grid(elev_fct ~ bads, scales = "free_x")+
  theme_bw()

dat_plot1 <- dat_bads[bads %in% bads_sub] %>% 
  merge(dat_crespi[, .(season, variable, elev_fct, qval_crespi = qval, pctl)])

dat_plot2_hn <- dat_bads[bads %in% bads_sub_hn] %>% 
  merge(dat_crespi[, .(season, variable, elev_fct, qval_crespi = qval, pctl)])

dat_plot1[variable == "tasmin"] %>%
  # .[season == "DJF"] %>%
  .[season %in% c("DJF", "JJA")] %>%
  .[elev_fct %in% sub_elev_fct] %>% 
  # .[pctl != 0 & pctl != 1] %>%
  ggplot(aes(qval, qval-qval_crespi, colour = bads, shape = bads))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(alpha = 0.7)+
  # geom_line()+
  facet_grid(institute_rcm ~ season+elev_fct, scales = "free")+
  scale_color_brewer(palette = "Set1")+
  theme_bw()


dat_plot1[variable == "pr"] %>%
  # .[season == "DJF"] %>%
  .[season %in% c("DJF", "JJA")] %>%
  .[elev_fct %in% sub_elev_fct] %>% 
  # .[pctl != 1] %>%
  ggplot(aes(qval, qval-qval_crespi, colour = bads, shape = bads))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(alpha = 0.7)+
  scale_x_sqrt()+
  # scale_y_sqrt()+
  facet_grid(institute_rcm ~ season+elev_fct, scales = "free")+
  scale_color_brewer(palette = "Set1")+
  theme_bw()

dat_plot2_hn[variable == "hn"] %>%
  # .[season == "DJF"] %>%
  .[season %in% c("DJF", "MAM")] %>%
  .[elev_fct %in% sub_elev_fct] %>% 
  .[pctl != 1] %>%
  
  .[, bads_multi := str_detect(bads, "mbcn")] %>% 
  .[, bads_ds := bads %>% forcats::fct_recode(
    "ds-pcalm" = "ba-qdm-ds-pcalm",
    "ds-pcalm" = "ba-mbcn-ds-pcalm",
    "ds-qdm" = "ba-qdm-ds-qdm",
    "ds-qdm" = "ba-mbcn-ds-qdm",
    "bads" = "bads-qdm",
    "bads" = "bads-mbcn"   
  )] %>% 
  
  ggplot(aes(qval, qval-qval_crespi, colour = bads_ds, shape = bads_multi))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(alpha = 0.7)+
  scale_x_sqrt()+
  # scale_y_sqrt()+
  facet_grid(institute_rcm ~ season+elev_fct, scales = "free")+
  scale_color_brewer(palette = "Set1")+
  theme_bw()


# summary


dat_ks1 <- dat_plot1[, 
                     .(qval_mae = mean(abs(qval - qval_crespi)),
                       qval_max = max(abs(qval - qval_crespi)),
                       # ks_stat = ks.test(qval, qval_crespi, exact = T)$statistic,
                       ks_stat = twosamples::ks_stat(qval, qval_crespi),
                       ad_stat = twosamples::ad_stat(qval, qval_crespi),
                       cvm_stat = twosamples::cvm_stat(qval, qval_crespi),
                       wass_stat = twosamples::wass_stat(qval, qval_crespi)),
                     .(season, elev_fct, variable, institute_rcm, bads)]

dat_ks1_nolim <- dat_plot1[pctl != 0 & pctl != 1, 
                           .(qval_mae = mean(abs(qval - qval_crespi)),
                             qval_max = max(abs(qval - qval_crespi)),
                             # ks_stat = ks.test(qval, qval_crespi, exact = T)$statistic,
                             ks_stat = twosamples::ks_stat(qval, qval_crespi),
                             ad_stat = twosamples::ad_stat(qval, qval_crespi),
                             cvm_stat = twosamples::cvm_stat(qval, qval_crespi),
                             wass_stat = twosamples::wass_stat(qval, qval_crespi)),
                           .(season, elev_fct, variable, institute_rcm, bads)]

# dat_ks1[variable != "hn"] %>%
# dat_ks1_nolim[variable != "hn"] %>%
#   .[elev_fct %in% sub_elev_fct] %>%
#   ggplot(aes(season, qval_max, fill = bads))+ #
#   geom_boxplot()+
#   facet_grid(variable ~ elev_fct , scales = "free_y")+
#   scale_fill_brewer(palette = "Set1")+
#   theme_bw()



# dat_ks1[variable != "tasmax"] %>%
dat_ks1_nolim[variable == "pr"] %>%
  .[elev_fct %in% sub_elev_fct] %>%
  melt(measure.vars = c("qval_mae", "qval_max", "ks_stat", "ad_stat", "cvm_stat", "wass_stat"),
       variable.name = "metric") %>% 
  ggplot(aes(season, value, fill = bads))+ # qval_max
  geom_boxplot()+
  # facet_grid(metric ~ variable, scales = "free_y")+
  facet_wrap(metric ~ elev_fct, scales = "free_y", ncol = 4)+
  theme_bw()




dat_ks2 <- dat_plot2_hn[, 
                     .(qval_mae = mean(abs(qval - qval_crespi)),
                       qval_max = max(abs(qval - qval_crespi)),
                       # ks_stat = ks.test(qval, qval_crespi, exact = T)$statistic,
                       ks_stat = twosamples::ks_stat(qval, qval_crespi),
                       ad_stat = twosamples::ad_stat(qval, qval_crespi),
                       cvm_stat = twosamples::cvm_stat(qval, qval_crespi),
                       wass_stat = twosamples::wass_stat(qval, qval_crespi)),
                     .(season, elev_fct, variable, institute_rcm, bads)]

dat_ks2_nolim <- dat_plot2_hn[pctl != 0 & pctl != 1, 
                           .(qval_mae = mean(abs(qval - qval_crespi)),
                             qval_max = max(abs(qval - qval_crespi)),
                             # ks_stat = ks.test(qval, qval_crespi, exact = T)$statistic,
                             ks_stat = twosamples::ks_stat(qval, qval_crespi),
                             ad_stat = twosamples::ad_stat(qval, qval_crespi),
                             cvm_stat = twosamples::cvm_stat(qval, qval_crespi),
                             wass_stat = twosamples::wass_stat(qval, qval_crespi)),
                           .(season, elev_fct, variable, institute_rcm, bads)]

# dat_ks2 %>%
# dat_ks2_nolim %>%
#   ggplot(aes(season, ks_test, fill = bads))+ # qval_max, ks_test
#   geom_boxplot()+
#   facet_wrap(~ variable, scales = "free_y")+
#   scale_fill_brewer(palette = "Set1")+
#   theme_bw()




# dat_ks2[variable == "hn"] %>%
dat_ks2_nolim[variable == "hn"] %>%
  .[elev_fct %in% sub_elev_fct] %>%
  .[season %in% c("DJF", "MAM")] %>% 
  .[, bads_multi := str_detect(bads, "mbcn")] %>% 
  .[, bads_ds := bads %>% forcats::fct_recode(
    "ds-pcalm" = "ba-qdm-ds-pcalm",
    "ds-pcalm" = "ba-mbcn-ds-pcalm",
    "ds-qdm" = "ba-qdm-ds-qdm",
    "ds-qdm" = "ba-mbcn-ds-qdm",
    "bads" = "bads-qdm",
    "bads" = "bads-mbcn"   
  )] %>% 
  melt(measure.vars = c("qval_mae", "qval_max", "ks_stat", "ad_stat", "cvm_stat", "wass_stat"),
       variable.name = "metric") %>% 
  ggplot(aes(elev_fct, value, fill = bads_ds, linetype = bads_multi))+ 
  geom_boxplot()+
  # facet_grid(metric ~ elev_fct, scales = "free_y")+
  # facet_grid(season ~ metric, scales = "free_y")+
  facet_wrap(season ~ metric, scales = "free_y", nrow = 2)+
  theme_bw()
