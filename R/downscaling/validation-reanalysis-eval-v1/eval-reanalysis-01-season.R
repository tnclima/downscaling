#

library(data.table)
setDTthreads(4)
library(ggplot2)
library(magrittr)
library(fs)
library(stringr)


# data --------------------------------------------------------------------

dat_crespi <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-crespi-season.rds")
dat_only_ba <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-ba-season.rds")
dat_rcm_raw <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-rcm-raw-season.rds")

dat_crespi[variable %in% c("pr", "hn") & qval < 0, qval := 0]

dat_bads <- dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-season/") %>% 
  lapply(\(i_path){
    dir_ls(i_path) %>% 
      lapply(\(x){
        readRDS(x) %>% 
          cbind(institute_rcm = x %>% path_file() %>% path_ext_remove(),
                bads = i_path %>% path_file)
      }) %>% rbindlist()
  }) %>% rbindlist



# ba only -----------------------------------------------------------------

dat_plot <- dat_rcm_raw %>% 
  cbind(ba = "raw") %>% 
  rbind(dat_only_ba)

dat_crespi


dat_plot[variable == "tasmin" & season == "DJF"] %>% 
  ggplot(aes(qval, pctl))+
  geom_step(aes(colour = ba))+
  geom_step(data = dat_crespi[variable == "tasmin" & season == "DJF"])+
  # facet_grid(season ~ institute_rcm)+
  facet_wrap(~institute_rcm)+
  theme_bw()

dat_plot2 <- dat_plot %>% 
  merge(dat_crespi[, .(season, variable, qval_crespi = qval, pctl)])

dat_plot2[variable == "tasmax" & pctl != 0 & pctl != 1] %>% 
  ggplot(aes(qval, qval_crespi, colour = ba))+
  geom_abline()+
  geom_point()+
  facet_grid(season ~ institute_rcm, scales = "free")+
  theme_bw()

dat_plot2[variable == "pr"] %>% 
  ggplot(aes(qval, qval_crespi, colour = ba))+
  geom_abline()+
  geom_point()+
  scale_x_sqrt()+
  scale_y_sqrt()+
  facet_grid(season ~ institute_rcm, scales = "free")+
  theme_bw()

dat_plot2[variable == "hn"] %>% 
  ggplot(aes(qval, qval_crespi, colour = ba))+
  geom_abline()+
  geom_point()+
  scale_x_sqrt()+
  scale_y_sqrt()+
  facet_grid(season ~ institute_rcm, scales = "free")+
  theme_bw()

# summary ( ks.test style)


dat_ks1 <- dat_plot2[, 
                     .(qval_mae = mean(abs(qval - qval_crespi)),
                       qval_max = max(abs(qval - qval_crespi)),
                       # ks_stat = ks.test(qval, qval_crespi, exact = T)$statistic,
                       ks_stat = twosamples::ks_stat(qval, qval_crespi),
                       ad_stat = twosamples::ad_stat(qval, qval_crespi),
                       cvm_stat = twosamples::cvm_stat(qval, qval_crespi)),
                     .(season, variable, institute_rcm, ba)]

dat_ks2_nolim <- dat_plot2[pctl != 0 & pctl != 1, 
                           .(qval_mae = mean(abs(qval - qval_crespi)),
                             qval_max = max(abs(qval - qval_crespi)),
                             # ks_stat = ks.test(qval, qval_crespi, exact = T)$statistic,
                             ks_stat = twosamples::ks_stat(qval, qval_crespi),
                             ad_stat = twosamples::ad_stat(qval, qval_crespi),
                             cvm_stat = twosamples::cvm_stat(qval, qval_crespi)),
                           .(season, variable, institute_rcm, ba)]

# with(dat_ks2_nolim, plot(qval_max, ks_stat))

# dat_ks1 %>%
dat_ks2_nolim %>%
  ggplot(aes(season, ks_stat, fill = ba))+ # qval_max
  geom_boxplot()+
  facet_wrap(~ variable, scales = "free_y")+
  theme_bw()

dat_ks1 %>% 
# dat_ks2_nolim %>% 
  melt(measure.vars = c("qval_mae", "qval_max", "ks_stat", "ad_stat", "cvm_stat"),
       variable.name = "metric") %>% 
  ggplot(aes(season, value, fill = ba))+ # qval_max
  geom_boxplot()+
  facet_grid(metric ~ variable, scales = "free_y")+
  theme_bw()




# bads ----------------------------------------------------------------------

bads_sub <- c("ba-qdm-ds-gam", "ba-qdm-ds-lr", "ba-qdm-ds-pcalm", "ba-qdm-ds-qdm", "bads-qdm")
bads_sub_hn <- c("ba-qdm-ds-pcalm", "ba-mbcn-ds-pcalm", 
                 "ba-qdm-ds-qdm", "ba-mbcn-ds-qdm", 
                 "bads-qdm", "bads-mbcn")

dat_bads$bads %>% table



dat_bads[variable == "tasmin" & season == "DJF" & institute_rcm == "SMHI-RCA4"] %>% 
  ggplot(aes(qval, pctl))+
  geom_step(colour = "blue")+ #aes(colour = bads)
  geom_step(data = dat_crespi[variable == "tasmin" & season == "DJF"], linetype = "dashed")+
  # facet_grid(season ~ institute_rcm)+
  facet_wrap(~bads)+
  theme_bw()

dat_plot1 <- dat_bads[bads %in% bads_sub] %>% 
  merge(dat_crespi[, .(season, variable, qval_crespi = qval, pctl)])

dat_plot2_hn <- dat_bads[bads %in% bads_sub_hn] %>% 
  merge(dat_crespi[, .(season, variable, qval_crespi = qval, pctl)])

# dat_plot1[variable == "tasmax"] %>%
dat_plot1[variable == "tasmax" & pctl != 0 & pctl != 1] %>%
  ggplot(aes(qval, qval-qval_crespi, colour = bads, shape = bads))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(alpha = 0.7)+
  # geom_line()+
  facet_grid(season ~ institute_rcm, scales = "free")+
  scale_color_brewer(palette = "Set1")+
  theme_bw()


# dat_plot1[variable == "pr"] %>% 
dat_plot1[variable == "pr" & pctl != 1] %>% #
# dat_plot1[variable == "pr" & pctl != 1 & pctl > 0.7] %>% #
  ggplot(aes(qval, qval-qval_crespi, colour = bads, shape = bads))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(alpha = 0.7)+
  # scale_x_sqrt()+
  # scale_y_sqrt()+
  facet_grid(season ~ institute_rcm, scales = "free")+
  scale_color_brewer(palette = "Set1")+
  theme_bw()

# dat_plot2_hn[variable == "hn"] %>% 
# dat_plot2_hn[variable == "hn" & !(pctl == 1 & bads %in% c("ba-mbcn-ds-qdm", "ba-qdm-ds-qdm"))] %>%
dat_plot2_hn[variable == "hn" & pctl != 1] %>% #
  ggplot(aes(qval, qval-qval_crespi, colour = bads))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(alpha = 0.7)+
  # scale_x_sqrt()+
  # scale_y_sqrt()+
  facet_grid(season ~ institute_rcm, scales = "free")+
  scale_color_brewer(palette = "Set1")+
  theme_bw()


# summary


dat_ks1 <- dat_plot1[, 
                     .(qval_mae = mean(abs(qval - qval_crespi)),
                       qval_max = max(abs(qval - qval_crespi)),
                       # ks_stat = ks.test(qval, qval_crespi, exact = T)$statistic,
                       ks_stat = twosamples::ks_stat(qval, qval_crespi),
                       ad_stat = twosamples::ad_stat(qval, qval_crespi),
                       cvm_stat = twosamples::cvm_stat(qval, qval_crespi)),
                     .(season, variable, institute_rcm, bads)]

dat_ks1_nolim <- dat_plot1[pctl != 0 & pctl != 1, 
                           .(qval_mae = mean(abs(qval - qval_crespi)),
                             qval_max = max(abs(qval - qval_crespi)),
                             # ks_stat = ks.test(qval, qval_crespi, exact = T)$statistic,
                             ks_stat = twosamples::ks_stat(qval, qval_crespi),
                             ad_stat = twosamples::ad_stat(qval, qval_crespi),
                             cvm_stat = twosamples::cvm_stat(qval, qval_crespi)),
                           .(season, variable, institute_rcm, bads)]

# dat_ks1[variable != "hn"] %>%
dat_ks1_nolim[variable != "hn"] %>%
  ggplot(aes(season, qval_max, fill = bads))+ # qval_max
  geom_boxplot()+
  facet_wrap(~ variable, scales = "free_y")+
  scale_fill_brewer(palette = "Set1")+
  theme_bw()



# dat_ks1[variable != "hn"] %>%
  dat_ks1_nolim[variable != "hn"] %>%
  melt(measure.vars = c("qval_mae", "qval_max", "ks_stat", "ad_stat", "cvm_stat"),
       variable.name = "metric") %>% 
  ggplot(aes(season, value, fill = bads))+ # qval_max
  geom_boxplot()+
  # facet_grid(metric ~ variable, scales = "free_y")+
  facet_wrap(variable ~ metric, scales = "free_y", ncol = 5)+
  theme_bw()




dat_ks2 <- dat_plot2_hn[, 
                     .(qval_mae = mean(abs(qval - qval_crespi)),
                       qval_max = max(abs(qval - qval_crespi)),
                       # ks_stat = ks.test(qval, qval_crespi, exact = T)$statistic,
                       ks_stat = twosamples::ks_stat(qval, qval_crespi),
                       ad_stat = twosamples::ad_stat(qval, qval_crespi),
                       cvm_stat = twosamples::cvm_stat(qval, qval_crespi)),
                     .(season, variable, institute_rcm, bads)]

dat_ks2_nolim <- dat_plot2_hn[pctl != 0 & pctl != 1, 
                           .(qval_mae = mean(abs(qval - qval_crespi)),
                             qval_max = max(abs(qval - qval_crespi)),
                             # ks_stat = ks.test(qval, qval_crespi, exact = T)$statistic,
                             ks_stat = twosamples::ks_stat(qval, qval_crespi),
                             ad_stat = twosamples::ad_stat(qval, qval_crespi),
                             cvm_stat = twosamples::cvm_stat(qval, qval_crespi)),
                           .(season, variable, institute_rcm, bads)]

# dat_ks2 %>%
dat_ks2_nolim %>%
  ggplot(aes(season, ks_test, fill = bads))+ # qval_max, ks_test
  geom_boxplot()+
  facet_wrap(~ variable, scales = "free_y")+
  scale_fill_brewer(palette = "Set1")+
  theme_bw()




dat_ks2[variable == "hn"] %>%
# dat_ks2_nolim[variable == "hn"] %>%
  .[, bads_multi := str_detect(bads, "mbcn")] %>% 
  .[, bads_ds := bads %>% forcats::fct_recode(
    "ds-pcalm" = "ba-qdm-ds-pcalm",
    "ds-pcalm" = "ba-mbcn-ds-pcalm",
    "ds-qdm" = "ba-qdm-ds-qdm",
    "ds-qdm" = "ba-mbcn-ds-qdm",
    "bads" = "bads-qdm",
    "bads" = "bads-mbcn"   
  )] %>% 
  melt(measure.vars = c("qval_mae", "qval_max", "ks_stat", "ad_stat", "cvm_stat"),
       variable.name = "metric") %>% 
  ggplot(aes(season, value, fill = bads_ds, linetype = bads_multi))+ # qval_max
  geom_boxplot()+
  # facet_grid(metric ~ variable, scales = "free_y")+
  facet_wrap(~ metric, scales = "free_y")+
  theme_bw()
