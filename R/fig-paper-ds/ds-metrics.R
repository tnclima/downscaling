#

library(data.table)
setDTthreads(4)
library(ggplot2)
library(magrittr)
library(fs)
library(stringr)
library(purrr)
library(patchwork)
library(ggh4x)



# data --------------------------------------------------------------------

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
  melt(measure.vars = c("mae", "bias", "bias_rel", "corr"), variable.name = "metric")


dat_plot[, bads_ds := factor(bads_ds, levels = c(
  "bads", "ds-qdm", "ds-qdm2", "ds-pcalm", "ds-lr", "ds-gam"
))]


ylabs <- setNames(
  c("MAE", "Bias", "Bias", "Correlation"),
  c("mae", "bias", "bias_rel", "corr")  
)

dat_plot[, metric2 := ylabs[metric]]
dat_plot[, metric2_fct := factor(metric2)]

dat_plot[, bads_multi_fct := ifelse(bads_multi, "multivariate", "univariate")]


# tasmax ------------------------------------------------------------------

gg <- dat_plot[variable == "tasmax" & bads_multi == F & metric != "bias_rel"] %>% 
  ggplot(aes(bads_ds, value))+
  geom_hline(data = data.frame(yy = 0, metric2_fct = "Bias"),
             aes(yintercept = yy), linetype = "dashed")+
  geom_boxplot()+
  facet_grid(metric2_fct ~ season, scales = "free_y", switch = "y")+
  # scale_y_facet(metric2_fct == "bias", labels = scales::label_percent())+
  theme_bw()+
  theme(strip.placement = "outside", 
        strip.background.y = element_blank(),
        strip.text.y.left = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  xlab(NULL)+
  ylab(NULL)


ggsave("fig/paper-ds/ds-metrics_tasmax.png",
       gg, width = 8, height = 6)



# tasmin ------------------------------------------------------------------


gg <- dat_plot[variable == "tasmin" & bads_multi == F & metric != "bias_rel"] %>% 
  ggplot(aes(bads_ds, value))+
  geom_hline(data = data.frame(yy = 0, metric2_fct = "Bias"),
             aes(yintercept = yy), linetype = "dashed")+
  geom_boxplot()+
  facet_grid(metric2_fct ~ season, scales = "free_y", switch = "y")+
  # scale_y_facet(metric2_fct == "bias", labels = scales::label_percent())+
  theme_bw()+
  theme(strip.placement = "outside", 
        strip.background.y = element_blank(),
        strip.text.y.left = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  xlab(NULL)+
  ylab(NULL)


ggsave("fig/paper-ds/ds-metrics_tasmin.png",
       gg, width = 8, height = 6)


# pr ----------------------------------------------------------------------


gg <- dat_plot[variable == "pr" & bads_multi == F & metric != "bias"] %>% 
  ggplot(aes(bads_ds, value))+
  geom_hline(data = data.frame(yy = 0, metric2_fct = "Bias"),
             aes(yintercept = yy), linetype = "dashed")+
  geom_boxplot()+
  facet_grid(metric2_fct ~ season, scales = "free_y", switch = "y")+
  scale_y_facet(metric2_fct == "Bias", labels = scales::label_percent())+
  theme_bw()+
  theme(strip.placement = "outside", 
        strip.background.y = element_blank(),
        strip.text.y.left = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  xlab(NULL)+
  ylab(NULL)


ggsave("fig/paper-ds/ds-metrics_pr.png",
       gg, width = 8, height = 6)


# hn ----------------------------------------------------------------------


gg <- dat_plot[variable == "hn" & season %in% c("DJF", "MAM") & metric != "bias"] %>% 
  ggplot(aes(bads_ds, value, fill = bads_multi_fct))+
  geom_hline(data = data.frame(yy = 0, metric2_fct = "Bias"),
             aes(yintercept = yy), linetype = "dashed")+
  geom_boxplot()+
  facet_grid(metric2_fct ~ season, scales = "free_y", switch = "y")+
  scale_y_facet(metric2_fct == "Bias", labels = scales::label_percent())+
  scale_fill_brewer(NULL, palette = "Pastel1")+
  theme_bw()+
  theme(strip.placement = "outside", 
        strip.background.y = element_blank(),
        strip.text.y.left = element_text(size = rel(1.2)))+
  xlab(NULL)+
  ylab(NULL)


ggsave("fig/paper-ds/ds-metrics_hn.png",
       gg, width = 8, height = 6)




# numbers -----------------------------------------------------------------


dat_plot[variable == "tasmax" & bads_multi == F & metric == "bias", range(value)]
dat_plot[variable == "tasmax" & bads_multi == F & metric == "corr", mean(value), season]
dat_plot[variable == "tasmax" & bads_multi == F & metric == "mae", mean(value), season]
dat_plot[variable == "pr" & bads_multi == F & metric == "bias_rel" & season == "DJF", mean(value), bads_ds]
dat_plot[variable == "pr" & bads_multi == F & metric == "corr" & season == "DJF", mean(value), bads_ds]
dat_plot[variable == "pr" & bads_multi == F & metric == "bias_rel", mean(value), .(season, bads_ds)]


dat_zz <- dat_plot[variable == "tasmax" & bads_multi == F & bads_ds != "ds-qdm"] %>% 
  dcast(season + bads_ds + institute_rcm ~ metric)

dat_zz <- dat_plot[variable == "tasmax" & bads_multi == F & bads_ds != "ds-qdm" & season == "SON"] %>% 
  dcast(season + bads_ds + institute_rcm ~ metric)

dat_zz <- dat_plot[variable == "tasmin" & bads_multi == F & bads_ds != "ds-qdm" & season == "DJF"] %>% 
  dcast(season + bads_ds + institute_rcm ~ metric)


dat_zz <- dat_plot[variable == "pr" & bads_multi == F & bads_ds != "ds-qdm" & season == "DJF"] %>% 
  dcast(season + bads_ds + institute_rcm ~ metric)
