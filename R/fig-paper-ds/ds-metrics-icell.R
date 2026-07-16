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
library(scico)
library(forcats)


# data --------------------------------------------------------------------

dat_aux <- eurocordexr::nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc",
                                      add_xy = T)
dat_aux <- dat_aux[!is.na(orog), .(icell, x = longitude, y = latitude, orog)]

path_uni <- dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/icell/metrics/", glob = "*/ba*") %>% 
  str_subset("mbcn", negate = T) %>% 
  str_subset("ba-qdm$", negate = T) %>% 
  str_subset("-ds-qdm$", negate = T)
  
  

dat_ba <- map(
  path_uni,
  \(path_ba){
    map(dir_ls(path_ba),
        \(x){
          rcm <- x %>% path_file %>% path_ext_remove
          readRDS(x) %>% 
            cbind(institute_rcm = rcm)
        }) %>% rbindlist(fill = T) %>% cbind(bads = path_file(path_ba))
  }
) %>% rbindlist(fill = T)



dat_ba <- dat_ba[, bads_ds := bads %>% forcats::fct_recode(
  "ds-pcalm" = "ba-qdm-ds-pcalm",
  # "ds-pcalm" = "ba-mbcn-ds-pcalm",
  # "ds-qdm" = "ba-qdm-ds-qdm",
  # "ds-qdm" = "ba-mbcn-ds-qdm",
  "ds-qdm2" = "ba-qdm-ds-qdm2",
  # "ds-qdm2" = "ba-mbcn-ds-qdm2",
  "ds-gam" = "ba-qdm-ds-gam",
  # "ds-gam" = "ba-mbcn-ds-gam",
  "ds-lr" = "ba-qdm-ds-lr",
  # "ds-lr" = "ba-mbcn-ds-lr",
  "bads" = "bads-qdm"
  # "bads" = "bads-mbcn"   
)]


dat_ba[, bads_ds := factor(bads_ds, levels = c(
  "bads", "ds-qdm", "ds-qdm2", "ds-pcalm", "ds-lr", "ds-gam"
))]


# dat_ba[, bads_multi_fct := ifelse(bads_multi, "multivariate", "univariate")]

dat_ba[, rcm_short := eurocordexr::shortnames_rcm[institute_rcm]]
dat_ba[, institute := str_remove(institute_rcm, rcm_short)]
dat_ba[, institute := str_remove(institute, "-$")]



dat_ba2 <- dat_ba[,
                  lapply(.SD, mean),
                  .(season, icell, variable, bads, bads_ds),
                  .SDcols = c("mae", "bias", "bias_rel", "corr")]

dat_ba2[, bads2 := str_replace(bads, "ba-qdm-", "ba-qdm-\n")]


# add multi for hn
path_multi <- dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/icell/metrics/", glob = "*/ba*") %>% 
  str_subset("mbcn", negate = F) %>% 
  str_subset("ba-mbcn$", negate = T) %>%
  str_subset("-ds-qdm$", negate = T) |> 
  str_subset("-ds-lr$", negate = T) |> 
  str_subset("-ds-gam$", negate = T)
  

dat_ba_multi <- map(
  path_multi,
  \(path_ba){
    map(dir_ls(path_ba),
        \(x){
          rcm <- x %>% path_file %>% path_ext_remove
          readRDS(x) %>% 
            cbind(institute_rcm = rcm)
        }) %>% rbindlist(fill = T) %>% cbind(bads = path_file(path_ba))
  }
) %>% rbindlist(fill = T)



dat_ba_multi[, bads_ds := bads %>% forcats::fct_recode(
  # "ds-pcalm" = "ba-qdm-ds-pcalm",
  "ds-pcalm" = "ba-mbcn-ds-pcalm",
  # "ds-qdm" = "ba-qdm-ds-qdm",
  # "ds-qdm" = "ba-mbcn-ds-qdm",
  # "ds-qdm2" = "ba-qdm-ds-qdm2",
  "ds-qdm2" = "ba-mbcn-ds-qdm2",
  # "ds-gam" = "ba-qdm-ds-gam",
  # "ds-gam" = "ba-mbcn-ds-gam",
  # "ds-lr" = "ba-qdm-ds-lr",
  # "ds-lr" = "ba-mbcn-ds-lr",
  # "bads" = "bads-qdm"
  "bads" = "bads-mbcn"
)]


dat_ba_multi[, bads_ds := factor(bads_ds, levels = c(
  "bads", "ds-qdm", "ds-qdm2", "ds-pcalm", "ds-lr", "ds-gam"
))]


# dat_ba[, bads_multi_fct := ifelse(bads_multi, "multivariate", "univariate")]

dat_ba_multi[, rcm_short := eurocordexr::shortnames_rcm[institute_rcm]]
dat_ba_multi[, institute := str_remove(institute_rcm, rcm_short)]
dat_ba_multi[, institute := str_remove(institute, "-$")]



dat_ba2_multi <- dat_ba_multi[,
                              lapply(.SD, mean),
                              .(season, icell, variable, bads, bads_ds),
                              .SDcols = c("mae", "bias", "bias_rel", "corr")]

dat_ba2_multi[, bads2 := str_replace(bads, "ba-mbcn-", "ba-mbcn-\n")]


dat_ba2_hn <- rbind(
  dat_ba2[variable == "hn"], dat_ba2_multi[variable == "hn"]
)

dat_ba2_hn[, bads2 := factor(bads, levels = c(
  "bads-qdm", "bads-mbcn", "ba-qdm-ds-qdm2", "ba-mbcn-ds-qdm2", 
  "ba-qdm-ds-pcalm", "ba-mbcn-ds-pcalm"
))]


dat_ba2_hn[, bads_multi := str_detect(bads, "mbcn")] 
dat_ba2_hn[, bads_multi_fct := ifelse(bads_multi, "multivariate", "univariate")]


# pr ----------------------------------------------------------------------
# 
# gg <-
# dat_ba[variable == "pr"] %>% 
#   merge(dat_aux, by = "icell") %>% 
#   ggplot(aes(x,y, fill = bias_rel))+
#   geom_raster()+
#   scale_fill_scico("pr bias", 
#                    palette = "vik", 
#                    midpoint = 0, 
#                    labels = scales::label_percent(),
#                    direction = -1)+
#   facet_nested(season + bads_ds ~ institute + rcm_short)+
#   coord_fixed()+
#   theme_bw()+
#   theme(axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         legend.position = "bottom",
#         legend.key.width = unit(1.5, "cm"))+
#   xlab(NULL)+ylab(NULL)
# 
# ggsave("fig/paper-ds/ds-metrics-icell_pr_bias.png",
#        gg, width = 18, height = 14)
# 
# 
# 
# 
# gg <-
#   dat_ba[variable == "pr"] %>% 
#   merge(dat_aux, by = "icell") %>% 
#   ggplot(aes(x,y, fill = corr))+
#   geom_raster()+
#   scale_fill_scico("pr correlation", 
#                    palette = "batlow")+
#   facet_nested(season + bads_ds ~ institute + rcm_short)+
#   coord_fixed()+
#   theme_bw()+
#   theme(axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         legend.position = "bottom",
#         legend.key.width = unit(1.5, "cm"))+
#   xlab(NULL)+ylab(NULL)
# 
# ggsave("fig/paper-ds/ds-metrics-icell_pr_corr.png",
#        gg, width = 18, height = 14)
# 


# tasmin DJF, tasmax JJA --------------------------------------------------

# 
# 
# gg <-
#   dat_ba[bads_ds != "ds-gam"][(variable == "tasmax" & season == "JJA") |
#            (variable == "tasmin" & season == "DJF")] %>% 
#   merge(dat_aux, by = "icell") %>% 
#   ggplot(aes(x,y, fill = bias))+
#   geom_raster()+
#   scale_fill_scico("bias [°C]", 
#                    palette = "vik", 
#                    midpoint = 0, 
#                    # labels = scales::label_percent(),
#                    direction = +1)+
#   facet_nested(variable + season + bads_ds ~ institute + rcm_short)+
#   coord_fixed()+
#   theme_bw()+
#   theme(axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         legend.position = "bottom",
#         legend.key.width = unit(1.5, "cm"))+
#   xlab(NULL)+ylab(NULL)
# 
# ggsave("fig/paper-ds/ds-metrics-icell_tasminmax_bias.png",
#        gg, width = 18, height = 10)
# 
# 
# 
# 
# gg <-
#   dat_ba[bads_ds != "ds-gam"][(variable == "tasmax" & season == "JJA") |
#                                 (variable == "tasmin" & season == "DJF")] %>% 
#   merge(dat_aux, by = "icell") %>% 
#   ggplot(aes(x,y, fill = corr))+
#   geom_raster()+
#   scale_fill_scico("pr correlation", 
#                    palette = "batlow")+
#   facet_nested(variable + season + bads_ds ~ institute + rcm_short)+
#   coord_fixed()+
#   theme_bw()+
#   theme(axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         legend.position = "bottom",
#         legend.key.width = unit(1.5, "cm"))+
#   xlab(NULL)+ylab(NULL)
# 
# ggsave("fig/paper-ds/ds-metrics-icell_tasminmax_corr.png",
#        gg, width = 18, height = 10)
# 



# ensemble avg - pr ------------------------------------------------------------


gg_pr_bias <-
dat_ba2[variable == "pr"] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = bias_rel))+
  geom_raster()+
  scale_fill_scico("pr bias", 
                   palette = "vik", 
                   midpoint = 0, 
                   labels = scales::label_percent(),
                   direction = -1)+
  facet_nested(season ~ bads)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom")+
  xlab(NULL)+ylab(NULL)


gg_pr_mae <-
  dat_ba2[variable == "pr"] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = mae))+
  geom_raster()+
  scale_fill_scico("pr MAE", 
                   palette = "bamako")+
  facet_nested(season ~ bads)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom")+
  xlab(NULL)+ylab(NULL)


gg_pr_corr <-
  dat_ba2[variable == "pr"] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = corr))+
  geom_raster()+
  scale_fill_scico("pr correlation", 
                   palette = "batlow")+
  facet_nested(season ~ bads)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom")+
  xlab(NULL)+ylab(NULL)


gg_pr_out <- (gg_pr_bias+gg_pr_mae+gg_pr_corr)+
  plot_layout(ncol = 2)+
  plot_annotation(tag_level = "a", tag_suffix = ")")


ggsave(filename = "fig/paper-ds-pdf/ds-metrics-icell_ensavg_pr.pdf",
       gg_pr_out,
       width = 10, height = 10)



# ensemble avg - tasmin/max ------------------------------------------------------------

gg_tas_bias <-
  dat_ba2[variable %in% c("tasmin", "tasmax")] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = bias))+
  geom_raster()+
  scale_fill_scico("bias [°C]", 
                   palette = "vik", 
                   midpoint = 0, 
                   direction = +1,
                   limits = c(-2,2),
                   oob = scales::oob_squish)+
  facet_nested(bads2 ~ variable + season)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())+
  xlab(NULL)+ylab(NULL)


gg_tas_mae <-
  dat_ba2[variable %in% c("tasmin", "tasmax")] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = mae))+
  geom_raster()+
  scale_fill_scico("MAE [°C]", 
                   palette = "bamako",
                   limits = c(NA,5),
                   oob = scales::oob_squish)+
  facet_nested(bads2 ~ variable + season)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())+
  xlab(NULL)+ylab(NULL)


gg_tas_corr <-
  dat_ba2[variable %in% c("tasmin", "tasmax")] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = corr))+
  geom_raster()+
  scale_fill_scico("correlation", 
                   palette = "batlow")+
  # limits = c(-2,2),
  # oob = scales::oob_squish)+
  facet_nested(bads2 ~ variable + season)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())+
  xlab(NULL)+ylab(NULL)


gg_tas_out <- (gg_tas_bias+gg_tas_mae+gg_tas_corr)+
  plot_layout(ncol = 1)+
  plot_annotation(tag_level = "a", tag_suffix = ")")



ggsave(filename = "fig/paper-ds-pdf/ds-metrics-icell_ensavg_tasminmax.pdf",
       gg_tas_out,
       width = 12, height = 13)




# ensemble avg - hn ------------------------------------------------------------


gg_hn_bias_rel <-
  dat_ba2_hn[season %in% c("DJF", "MAM")] %>%
  merge(dat_aux, by = "icell") %>%
  ggplot(aes(x,y, fill = bias_rel))+
  geom_raster()+
  scale_fill_scico("hn bias",
                   palette = "vik",
                   midpoint = 0,
                   limits = c(NA, 1),
                   oob = scales::oob_squish,
                   labels = scales::label_percent(),
                   direction = -1)+
  facet_nested(season ~ bads2)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom")+
  xlab(NULL)+ylab(NULL)



gg_hn_bias <-
dat_ba2_hn[season %in% c("DJF", "MAM")] %>%
  merge(dat_aux, by = "icell") %>%
  ggplot(aes(x,y, fill = bias))+
  geom_raster()+
  scale_fill_scico("hn bias [cm/day]",
                   palette = "vik",
                   midpoint = 0,
                   # limits = c(NA, 1),
                   # oob = scales::oob_squish,
                   # labels = scales::label_percent(),
                   direction = -1)+
  facet_nested(season ~ bads2)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom")+
  xlab(NULL)+ylab(NULL)


gg_hn_mae <-
  dat_ba2_hn[season %in% c("DJF", "MAM")] %>%
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = mae))+
  geom_raster()+
  scale_fill_scico("hn MAE", 
                   palette = "bamako")+
  facet_nested(season ~ bads2)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom")+
  xlab(NULL)+ylab(NULL)


gg_hn_corr <-
  dat_ba2_hn[season %in% c("DJF", "MAM")] %>%
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = corr))+
  geom_raster()+
  scale_fill_scico("hn correlation", 
                   palette = "batlow")+
  facet_nested(season ~ bads2)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom")+
  xlab(NULL)+ylab(NULL)


gg_hn_out <- (gg_hn_bias_rel+gg_hn_bias+gg_hn_mae+gg_hn_corr)+
  plot_layout(ncol = 1)+
  plot_annotation(tag_level = "a", tag_suffix = ")")


ggsave(filename = "fig/paper-ds-pdf/ds-metrics-icell_ensavg_hn.pdf",
       gg_hn_out,
       width = 10, height = 14)


# hn violin ---------------------------------------------------------------

dat_plot_hn_violin <- dat_ba2_hn[season %in% c("DJF", "MAM")] |> 
  # melt(measure.vars = c("bias", "bias_rel", "corr", "mae"),
  melt(measure.vars = c("bias_rel", "corr", "mae"),
       variable.name = "metric",
       variable.factor = F)
  

# ylabs <- setNames(
#   c("MAE", "Bias [cm/day]", "Bias (relative)", "Correlation"),
#   c("mae", "bias", "bias_rel", "corr")  
# )
ylabs <- setNames(
  c("MAE", "Bias", "Bias", "Correlation"),
  c("mae", "bias", "bias_rel", "corr")
)



dat_plot_hn_violin[, metric2 := ylabs[metric]]
dat_plot_hn_violin[, metric2_fct := factor(metric2)]

gg_hn <- 
# dat_plot_hn_violin |> 
dat_plot_hn_violin[!(metric == "bias_rel" & value > 1)] |> 
  ggplot(aes(value, fct_rev(bads2), fill = bads_multi_fct))+
  geom_violin(quantile.linetype = "solid", quantiles = c(0.5))+
  facet_grid(season ~ metric2_fct, scales = "free_x")+
  # scale_x_facet(metric2_fct == "Bias (relative)", labels = scales::label_percent())+
  scale_x_facet(metric2_fct == "Bias", labels = scales::label_percent())+
  scale_fill_brewer(NULL, palette = "Pastel1")+
  theme_bw()+
  theme(legend.position = "bottom")+
  xlab(NULL)+ylab(NULL)

ggsave(filename = "fig/paper-ds-pdf/ds-metrics-icell_ensavg_hn_violin.pdf",
       gg_hn,
       width = 10, height = 5)



# subset for main results -------------------------------------------------

# mam tasmin, pr mam

gg1 <-
  dat_ba2[variable == "pr" & season == "MAM"] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = bias_rel))+
  geom_raster()+
  scale_fill_scico("bias", 
                   palette = "vik", 
                   midpoint = 0, 
                   labels = scales::label_percent(),
                   direction = -1)+
  facet_grid(. ~ bads)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())+
  xlab(NULL)+ylab(NULL)+
  ggtitle("pr MAM")


gg2 <-
  dat_ba2[variable == "pr" & season == "MAM"] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = mae))+
  geom_raster()+
  scale_fill_scico("MAE [mm]", 
                   palette = "bamako")+
  facet_grid(. ~ bads)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())+
  xlab(NULL)+ylab(NULL)


gg3 <-
  dat_ba2[variable == "pr" & season == "MAM"] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = corr))+
  geom_raster()+
  scale_fill_scico("correlation", 
                   palette = "batlow")+
  facet_grid(. ~ bads)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())+
  xlab(NULL)+ylab(NULL)



gg_out <- (gg1+gg2+gg3)+
  plot_layout(ncol = 1)


ggsave(filename = "fig/paper-ds-pdf/eval-ds-maps-pr.pdf",
       gg_out,
       width = 8, height = 6)



gg4 <-
  dat_ba2[variable == "tasmax" & season == "MAM"] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = bias))+
  geom_raster()+
  scale_fill_scico("bias [°C]", 
                   palette = "vik", 
                   midpoint = 0, 
                   direction = +1,
                   limits = c(-2,2),
                   oob = scales::oob_squish)+
  facet_grid(. ~ bads)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())+
  xlab(NULL)+ylab(NULL)+
  ggtitle("tasmax MAM")


gg5 <-
  dat_ba2[variable == "tasmax" & season == "MAM"] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = mae))+
  geom_raster()+
  scale_fill_scico("MAE [°C]", 
                   palette = "bamako",
                   limits = c(NA,5),
                   oob = scales::oob_squish)+
  facet_grid(. ~ bads)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())+
  xlab(NULL)+ylab(NULL)


gg6 <-
  dat_ba2[variable == "tasmax" & season == "MAM"] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = corr))+
  geom_raster()+
  scale_fill_scico("correlation", 
                   palette = "batlow")+
  # limits = c(-2,2),
  # oob = scales::oob_squish)+
  facet_grid(. ~ bads)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())+
  xlab(NULL)+ylab(NULL)


gg_out <- (gg4+gg5+gg6)+
  plot_layout(ncol = 1)


ggsave(filename = "fig/paper-ds-pdf/eval-ds-maps-tasmax.pdf",
       gg_out,
       width = 12, height = 6)



# numbers -----------------------------------------------------------------


dat_ba2[season == "MAM" & variable == "pr", median(bias_rel), bads]
dat_ba2[season == "MAM" & variable == "pr", range(bias_rel), bads]

dat_ba2[season == "MAM" & variable == "pr", median(bias), bads]
dat_ba2[season == "MAM" & variable == "pr", range(bias), bads]

dat_ba2[season == "MAM" & variable == "pr", median(mae), bads]
dat_ba2[season == "MAM" & variable == "pr", range(mae), bads]


dat_plot_hn_violin[season == "MAM" & metric == "bias_rel", median(value), bads]
