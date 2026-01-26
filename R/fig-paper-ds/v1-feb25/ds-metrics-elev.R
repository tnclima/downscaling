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


# data --------------------------------------------------------------------

dat_raw <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/elev/metrics/raw"),
  \(x){
    rcm <- x %>% path_file %>% path_ext_remove
    readRDS(x) %>% 
      cbind(institute_rcm = rcm)
  }
) %>% rbindlist()



dat_ba <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/elev/metrics/", glob = "*/ba*"),
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


dat_plot2 <- dat_plot[, 
                      .(value = mean(value)),
                      .(season, elev_fct, variable, bads_ds, metric,
                        metric2_fct, bads_multi, bads_multi_fct)]

# tasmax ------------------------------------------------------------------

gg <-
dat_plot2[variable == "tasmax" & bads_multi == F & metric != "bias_rel"] %>%
  ggplot(aes(bads_ds, value, colour = elev_fct))+
  geom_hline(data = data.frame(yy = 0, metric2_fct = "Bias"),
             aes(yintercept = yy), linetype = "dashed")+
  geom_point(shape = 21)+
  geom_line(aes(group = elev_fct))+
  facet_grid(metric2_fct ~ season, scales = "free_y", switch = "y")+
  scale_color_grey("Elevation")+
  # scale_color_scico_d(palette = "nuuk", direction = -1)+
  # scale_y_facet(metric2_fct == "bias", labels = scales::label_percent())+
  theme_bw()+
  theme(strip.placement = "outside", 
        strip.background.y = element_blank(),
        strip.text.y.left = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  xlab(NULL)+
  ylab(NULL)


ggsave("fig/paper-ds/ds-metrics-elev_tasmax.png",
       gg, width = 8, height = 6)



# tasmin ------------------------------------------------------------------

gg <-
  dat_plot2[variable == "tasmin" & bads_multi == F & metric != "bias_rel"] %>%
  ggplot(aes(bads_ds, value, colour = elev_fct))+
  geom_hline(data = data.frame(yy = 0, metric2_fct = "Bias"),
             aes(yintercept = yy), linetype = "dashed")+
  geom_point(shape = 21)+
  geom_line(aes(group = elev_fct))+
  facet_grid(metric2_fct ~ season, scales = "free_y", switch = "y")+
  scale_color_grey("Elevation")+
  # scale_color_scico_d(palette = "nuuk", direction = -1)+
  # scale_y_facet(metric2_fct == "bias", labels = scales::label_percent())+
  theme_bw()+
  theme(strip.placement = "outside", 
        strip.background.y = element_blank(),
        strip.text.y.left = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  xlab(NULL)+
  ylab(NULL)


ggsave("fig/paper-ds/ds-metrics-elev_tasmin.png",
       gg, width = 8, height = 6)


# pr ----------------------------------------------------------------------


gg <-
dat_plot2[variable == "pr" & bads_multi == F & metric != "bias"] %>%
  ggplot(aes(bads_ds, value, colour = elev_fct))+
  geom_hline(data = data.frame(yy = 0, metric2_fct = "Bias"),
             aes(yintercept = yy), linetype = "dashed")+
  geom_point(shape = 21)+
  geom_line(aes(group = elev_fct))+
  facet_grid(metric2_fct ~ season, scales = "free_y", switch = "y")+
  scale_color_grey("Elevation")+
  scale_y_facet(metric2_fct == "Bias", labels = scales::label_percent())+
  theme_bw()+
  theme(strip.placement = "outside", 
        strip.background.y = element_blank(),
        strip.text.y.left = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  xlab(NULL)+
  ylab(NULL)


ggsave("fig/paper-ds/ds-metrics-elev_pr.png",
       gg, width = 8, height = 6)


# hn ----------------------------------------------------------------------


gg <-
dat_plot2[variable == "hn" & season %in% c("DJF", "MAM") & metric != "bias" & 
            bads_ds != "ds-qdm"] %>% 
  ggplot(aes(elev_fct, value, colour = bads_ds, 
             shape = bads_multi_fct, linetype = bads_multi_fct))+
  geom_hline(data = data.frame(yy = 0, metric2_fct = "Bias"),
             aes(yintercept = yy), linetype = "dashed")+
  geom_point()+
  geom_line(aes(group = paste0(bads_ds, bads_multi_fct)))+
  facet_grid(metric2_fct ~ season, scales = "free_y", switch = "y")+
  scale_y_facet(metric2_fct == "Bias", labels = scales::label_percent())+
  scale_colour_brewer(NULL, palette = "Dark2")+
  theme_bw()+
  theme(strip.placement = "outside", 
        strip.background.y = element_blank(),
        strip.text.y.left = element_text(size = rel(1.2)),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  xlab("Elevation")+
  ylab(NULL)


ggsave("fig/paper-ds/ds-metrics-elev_hn.png",
       gg, width = 6, height = 6)


