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





# subset ------------------------------------------------------------------


# tasmin 


gg_tasmin <-
dat_plot[variable == "tasmin" & bads_multi == F & 
           metric != "bias_rel" & season == "DJF" & 
           bads %in% c("bads-qdm", "ba-qdm-ds-qdm", "ba-qdm-ds-qdm2")] %>% 
  ggplot(aes(value, bads))+
  geom_vline(data = data.frame(xx = 0, metric2_fct = "Bias"),
             aes(xintercept = xx), linetype = "dashed")+
  geom_boxplot()+
  # facet_grid(metric2_fct ~ season, scales = "free_y", switch = "y")+
  facet_wrap(~ metric2_fct, scales = "free_x")+
  # scale_y_facet(metric2_fct == "bias", labels = scales::label_percent())+
  theme_bw()+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("tasmin DJF")



# pr 


gg_pr <-
  dat_plot[variable == "pr" & bads_multi == F &
             metric != "bias" & season == "DJF" &
             bads %in% c("bads-qdm", "ba-qdm-ds-qdm", "ba-qdm-ds-qdm2")] %>% 
  ggplot(aes(value, bads))+
  geom_vline(data = data.frame(xx = 0, metric2_fct = "Bias"),
             aes(xintercept = xx), linetype = "dashed")+
  geom_boxplot()+
  # facet_grid(metric2_fct ~ season, scales = "free_y", switch = "y")+
  facet_wrap(~ metric2_fct, scales = "free_x")+
  scale_x_facet(metric2_fct == "Bias", labels = scales::label_percent())+
  theme_bw()+

  xlab(NULL)+
  ylab(NULL)+
  ggtitle("pr DJF")


gg_out <- (gg_tasmin / gg_pr)+
  plot_annotation(tag_levels = "a", tag_suffix = ")")


ggsave("fig/paper-ds/sep-ba-ds.png",
       gg_out, width = 10, height = 4)




# numbers -----------------------------------------------------------------

dat_plot_tasmin <- dat_plot[variable == "tasmin" & bads_multi == F & 
                              metric != "bias_rel" & season == "DJF" & 
                              bads %in% c("bads-qdm", "ba-qdm-ds-qdm", "ba-qdm-ds-qdm2")] 

dat_plot_pr <- dat_plot[variable == "pr" & bads_multi == F &
           metric != "bias" & season == "DJF" &
           bads %in% c("bads-qdm", "ba-qdm-ds-qdm", "ba-qdm-ds-qdm2")]

dat_plot_pr[metric == "bias_rel", median(value), bads]
dat_plot_pr[metric == "bias_rel", range(value), bads]
