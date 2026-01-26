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



# data --------------------------------------------------------------------


dat_ba <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v5/icell/metrics/"),
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
dat_plot[, pc_extra := str_remove(bads_fct, "nPC[0-9]") |> str_remove("^-")]
# dat_plot[pc_extra == "", pc_extra := "no"]
dat_plot[, table(variable, pc_extra)]
dat_plot[, ff := pc_extra]
dat_plot[str_starts(variable, "tas"), ff := ifelse(pc_extra == "", "PC1", "orog")]
dat_plot[variable == "pr", ff := ifelse(pc_extra == "", "PCA-occ-int",
                                        ifelse(pc_extra == "orog-pcalog",
                                               "PCAlog-occ-PCA-int",
                                               "PCA-int"))]
dat_plot[variable == "hn", ff := ifelse(pc_extra == "",
                                        "[tas]PC1 [pr]PCA-occ-int",
                                        "[tas]orog [pr]PCAlog-occ-PCA-int")]


# pr ----------------------------------------------------------------------



gg1 <-
dat_plot[variable == "pr"] |> 
  ggplot(aes(as.factor(n_pc), value, fill = ff))+
  geom_violin()+
  facet_grid(metric ~ season, scales = "free_y")+
  theme_bw()+
  theme(legend.title = element_blank())+
  xlab("# PC")+
  ylab(NULL)

ggsave("fig/paper-ds/obs-ds-pcalm_pr.png",
       gg1,
       width = 12, height = 6)



# tasmin ------------------------------------------------------------------


gg1 <-
  dat_plot[variable == "tasmin" & metric != "bias_rel"] |> 
  ggplot(aes(as.factor(n_pc), value, fill = ff))+
  geom_violin()+
  facet_grid(metric ~ season, scales = "free_y")+
  theme_bw()+
  theme(legend.title = element_blank())+
  xlab("# PC")+
  ylab(NULL)

ggsave("fig/paper-ds/obs-ds-pcalm_tasmin.png",
       gg1,
       width = 12, height = 5)




# tasmax ------------------------------------------------------------------



gg1 <-
  dat_plot[variable == "tasmax" & metric != "bias_rel"] |> 
  ggplot(aes(as.factor(n_pc), value, fill = ff))+
  geom_violin()+
  facet_grid(metric ~ season, scales = "free_y")+
  theme_bw()+
  theme(legend.title = element_blank())+
  xlab("# PC")+
  ylab(NULL)

ggsave("fig/paper-ds/obs-ds-pcalm_tasmax.png",
       gg1,
       width = 12, height = 5)



# subset for main -------------------------------------------------------

gg_pr_mae <- dat_plot[variable == "pr" & metric == "mae" & season == "JJA"] |> 
  ggplot(aes(as.factor(n_pc), value, fill = ff))+
  geom_violin(quantile.linetype = "solid", quantiles = c(0.5))+
  theme_bw()+
  theme(legend.title = element_blank())+
  xlab("# PC")+
  ylab("MAE")

gg_pr_bias <- dat_plot[variable == "pr" & metric == "bias_rel" & season == "JJA"] |> 
  ggplot(aes(as.factor(n_pc), value, fill = ff))+
  geom_violin(quantile.linetype = "solid", quantiles = c(0.5))+
  scale_y_continuous(labels = scales::label_percent())+
  theme_bw()+
  theme(legend.title = element_blank(), 
        plot.title.position = "plot")+
  xlab("# PC")+
  ylab("Relative bias")+
  ggtitle("a) Precipitation JJA")



gg_tasmin_mae <- dat_plot[variable == "tasmin" & metric == "mae" & season == "JJA"] |> 
  ggplot(aes(as.factor(n_pc), value, fill = ff))+
  geom_violin(quantile.linetype = "solid", quantiles = c(0.5))+
  theme_bw()+
  theme(legend.title = element_blank())+
  xlab("# PC")+
  ylab("MAE")


gg_tasmin_bias <- dat_plot[variable == "tasmin" & metric == "bias" & season == "JJA"] |> 
  ggplot(aes(as.factor(n_pc), value, fill = ff))+
  geom_violin(quantile.linetype = "solid", quantiles = c(0.5))+
  theme_bw()+
  theme(legend.title = element_blank(), 
        plot.title.position = "plot")+
  xlab("# PC")+
  ylab("Bias")+
  ggtitle("b) Minimum temperature JJA")

gg1 <- gg_pr_bias + gg_pr_mae + plot_layout(guides = "collect")
gg2 <- gg_tasmin_bias + gg_tasmin_mae + plot_layout(guides = "collect")

gg_out <- gg1/gg2

ggsave("fig/paper-ds/obs-ds-pcalm_subset.png", gg_out, width = 11, height = 5)
