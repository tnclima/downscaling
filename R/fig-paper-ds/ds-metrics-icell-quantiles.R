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

dat_crespi <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/icell/mean-pctl/crespi.rds")
dat_crespi_011 <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/icell/mean-pctl/crespi-011.rds")




path_uni <- dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/icell/mean-pctl/", glob = "*/ba*") %>% 
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
# 
# dat_ba[, rcm_short := eurocordexr::shortnames_rcm[institute_rcm]]
# dat_ba[, institute := str_remove(institute_rcm, rcm_short)]
# dat_ba[, institute := str_remove(institute, "-$")]

# vars_summ <- c(
#   str_c("tasmin_p", c(1,5,50,95,99)),
#   str_c("tasmax_p", c(1,5,50,95,99)),
#   "pr_mean",
#   str_c("pr_p", c(95,99))
# )
vars_summ <- c(
  str_c("tasmin_p", c(0, 1,5,50,95,99, 100)),
  str_c("tasmax_p", c(0, 1,5,50,95,99, 100)),
  "pr_mean",
  str_c("pr_p", c(95,99,100))
)

dat_ba2 <- dat_ba[,
                  lapply(.SD, mean),
                  .(season, icell, bads, bads_ds),
                  .SDcols = vars_summ]

# dat_ba2[, bads2 := str_replace(bads, "ba-qdm-", "ba-qdm-\n")]

dat_plot_crespi <- dat_crespi %>%  
  melt(id.vars = c("season", "icell"),
       variable.name = "variable_pctl") %>% 
  .[, c("variable", "zz", "pctl") := tstrsplit(variable_pctl, "_")]
setnames(dat_plot_crespi, "value", "value_crespi")

dat_plot <-
  dat_ba2 |> 
  melt(id.vars = c("season", "icell", "bads", "bads_ds"),
       variable.name = "variable_pctl") %>% 
  .[, c("variable", "pctl") := tstrsplit(variable_pctl, "_")] %>% 
  merge(dat_plot_crespi, by = c("season", "icell", "variable", "pctl"))

dat_plot[, pctl_fct := factor(pctl, levels = c("mean", str_c("p", c(0,1,5,50,95,99,100))))]

# 1km ---------------------------------------------------------------


  
gg_pr <-
dat_plot[variable == "pr" & !bads %in% c("ba-qdm-ds-lr", "ba-qdm-ds-gam")] |> 
  ggplot(aes((value - value_crespi)/value_crespi, bads, fill = pctl_fct))+
  # geom_violin(quantile.linetype = "solid", quantiles = c(0.5))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_boxplot(coef = Inf)+
  # facet_grid(season ~ "pr")+
  facet_wrap(~season)+
  scale_x_continuous(labels = scales::label_percent())+
  scale_fill_brewer(NULL, palette = "PuBu", direction = 1)+
  theme_bw()+
  # theme(legend.position = "bottom")+
  xlab("Bias")+ylab(NULL)



ggsave(filename = "fig/paper-ds-pdf/ds-metrics-icell-quantiles_ensavg_pr.pdf",
       gg_pr,
       width = 10, height = 7)


gg_tasmin <-
dat_plot[variable == "tasmin"] |> 
  ggplot(aes(value - value_crespi, bads, fill = pctl_fct))+
  # geom_violin(quantile.linetype = "solid", quantiles = c(0.5))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_boxplot(coef = Inf)+
  # facet_grid(season ~ "tasmin")+
  facet_wrap(~season)+
  scale_x_continuous(limits = c(-5,5), oob = scales::oob_squish)+
  scale_fill_brewer(NULL, palette = "RdBu", direction = -1)+
  theme_bw()+
  # theme(legend.position = "bottom")+
  xlab("Bias [°C]")+ylab(NULL)



ggsave(filename = "fig/paper-ds-pdf/ds-metrics-icell-quantiles_ensavg_tasmin.pdf",
       gg_tasmin,
       width = 9, height = 12)




gg_tasmax <- dat_plot[variable == "tasmax"] |> 
  ggplot(aes(value - value_crespi, bads, fill = pctl_fct))+
  # geom_violin(quantile.linetype = "solid", quantiles = c(0.5))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_boxplot(coef = Inf)+
  # facet_grid(season ~ "tasmax")+
  facet_wrap(~season)+
  scale_x_continuous(limits = c(-5,5), oob = scales::oob_squish)+
  scale_fill_brewer(NULL, palette = "RdBu", direction = -1)+
  theme_bw()+
  # theme(legend.position = "bottom")+
  xlab("Bias [°C]")+ylab(NULL)



ggsave(filename = "fig/paper-ds-pdf/ds-metrics-icell-quantiles_ensavg_tasmax.pdf",
       gg_tasmax,
       width = 9, height = 12)



# 011 ---------------------------------------------------------------------

path_ba_011 <- "/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/icell/mean-pctl/ba-qdm/"

dat_ba_011 <- map(
  path_ba_011,
  \(path_ba){
    map(dir_ls(path_ba),
        \(x){
          rcm <- x %>% path_file %>% path_ext_remove
          readRDS(x) %>% 
            cbind(institute_rcm = rcm)
        }) %>% rbindlist(fill = T) %>% cbind(bads = path_file(path_ba))
  }
) %>% rbindlist(fill = T)

dat_ba_011_2 <- dat_ba_011[,
                          lapply(.SD, mean),
                          .(season, icell),
                          .SDcols = vars_summ]

# dat_ba2[, bads2 := str_replace(bads, "ba-qdm-", "ba-qdm-\n")]

dat_plot_crespi_011 <- dat_crespi_011 %>%  
  melt(id.vars = c("season", "icell"),
       variable.name = "variable_pctl") %>% 
  .[, c("variable", "zz", "pctl") := tstrsplit(variable_pctl, "_")]
setnames(dat_plot_crespi_011, "value", "value_crespi")

dat_plot_011 <-
  dat_ba_011_2 |> 
  melt(id.vars = c("season", "icell"),
       variable.name = "variable_pctl") %>% 
  .[, c("variable", "pctl") := tstrsplit(variable_pctl, "_")] %>% 
  merge(dat_plot_crespi_011, by = c("season", "icell", "variable", "pctl"))

dat_plot_011[, pctl_fct := factor(pctl, levels = c("mean", str_c("p", c(0,1,5,50,95,99,100))))]



gg_011_tasmax <- dat_plot_011[variable == "tasmax"] |> 
  ggplot(aes(value - value_crespi, fct_rev(season), fill = pctl_fct))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_boxplot(coef = Inf)+
  # facet_wrap(~season)+
  # scale_x_continuous(limits = c(-5,5), oob = scales::oob_squish)+
  scale_fill_brewer(NULL, palette = "RdBu", direction = -1)+
  theme_bw()+
  xlab("Bias [°C]")+ylab(NULL)+
  ggtitle("tasmax")

gg_011_tasmin <- dat_plot_011[variable == "tasmin"] |> 
  ggplot(aes(value - value_crespi, fct_rev(season), fill = pctl_fct))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_boxplot(coef = Inf)+
  # facet_wrap(~season)+
  # scale_x_continuous(limits = c(-5,5), oob = scales::oob_squish)+
  scale_fill_brewer(NULL, palette = "RdBu", direction = -1)+
  theme_bw()+
  xlab("Bias [°C]")+ylab(NULL)+
  ggtitle("tasmin")

gg_011_pr <- dat_plot_011[variable == "pr"] |> 
  ggplot(aes((value - value_crespi)/value_crespi, fct_rev(season), fill = pctl_fct))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_boxplot(coef = Inf)+
  # facet_wrap(~season)+
  scale_x_continuous(labels = scales::label_percent())+
  scale_fill_brewer(NULL, palette = "PuBu", direction = 1)+
  theme_bw()+
  xlab("Bias")+ylab(NULL)+
  ggtitle("pr")

gg_out <- (gg_011_pr + gg_011_tasmax + gg_011_tasmin)
ggsave("fig/paper-ds-pdf/ds-metrics-icell-quantiles_ensavg_011.pdf", 
       gg_out, width = 12, height = 10)

# numbers -----------------------------------------------------------------


dat_plot[variable == "tasmin" &  season == "DJF" & bads == "bads-qdm",
         .(median(value-value_crespi),
           min(value-value_crespi),
           max(value-value_crespi)),
         .(pctl)] 

dat_plot[variable == "pr" &  season == "JJA" & bads == "bads-qdm",
         .(median((value-value_crespi)/value_crespi),
           min((value-value_crespi)/value_crespi),
           max((value-value_crespi)/value_crespi)),
         .(pctl)] 