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
# 
# dat_aux <- eurocordexr::nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc",
#                                       add_xy = T)
# dat_aux <- dat_aux[!is.na(orog), .(icell, x = longitude, y = latitude, orog)]

dat_crespi <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/tnaa/ecdf/crespi.rds")
dat_crespi_011 <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/tnaa/ecdf/crespi-011.rds")

dat_raw <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/tnaa/ecdf/raw"),
  \(x){
    rcm <- x %>% path_file %>% path_ext_remove
    readRDS(x) %>% 
      cbind(institute_rcm = rcm)
  }
) %>% rbindlist()


path_uni <- dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/tnaa/ecdf/", glob = "*/ba*") %>% 
  str_subset("mbcn", negate = T) # %>% 
  # str_subset("ba-qdm$", negate = T) # %>% 
  # str_subset("-ds-qdm$", negate = T)



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



dat_plot <- dat_ba[bads != "ba-qdm"] |> 
  merge(dat_crespi[, .(season, 
                       qval_crespi = qval, pctl, 
                       variable = str_remove(variable, "_crespi"))])

dat_plot[, bads_ds := bads %>% forcats::fct_recode(
  "ds-pcalm" = "ba-qdm-ds-pcalm",
  # "ds-pcalm" = "ba-mbcn-ds-pcalm",
  "ds-qdm" = "ba-qdm-ds-qdm",
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


dat_plot[, bads_ds := factor(bads_ds, levels = c(
  "bads", "ds-qdm", "ds-qdm2", "ds-pcalm", "ds-lr", "ds-gam"
))]


# dat_ba[, bads_multi_fct := ifelse(bads_multi, "multivariate", "univariate")]

# dat_ba[, rcm_short := eurocordexr::shortnames_rcm[institute_rcm]]
# dat_ba[, institute := str_remove(institute_rcm, rcm_short)]
# dat_ba[, institute := str_remove(institute, "-$")]

# dat_plot2_abs <- dat_plot[variable %in% c("tasmin", "tasmax"),
#                           mitmatmisc::calc_pctl(qval-qval_crespi),
#                           .(variable, season, pctl, bads_ds)]
# dat_plot2_rel <- dat_plot[pctl >= 0.8 & variable %in% c("pr", "hn"),
#                           mitmatmisc::calc_pctl((qval-qval_crespi)/qval_crespi),
#                           .(variable, season, pctl, bads_ds)]


dat_plot_011 <- rbind(
  dat_raw %>% cbind(bads = "raw"),
  dat_ba[bads %in% c("ba-qdm")]
) %>% 
  merge(dat_crespi_011[, .(season, qval_crespi = qval, pctl, variable = str_remove(variable, "_crespi"))])




# qq-plot --------------------------------------------------------------------



## tasmin/max --------------------------------------------------------------



# dat_plot[variable %in% c("tasmin", "tasmax")] |> 
#   ggplot(aes(qval_crespi, qval, group = institute_rcm))+
#   geom_abline()+
#   geom_point(alpha = 0.5, size = 1)+
#   # facet_grid2(variable + season ~ bads_ds, scales = "free", independent = "x")+
#   facet_nested(variable + season ~ bads_ds, scales = "free", independent = "x")+
#   theme_bw()

# gg <-
dat_plot[(variable == "tasmin" & season == "DJF") | 
           (variable == "tasmax" & season == "JJA")] |>
  ggplot(aes(qval_crespi, qval, group = institute_rcm))+
  geom_abline()+
  geom_point(alpha = 0.5, size = 1)+
  # facet_grid2(variable + season ~ bads_ds, scales = "free", independent = "x")+
  facet_nested(variable + season ~ bads_ds, scales = "free", independent = "x")+
  theme_bw()+
  xlab("Observed quantile")+
  ylab("Downscaling Model quantile")

ggsave("fig/paper-ds/qq-bads-tasminmax.png",
       gg, width = 12, height = 5)


gg <-
dat_plot[(variable == "tasmin" & season == "DJF") | 
                 (variable == "tasmax" & season == "JJA")] |>
  ggplot(aes(qval_crespi, qval - qval_crespi, group = institute_rcm))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(alpha = 0.5, size = 0.5)+
  geom_line(alpha = 0.5, linewidth = 0.3)+
  # facet_grid2(variable + season ~ bads_ds, scales = "free", independent = "x")+
  facet_nested(variable + season ~ bads_ds, scales = "free", space = "free_y", independent = "x")+
  theme_bw()+
  xlab("Observed quantile")+
  ylab("Difference in quantiles (between downscaled and observed)")

ggsave("fig/paper-ds/qq-bads-tasminmax_absdiff.png",
       gg, width = 12, height = 5)



## pr ----------------------------------------------------------------------



gg <- dat_plot[variable == "pr"] |> 
  ggplot(aes(qval_crespi, qval, group = institute_rcm))+
  geom_abline()+
  geom_point(alpha = 0.5, size = 1)+
  scale_x_sqrt()+
  scale_y_sqrt()+
  facet_grid2(season ~ bads_ds, scales = "free_y")+
  theme_bw()+
  xlab("Observed quantile")+
  ylab("Downscaling Model quantile")

ggsave("fig/paper-ds/qq-bads-pr.png",
       gg, width = 10, height = 6)

gg <- dat_plot[variable == "pr" & pctl > 0.25] |> 
  ggplot(aes(qval_crespi, qval/qval_crespi - 1, group = institute_rcm))+
  # geom_abline()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(alpha = 0.5, size = 0.5)+
  geom_line(alpha = 0.5, linewidth = 0.3)+
  # scale_x_sqrt()+
  # scale_y_sqrt()+
  scale_y_continuous(labels = scales::label_percent(), 
                     limits = c(-1, 1),
                     oob = scales::oob_squish)+
  facet_grid2(season ~ bads_ds)+
  theme_bw()+
  xlab("Observed quantile")+
  ylab("Difference in quantiles (between downscaled and observed)")

ggsave("fig/paper-ds/qq-bads-pr_reldiff.png",
       gg, width = 10, height = 6)

# qq-plot 011--------------------------------------------------------------------


gg1 <- dat_plot_011[bads == "ba-qdm" & variable == "pr"] |> 
  ggplot(aes(qval_crespi, qval, group = institute_rcm))+
  geom_abline()+
  geom_point(alpha = 0.5, size = 1)+
  scale_x_sqrt()+
  scale_y_sqrt()+
  facet_grid2(variable ~ season)+
  theme_bw()+
  xlab("Observed quantile")+
  ylab("Model quantile (ba-qdm)")

gg2 <- dat_plot_011[bads == "ba-qdm" & variable %in% c("tasmin", "tasmax")] |> 
  ggplot(aes(qval_crespi, qval, group = institute_rcm))+
  geom_abline()+
  geom_point(alpha = 0.5, size = 1)+
  facet_grid2(variable ~ season, scales = "free", independent = T)+
  theme_bw()+
  xlab("Observed quantile")+
  ylab("Model quantile (ba-qdm)")

gg_out <- (gg1/gg2)+
  plot_layout(heights = c(1,2), axis_titles = "collect")

ggsave("fig/paper-ds/qq-ba.png",
       gg_out, width = 10, height = 6)




gg1 <-
  dat_plot_011[bads == "ba-qdm" & variable == "pr" & pctl > 0.25] |> 
  ggplot(aes(qval_crespi, qval/qval_crespi - 1, group = institute_rcm))+
  # geom_abline()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(alpha = 0.5, size = 0.5)+
  geom_line(alpha = 0.5, linewidth = 0.3)+
  # scale_x_sqrt()+
  # scale_y_sqrt()+
  scale_y_continuous(labels = scales::label_percent(), 
                     limits = c(-1, 1),
                     oob = scales::oob_squish)+
  facet_grid2(variable ~ season)+
  theme_bw()+
  xlab("Observed quantile")+
  ylab("Difference Model-Observed quantile (ba-qdm)")

gg2 <-
  dat_plot_011[bads == "ba-qdm" & variable %in% c("tasmin", "tasmax")] |> 
  ggplot(aes(qval_crespi, qval-qval_crespi, group = institute_rcm))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(alpha = 0.5, size = 0.5)+
  geom_line(alpha = 0.5, linewidth = 0.3)+
  facet_grid2(variable ~ season, scales = "free", independent = T)+
  theme_bw()+
  xlab("Observed quantile")+
  ylab("Difference Model-Observed quantile (ba-qdm)")

gg_out <- (gg1/gg2)+
  plot_layout(heights = c(1,2), axis_titles = "collect")

ggsave("fig/paper-ds/qq-ba_diff.png",
       gg_out, width = 10, height = 6)




# numbers -----------------------------------------------------------------


dat_plot[variable == "pr" & season == "DJF" & pctl >= 0.95,
         mitmatmisc::calc_pctl(qval - qval_crespi),
         .(pctl, bads)]

dat_crespi[variable == "pr_crespi" & season == "MAM" & pctl < 0.9 & pctl > 0.2,
           .(pctl, season, precip_qval = round(qval, 2))]

dat_crespi[variable == "pr_crespi" & pctl == 0.75,
           .(pctl, season, precip_qval = round(qval, 2))]

dat_plot[variable == "pr" & pctl > 0.2 & pctl < 0.9 & bads_ds != "ds-qdm",
         mitmatmisc::calc_pctl((qval - qval_crespi)/qval_crespi),
         .(pctl)]

dat_plot[variable == "pr" & pctl > 0.75 & pctl < 0.98 & bads_ds != "ds-qdm",
         mitmatmisc::calc_pctl((qval - qval_crespi)/qval_crespi)]

dat_plot[variable == "pr" & pctl == 1 & bads_ds != "ds-qdm" & season == "JJA",
         mitmatmisc::calc_pctl((qval - qval_crespi)/qval_crespi)]
# 
# dat_plot[variable == "pr" & pctl == 1 & bads_ds != "ds-qdm",
#          mitmatmisc::calc_pctl((qval - qval_crespi)/qval_crespi)]

dat_plot[variable == "tasmax" & pctl <= 0.01 & bads_ds != "ds-qdm" & season == "JJA",
         mitmatmisc::calc_pctl(qval - qval_crespi) |> lapply(round, 1),
         pctl]

dat_plot[variable == "tasmax" & pctl >= 0.99 & bads_ds != "ds-qdm" & season == "JJA",
         mitmatmisc::calc_pctl(qval - qval_crespi) |> lapply(round, 1),
         pctl]

# 
# dat_plot[variable == "pr" & season == "DJF" & pctl >= 0.95,
#          mitmatmisc::calc_pctl((qval - qval_crespi)/qval_crespi),
#          .(pctl, bads)]
# 
# 
# dat_plot[variable == "tasmin" & season == "DJF" & pctl >= 0.98,
#          mitmatmisc::calc_pctl(qval - qval_crespi),
#          .(pctl, bads)]
# 
# dat_plot[variable == "tasmin" & season == "DJF" & pctl <= 0.02,
#          mitmatmisc::calc_pctl(qval - qval_crespi),
#          .(pctl, bads)]
# 
# dat_plot[variable == "tasmax" & season == "JJA" & pctl <= 0.02,
#          mitmatmisc::calc_pctl(qval - qval_crespi),
#          .(pctl, bads)]
