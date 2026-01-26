#

library(lubridate)
library(data.table)
setDTthreads(4)
library(fs)
library(purrr)
library(foreach)
library(stringr)
library(ggplot2)
library(scico)
library(eurocordexr)
library(patchwork)
library(ggh4x)

dat_aux <- nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc",
                         add_xy = T)
dat_aux <- dat_aux[!is.na(orog), .(icell, lon = longitude, lat = latitude, orog)]

dat <- readRDS("data/corr-tp-rcm-1km.rds")
dat[, multivar := ifelse(str_detect(bads, fixed("mbcn")), "multivariate", "univariate")]
dat[, bads2 := str_remove(bads, "ba-qdm-") |> str_remove("ba-mbcn-")]
dat$bads |> table()


# icell_sub <- dat[bads == "obs" & !is.na(corr_pr_tasmin), unique(icell)]
i_var <- "corr_pr_tasmax"
dat_i <- dat[month == 4]
setnames(dat_i, i_var, "value")

col_lim <- range(dat_i$value, na.rm = T)


gg1_corr <-
  dat_i[bads != "obs"] |> 
  merge(dat_aux) |> 
  ggplot(aes(lon, lat, fill = value))+
  geom_raster()+
  facet_nested(rcm ~ multivar + bads, switch = "y")+
    theme_classic()+
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          strip.text.y.left = element_text(angle = 0),
          legend.position = "none")+
  scale_fill_scico("corr", palette = "roma", midpoint = 0, limits = col_lim)+
  coord_fixed()

gg2_obs <-
  dat_i[bads == "obs"] |> 
  merge(dat_aux) |> 
  ggplot(aes(lon, lat, fill = value))+
  geom_raster()+
  facet_nested(. ~ rcm)+
  theme_classic()+
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom")+
  scale_fill_scico("corr", palette = "roma", midpoint = 0, limits = col_lim)+
  coord_fixed()



gg_out <- (gg1_corr / gg2_obs)+
  plot_layout(heights = c(8, 1))

ggsave("fig/paper-ds/corr-tasmax-pr-apr.png",
       gg_out, width = 12, height = 14)

