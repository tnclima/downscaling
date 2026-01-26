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

# pr ----------------------------------------------------------------------

gg <-
dat_ba[variable == "pr"] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = bias_rel))+
  geom_raster()+
  scale_fill_scico("pr bias", 
                   palette = "vik", 
                   midpoint = 0, 
                   labels = scales::label_percent(),
                   direction = -1)+
  facet_nested(season + bads_ds ~ institute + rcm_short)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))+
  xlab(NULL)+ylab(NULL)

ggsave("fig/paper-ds/ds-metrics-icell_pr_bias.png",
       gg, width = 18, height = 14)




gg <-
  dat_ba[variable == "pr"] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = corr))+
  geom_raster()+
  scale_fill_scico("pr correlation", 
                   palette = "batlow")+
  facet_nested(season + bads_ds ~ institute + rcm_short)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))+
  xlab(NULL)+ylab(NULL)

ggsave("fig/paper-ds/ds-metrics-icell_pr_corr.png",
       gg, width = 18, height = 14)



# tasmin DJF, tasmax JJA --------------------------------------------------




