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

path_randyear <- dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v6/icell/metrics/", glob = "*/ba*")

path_uni <- str_c("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/icell/metrics/", path_file(path_randyear)) 
  
  

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

dat_ba_randyear <- map(
  path_randyear,
  \(path_ba){
    map(dir_ls(path_ba),
        \(path_i_rep){
          map(dir_ls(path_i_rep), \(x){
            rcm <- x %>% path_file %>% path_ext_remove
            readRDS(x) %>% 
              cbind(institute_rcm = rcm)
          }) %>% rbindlist(fill = T) %>% cbind(bads = path_file(path_ba), 
                                               randyear = path_file(path_i_rep))
        }) |> rbindlist(fill = T)
  }) %>% rbindlist(fill = T)



dat_plot <- dat_ba[bads %in% dat_ba_randyear$bads] |> 
  cbind(randyear = "0") |> 
  rbind(dat_ba_randyear, fill = T)


dat_plot2 <- dat_plot[,
                  lapply(.SD, mean),
                  .(season, icell, variable, bads, randyear),
                  .SDcols = c("mae", "bias", "bias_rel", "corr")]

# dat_plot[, bads_multi := str_detect(bads, "mbcn")]
dat_plot2[, bads_ds := bads %>% forcats::fct_recode(
  "ds-pcalm" = "ba-qdm-ds-pcalm",
  "ds-qdm2" = "ba-qdm-ds-qdm2",
  "bads" = "bads-qdm"
)]

dat_plot2[, bads_ds := factor(bads_ds, levels = c(
  "bads", "ds-qdm", "ds-qdm2", "ds-pcalm", "ds-lr", "ds-gam"
))]

# dat_plot2[, rcm_short := eurocordexr::shortnames_rcm[institute_rcm]]
# dat_plot2[, institute := str_remove(institute_rcm, rcm_short)]
# dat_plot2[, institute := str_remove(institute, "-$")]

# dat_ba2[, bads2 := str_replace(bads, "ba-qdm-", "ba-qdm-\n")]

rm(dat_ba_randyear, dat_plot); gc()

dat_plot2[, randyear_fct := fct_relevel(randyear, str_c(0:20))]
dat_plot2[, randyear2 := ifelse(randyear == "0", "initial sampling", "20 replications")]
dat_plot2[, randyear2_fct := fct_relevel(randyear2, "initial sampling")]


# subset for main results -------------------------------------------------

# mam tasmax, pr mam

gg1 <-
  dat_plot2[variable == "pr" & season == "MAM"] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = bias_rel))+
  geom_raster()+
  scale_fill_scico("bias", 
                   palette = "vik", 
                   midpoint = 0, 
                   labels = scales::label_percent(),
                   direction = -1)+
  facet_grid(randyear_fct ~ bads)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())+
  xlab(NULL)+ylab(NULL)+
  ggtitle("pr MAM")


gg2 <-
  dat_plot2[variable == "pr" & season == "MAM"] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = mae))+
  geom_raster()+
  scale_fill_scico("MAE [mm]", 
                   palette = "bamako")+
  facet_grid(randyear_fct ~ bads)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())+
  xlab(NULL)+ylab(NULL)


gg3 <-
  dat_plot2[variable == "pr" & season == "MAM"] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = corr))+
  geom_raster()+
  scale_fill_scico("correlation", 
                   palette = "batlow")+
  facet_grid(randyear_fct ~ bads)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())+
  xlab(NULL)+ylab(NULL)



gg_out <- (gg1+gg2+gg3)+
  plot_layout(nrow = 1)&
  theme(legend.position = "bottom")


ggsave(filename = "fig/paper-ds-rev1/randyear-maps_pr.png",
       gg_out,
       width = 12, height = 20)



gg4 <-
  dat_plot2[variable == "tasmax" & season == "MAM"] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = bias))+
  geom_raster()+
  scale_fill_scico("bias [°C]", 
                   palette = "vik", 
                   midpoint = 0, 
                   direction = +1,
                   limits = c(-2,2),
                   oob = scales::oob_squish)+
  facet_grid(randyear_fct ~ bads)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())+
  xlab(NULL)+ylab(NULL)+
  ggtitle("tasmax MAM")


gg5 <-
  dat_plot2[variable == "tasmax" & season == "MAM"] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = mae))+
  geom_raster()+
  scale_fill_scico("MAE [°C]", 
                   palette = "bamako",
                   limits = c(NA,5),
                   oob = scales::oob_squish)+
  facet_grid(randyear_fct ~ bads)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())+
  xlab(NULL)+ylab(NULL)


gg6 <-
  dat_plot2[variable == "tasmax" & season == "MAM"] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = corr))+
  geom_raster()+
  scale_fill_scico("correlation", 
                   palette = "batlow")+
  # limits = c(-2,2),
  # oob = scales::oob_squish)+
  facet_grid(randyear_fct ~ bads)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())+
  xlab(NULL)+ylab(NULL)


gg_out <- (gg4+gg5+gg6)+
  plot_layout(nrow = 1)&
  theme(legend.position = "bottom")


ggsave(filename = "fig/paper-ds-rev1/randyear-maps_tasmax.png",
       gg_out,
       width = 12, height = 20)


