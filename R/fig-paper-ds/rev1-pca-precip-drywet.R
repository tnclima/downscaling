# overview PCA Crespi

library(eurocordexr)
library(data.table)
setDTthreads(4)
library(magrittr)
library(ggplot2)
library(fs)
library(foreach)
library(scico)
library(patchwork)
library(forcats)
library(stringr)


dat_aux <- nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc", add_xy = T)
dat_aux <- dat_aux[, .(icell, x = longitude, y = latitude, orog)]
# dat_aux %>% ggplot(aes(x,y,fill=orog))+geom_raster()


dat1 <- rbind(
  cbind(readRDS("/home/climatedata/downscaling/pca-ked/crespi-pca/final-choice/season-sub0-pr-centerTRUE-scaleFALSE.rds"), ff = "all"),
  cbind(readRDS("/home/climatedata/downscaling/pca-ked/crespi-pca/testing-wet-dry/nodry-pr-centerTRUE-scaleFALSE.rds"), ff = "no dry"),
  cbind(readRDS("/home/climatedata/downscaling/pca-ked/crespi-pca/testing-wet-dry/allwet-pr-centerTRUE-scaleFALSE.rds"), ff = "full wet")
)

dat2 <- dat1 %>% 
  melt(id.vars = c("variable", "season", "icell", "ff"), measure.vars = paste0("PC",1:9),
       variable.name = "pc") %>% 
  merge(dat_aux)

dat2[, season := factor(season, levels = c("DJF", "MAM", "JJA", "SON"))]
# dat2[, value_sc := scale(value), .(variable, season, pc)]
dat2[, ff := factor(ff, levels = c("all", "no dry", "full wet"))]


# plot --------------------------------------------------------------------




gg_pr1 <-
dat2[variable == "pr" & season == "DJF" & pc %in% str_c("PC", 1:5)] %>%
  ggplot(aes(x, y, fill = value))+
  geom_raster()+
  # facet_grid(pc ~ ff)+
  facet_grid(ff ~ pc)+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  coord_quickmap()+
  theme_bw()+
  theme(plot.title.position = "plot")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("a) Precipitation DJF")



gg_pr2 <-
  dat2[variable == "pr" & season == "JJA" & pc %in% str_c("PC", 1:5)] %>%
  ggplot(aes(x, y, fill = value))+
  geom_raster()+
  # facet_grid(pc ~ ff)+
  facet_grid(ff ~ pc)+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  coord_quickmap()+
  theme_bw()+
  theme(plot.title.position = "plot")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("b) Precipitation JJA")




gg_out <- wrap_plots(gg_pr1, gg_pr2, ncol = 1) &
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())
  

ggsave("fig/paper-ds-rev1/pca-precip-drywet.png", gg_out, width = 7, height = 8)

