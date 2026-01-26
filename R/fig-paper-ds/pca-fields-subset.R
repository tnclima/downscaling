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
  readRDS("/home/climatedata/downscaling/pca-ked/crespi-pca/final-choice/season-sub0-pr-centerTRUE-scaleFALSE.rds"),
  readRDS("/home/climatedata/downscaling/pca-ked/crespi-pca/final-choice/season-sub0-tasmax-centerTRUE-scaleFALSE.rds"),
  readRDS("/home/climatedata/downscaling/pca-ked/crespi-pca/final-choice/season-sub0-tasmin-centerTRUE-scaleFALSE.rds")
)

dat2 <- dat1 %>% 
  melt(id.vars = c("variable", "season", "icell"), measure.vars = paste0("PC",1:9),
       variable.name = "pc") %>% 
  merge(dat_aux)

dat2[, season := factor(season, levels = c("DJF", "MAM", "JJA", "SON"))]
dat2[, value_sc := scale(value), .(variable, season, pc)]



# plot --------------------------------------------------------------------




gg_pr <-
dat2[variable == "pr" & season == "DJF"] %>%
  ggplot(aes(x, y, fill = value))+
  geom_raster()+
  facet_wrap( ~ pc, nrow = 2)+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  coord_quickmap()+
  theme_bw()+
  theme(plot.title.position = "plot")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("a) Precipitation DJF")




gg_tasmin1 <-
dat2[variable == "tasmin" & pc == "PC1" & season == "DJF"] %>%
  ggplot(aes(x, y, fill = value))+
  geom_raster()+
  facet_wrap( ~ pc)+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  coord_quickmap()+
  theme_bw()+
  theme(plot.title.position = "plot")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("b) Minimum temperature DJF")

gg_tasmin2 <-
  dat2[variable == "tasmin" & !pc %in% str_c("PC", c(1)) & season == "DJF"] %>%
  ggplot(aes(x, y, fill = value))+
  geom_raster()+
  facet_wrap( ~ pc, nrow = 2)+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  coord_quickmap()+
  theme_bw()+
  xlab(NULL)+
  ylab(NULL)

gg_tas <- (gg_tasmin1 | gg_tasmin2)+
  plot_layout(widths = c(1.2,2))# +
  # plot_annotation(title = "b) Minimum temperature DJF")

# ((gg_pr+plot_annotation(title = "a) Precipitation DJF"))+gg_tas)+
  # plot_layout(tag_level = "keep")

gg_out <- wrap_plots(gg_pr, gg_tas, ncol = 1, heights = c(2,1)) &
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())
  

ggsave("fig/paper-ds/pca-djf-subset.png", gg_out, width = 8, height = 6)

