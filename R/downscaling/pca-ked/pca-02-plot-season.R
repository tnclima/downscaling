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

nn <- "1000"
suffix <- "centerFALSE-scaleTRUE"

dat_aux <- nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc", add_xy = T)
dat_aux <- dat_aux[, .(icell, x = longitude, y = latitude, orog)]
# dat_aux %>% ggplot(aes(x,y,fill=orog))+geom_raster()


dat1 <- rbind(
  readRDS(str_c("/home/climatedata/downscaling/pca-ked/crespi-pca/season-sub", nn, "-pr-", suffix, ".rds")),
  readRDS(str_c("/home/climatedata/downscaling/pca-ked/crespi-pca/season-sub", nn, "-tasmax-", suffix, ".rds")),
  readRDS(str_c("/home/climatedata/downscaling/pca-ked/crespi-pca/season-sub", nn, "-tasmin-", suffix, ".rds"))
)

dat2 <- dat1 %>% 
  melt(id.vars = c("variable", "season", "icell"), measure.vars = paste0("PC",1:9),
       variable.name = "pc") %>% 
  merge(dat_aux)

dat2[, season := factor(season, levels = c("DJF", "MAM", "JJA", "SON"))]
dat2[, value_sc := scale(value), .(variable, season, pc)]



# tasmax --------------------------------------------------------------------

gg1 <- dat2[variable == "tasmax" & pc == "PC1"] %>%
  # dat2[variable == "tasmax"] %>%
  ggplot(aes(x, y, fill = value))+
  geom_raster()+
  facet_grid(season ~ pc)+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  coord_quickmap()+
  theme_bw()+
  xlab(NULL)+
  ylab(NULL)

gg2 <- dat2[variable == "tasmax" & pc != "PC1"] %>%
  # dat2[variable == "tasmax"] %>%
  ggplot(aes(x, y, fill = value))+
  geom_raster()+
  facet_grid(season ~ pc)+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  coord_quickmap()+
  theme_bw()+
  xlab(NULL)+
  ylab(NULL)

gg_out <- (gg1 | gg2) + plot_layout(widths = c(1,8))

ggsave(str_c("fig/pca-ked/pca-results/season-sub", nn, "-tasmax-", suffix, ".png"),
       gg_out,
       width = 14, height = 5)


# tasmin --------------------------------------------------------------------

gg1 <- dat2[variable == "tasmin" & pc == "PC1"] %>%
  ggplot(aes(x, y, fill = value))+
  geom_raster()+
  facet_grid(season ~ pc)+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  coord_quickmap()+
  theme_bw()+
  xlab(NULL)+
  ylab(NULL)

gg2 <- dat2[variable == "tasmin" & pc != "PC1"] %>%
  ggplot(aes(x, y, fill = value))+
  geom_raster()+
  facet_grid(season ~ pc)+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  coord_quickmap()+
  theme_bw()+
  xlab(NULL)+
  ylab(NULL)

gg_out <- (gg1 | gg2) + plot_layout(widths = c(1,8))

ggsave(str_c("fig/pca-ked/pca-results/season-sub", nn, "-tasmin-", suffix, ".png"),
       gg_out,
       width = 14, height = 5)

# pr ------------------------------------------------------------------

gg_out <- dat2[variable == "pr"] %>%
  ggplot(aes(x, y, fill = value))+
  geom_raster()+
  facet_grid(season ~ pc)+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  coord_quickmap()+
  theme_bw()+
  xlab(NULL)+
  ylab(NULL)

ggsave(str_c("fig/pca-ked/pca-results/season-sub", nn, "-pr-", suffix, ".png"),
       gg_out,
       width = 14, height = 5)




# expvar ------------------------------------------------------------------

dat_expvar <- rbind(
  cbind(readRDS(str_c("/home/climatedata/downscaling/pca-ked/crespi-pca-varexp/season-sub", nn, "-pr-", suffix, ".rds")), variable = "pr"),
  cbind(readRDS(str_c("/home/climatedata/downscaling/pca-ked/crespi-pca-varexp/season-sub", nn, "-tasmax-", suffix, ".rds")), variable = "tasmax"),
  cbind(readRDS(str_c("/home/climatedata/downscaling/pca-ked/crespi-pca-varexp/season-sub", nn, "-tasmin-", suffix, ".rds")), variable = "tasmin")
)

dat_expvar[, pc_num := str_remove(pc, "PC") %>% as.numeric]

dat_expvar %>% 
  ggplot(aes(pc_num, cumsum_prop_sd, colour = season))+
  # geom_point()+
  geom_line(aes(group = season))+
  # facet_grid(variable ~ season)+
  facet_wrap(~ variable)+
  xlim(0, 20)+
  theme_bw()+
  xlab("PC")+
  ylab("Exp Var")

ggsave(str_c("fig/pca-ked/pca-results/season", nn, "-expvar-", suffix, ".png"),
       width = 12, height = 4)

