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

th <- 0.1 # threshold for precip occurrence
n_sub_dates <- 0 # 0 for no subsetting
main_effects <- T # use main effects in PCA? probably equivalent to scaling

dat_aux <- nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc", add_xy = T)
dat_aux <- dat_aux[, .(icell, x = longitude, y = latitude, orog)]
# dat_aux %>% ggplot(aes(x,y,fill=orog))+geom_raster()

# 
# dat1 <- rbind(
#   readRDS(str_c("/home/climatedata/downscaling/pca-ked/crespi-pca/season-sub", nn, "-pr-", suffix, ".rds")),
#   readRDS(str_c("/home/climatedata/downscaling/pca-ked/crespi-pca/season-sub", nn, "-tasmax-", suffix, ".rds")),
#   readRDS(str_c("/home/climatedata/downscaling/pca-ked/crespi-pca/season-sub", nn, "-tasmin-", suffix, ".rds"))
# )

dat1 <- readRDS(str_c(
  "/home/climatedata/downscaling/pca-ked/crespi-pca/pcalog-season-sub",
  n_sub_dates, "-pr-th", th, "-maineffects", main_effects, ".rds"
  ))

dat2 <- dat1 %>% 
  melt(id.vars = c("variable", "season", "icell"), measure.vars = paste0("PC",1:9),
       variable.name = "pc") %>% 
  merge(dat_aux)

dat2[, season := factor(season, levels = c("DJF", "MAM", "JJA", "SON"))]
dat2[, value_sc := scale(value), .(variable, season, pc)]




# pr ------------------------------------------------------------------

gg_out <-
dat2 %>%
  ggplot(aes(x, y, fill = value))+
  geom_raster()+
  facet_grid(season ~ pc)+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  coord_quickmap()+
  theme_bw()+
  xlab(NULL)+
  ylab(NULL)

ggsave(str_c("fig/pca-ked/pca-results/pcalog-season-sub",
             n_sub_dates, "-pr-th", th, "-maineffects", main_effects, ".png"),
       gg_out,
       width = 14, height = 5)


