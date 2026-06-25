# stacked crespi precip chart

library(terra)
library(ggplot2)
library(dplyr)
library(scico)

library(eurocordexr)
library(data.table)
library(fs)
library(foreach)
library(patchwork)
library(forcats)
library(stringr)

# stacked precip ----------------------------------------------------------



r <- rast("/home/climatedata/stations/hourly-v2-euregio/11-grids/test-03-precip-2y/pr_crespi_2015.nc")
set.seed(1)
i_sel <- sample(1:365, 5)
i_sel <- c(204, 34, 12, 270, 178)
i_sel <- c(204:209)
r2 <- r[[i_sel]]

r3 <- project(r2, "EPSG:3857")
df <- as.data.frame(r3, xy = TRUE, na.rm = TRUE)
names(df)[3:7] <- paste0("value", 1:5)

dy <- diff(ext(r3)[3:4]) * 0.06   # 12% of raster height
dx <- diff(ext(r3)[1:2]) * 0.06   # 12% of raster height
d1 <- df %>% mutate(y = y + 0*dy, layer = "L1")
d2 <- df %>% mutate(y = y + 1*dy, x = x + 1*dx, layer = "L2")
d3 <- df %>% mutate(y = y + 2*dy, x = x + 2*dx,layer = "L3")
d4 <- df %>% mutate(y = y + 3*dy, x = x + 3*dx,layer = "L4")
d5 <- df %>% mutate(y = y + 4*dy, x = x + 4*dx,layer = "L5")

ggplot() +
  geom_raster(data = d5, aes(x, y, fill = value5)) +
  geom_raster(data = d4, aes(x, y, fill = value4)) +
  geom_raster(data = d3, aes(x, y, fill = value3)) +
  geom_raster(data = d2, aes(x, y, fill = value2)) +
  geom_raster(data = d1, aes(x, y, fill = value1)) +
  scale_fill_scico(palette = "davos", direction = 1)+
  facet_grid(. ~ "40 years observations")+
  coord_equal() +
  theme_void()+
  theme(legend.position = "none",
        strip.text = element_text(size = 20))

ggsave("fig/other/rmc-01-stacked-precip.png", width = 6, height = 4, bg = "white")



# first 4 PCs -------------------------------------------------------------

dat_aux <- nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc", add_xy = T)
dat_aux <- dat_aux[, .(icell, x = longitude, y = latitude, orog)]
dat1 <- readRDS("/home/climatedata/downscaling/pca-ked/crespi-pca/final-choice/season-sub0-pr-centerTRUE-scaleFALSE.rds")

dat2 <- dat1 %>% 
  melt(id.vars = c("variable", "season", "icell"), measure.vars = paste0("PC",1:4),
       variable.name = "pc") %>% 
  merge(dat_aux)

dat2[, season := factor(season, levels = c("DJF", "MAM", "JJA", "SON"))]
dat2[, value_sc := scale(value), .(variable, season, pc)]

dat2[season == "SON"] %>%
  ggplot(aes(x, y, fill = value))+
  geom_raster()+
  facet_grid(. ~ pc)+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  coord_quickmap()+
  theme_void()+
  theme(legend.position = "none",
        strip.text = element_text(size = 20))+
  xlab(NULL)+
  ylab(NULL)

ggsave("fig/other/rmc-02-pca-son.png", width = 10, height = 3, bg = "white")



# one day RCM plus coef of PCs --------------------------------------------

dat_coef <- readRDS("data/coef-bathends-pcalm.rds")
dat_coef$ba |> table()

dat_coef1 <- dat_coef[institute_rcm == "GERICS-REMO2015" & date == "2003-11-01" & variable == "pr"]
cc <- dat_coef1[term == "(Intercept)", estimate]
dat_pc1 <- dat2[season == "SON"] |> merge(dat_coef1[, .(pc = term, estimate)], by = "pc")

# dat_coef1 <- dat_coef[institute_rcm == "GERICS-REMO2015" & date == "2003-03-31" & variable == "pr"]
# cc <- dat_coef1[term == "(Intercept)", estimate]
# dat_pc1 <- dat2[season == "MAM"] |> merge(dat_coef1[, .(pc = term, estimate)], by = "pc")

dat_pc2 <- dat_pc1[, .(value = (sum(value*estimate)+cc)**2), .(icell, x, y)]

dat_pc1[, facet_lbl := sprintf("%0.3f * %s", estimate, pc)]
dat_pc1[, facet_lbl_fct := fct_inorder(facet_lbl)]

dat_pc1 |> 
  ggplot(aes(x, y, fill = value*estimate))+
  geom_raster()+
  facet_grid(. ~ facet_lbl_fct)+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  coord_quickmap()+
  theme_void()+
  theme(legend.position = "none",
        strip.text = element_text(size = 20))+
  xlab(NULL)+
  ylab(NULL)

ggsave("fig/other/rmc-03-pca-coef.png", width = 10, height = 3, bg = "white")



dat_pc2 |> 
  ggplot(aes(x, y, fill = value))+
  geom_raster()+
  scale_fill_scico(palette = "davos")+
  facet_grid(. ~ "SUM(.)")+
  coord_quickmap()+
  theme_void()+
  theme(legend.position = "none",
        strip.text = element_text(size = 20))+
  xlab(NULL)+
  ylab(NULL)

ggsave("fig/other/rmc-04-pca-sum.png", width = 4, height = 3, bg = "white")



# RCM 

file_rcm_orog <- "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eobs.nc"
dat_aux_011 <- nc_grid_to_dt(file_rcm_orog,
                             add_xy = T)
dat_aux_011[, date := NULL]

fn_rcm <- "/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v2/ba-mbcnspat/pr_GERICS-REMO2015_ECMWF-ERAINT_evaluation.nc"
dat_rcm <- nc_grid_to_dt(fn_rcm, date_range = c("2003-11-01", "2003-11-01"))
setnames(dat_rcm, 3, "value")

dat_rcm[!is.na(value)] |> merge(dat_aux_011) |> 
  ggplot(aes(lon, lat, fill = value))+
  geom_raster()+
  scale_fill_scico(palette = "davos")+
  facet_grid(. ~ "RCM")+
  coord_quickmap()+
  theme_void()+
  theme(legend.position = "none",
        strip.text = element_text(size = 20))+
  xlab(NULL)+
  ylab(NULL)

ggsave("fig/other/rmc-05-rcm.png", width = 4, height = 3, bg = "white")



# upscaled PCs
rs_template_rcm <- rast(file_rcm_orog)
rs_template_1km <- rast("/home/climatedata/obs/orography/crespi_lonlat_1km_precipitation.nc")

rs_pca_1km <- rast(rs_template_1km, nlyrs = 4)
rs_pca_1km[
  dat1[season == "SON", icell]
] <- dat1[season == "SON", str_c("PC", 1:4), with=F]

rs_pca_1km %>% flip %>% 
  aggregate(fact = 10) %>%
  resample(rs_template_rcm) -> rs_pca_rcm

dat_pc_upscaled <- rs_pca_rcm %>% as.data.table(cells = T, na.rm = F)
setnames(dat_pc_upscaled, c("icell", str_c("PC", 1:4)))

dat_pc_upscaled[!is.na(PC1)] |> 
  melt(id.vars = "icell", variable.factor = F, variable.name = "pc") |> 
  merge(dat_aux_011) |> 
  ggplot(aes(lon, lat, fill = value))+
  geom_raster()+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  facet_grid(. ~ pc)+
  coord_quickmap()+
  theme_void()+
  theme(legend.position = "none",
        strip.text = element_text(size = 20))+
  xlab(NULL)+
  ylab(NULL)

ggsave("fig/other/rmc-06-upscaled-pc.png", width = 10, height = 3, bg = "white")

