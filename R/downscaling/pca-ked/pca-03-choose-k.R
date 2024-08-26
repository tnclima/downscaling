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
suffix <- "centerTRUE-scaleTRUE"

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




# check PC1 temp ----------------------------------------------------------

# PC1 is elevation for tas*
dat2[variable == "tasmin"] %>%
  ggplot(aes(orog, value))+
  # geom_point()+
  geom_bin2d()+
  facet_grid(season ~ pc)+
  theme_bw()

# check slope (-> same in all seasons if center==TRUE, different if center==FALSE)
dat2[variable == "tasmin" & pc == "PC1"] %>% 
  ggplot(aes(orog, value))+
  # geom_point()+
  geom_bin2d()+
  geom_smooth(method = lm)+
  facet_wrap( ~ season)+
  theme_bw()

dat2[variable == "tasmin" & pc == "PC1", 
     broom::tidy(lm(value ~ orog)), 
     .(season)]




# expvar ------------------------------------------------------------------


dat_expvar <- rbind(
  cbind(readRDS(str_c("/home/climatedata/downscaling/pca-ked/crespi-pca-varexp/season-sub", nn, "-pr-", suffix, ".rds")), variable = "pr"),
  cbind(readRDS(str_c("/home/climatedata/downscaling/pca-ked/crespi-pca-varexp/season-sub", nn, "-tasmax-", suffix, ".rds")), variable = "tasmax"),
  cbind(readRDS(str_c("/home/climatedata/downscaling/pca-ked/crespi-pca-varexp/season-sub", nn, "-tasmin-", suffix, ".rds")), variable = "tasmin")
)

dat_expvar[, pc_num := str_remove(pc, "PC") %>% as.numeric]
dat_expvar[, season := factor(season, levels = c("DJF", "MAM", "JJA", "SON"))]


# dat_expvar[variable == "tasmin" & season == "DJF" & pc_num != 1] %>%
#   ggplot(aes(pc_num, prop_sd))+
#   geom_point()+
#   geom_line()+
#   theme_bw()
# 
# dat_expvar[variable == "tasmin" & season == "DJF" ] %>%
#   ggplot(aes(pc_num, log(prop_sd)))+
#   geom_point()+
#   geom_line()+
#   theme_bw()


dat_expvar[!(variable != "pr" & pc_num == 1)] %>% 
  ggplot(aes(pc_num, prop_sd, colour = season))+
  geom_point()+
  geom_line()+
  facet_wrap(~ variable, scales = "free_y")+
  xlim(0, 20)+
  theme_bw()+
  xlab("PC")+
  ylab("Exp Var per PC (not cumulative)")

ggsave(str_c("fig/pca-ked/pca-results/season", nn, "-expvarSinglePC-", suffix, ".png"),
       width = 12, height = 4)





