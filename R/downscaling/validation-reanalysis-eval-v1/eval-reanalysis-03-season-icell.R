#

library(data.table)
setDTthreads(4)
library(ggplot2)
library(magrittr)
library(fs)
library(stringr)
library(scico)

sub_pctl <- c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)

# data --------------------------------------------------------------------

dat_aux <- eurocordexr::nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc",
                                        add_xy = T)
dat_aux <- dat_aux[!is.na(orog), .(icell, x = longitude, y = latitude, orog)]

dat_aux_011 <- eurocordexr::nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eobs.nc",
                                          add_xy = T)
dat_aux_011[, date := NULL]

dat_crespi <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-crespi-season-icell.rds")
dat_only_ba <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-ba-season-icell.rds")
dat_rcm_raw <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-rcm-raw-season-icell.rds")

dat_crespi[variable %in% c("pr", "hn") & qval < 0, qval := 0]
dat_crespi <- dat_crespi[pctl %in% sub_pctl]

bads_sub <- c("ba-qdm-ds-gam", "ba-qdm-ds-lr", "ba-qdm-ds-pcalm", "ba-qdm-ds-qdm", "bads-qdm")
bads_sub_hn <- c("ba-qdm-ds-pcalm", "ba-mbcn-ds-pcalm", 
                 "ba-qdm-ds-qdm", "ba-mbcn-ds-qdm", 
                 "bads-qdm", "bads-mbcn")

# takes very long -> save?
# 
# dat_bads <- dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-season-icell/") %>% 
#   lapply(\(i_path){
#     dir_ls(i_path) %>% 
#       lapply(\(x){
#         readRDS(x) %>% 
#           .[pctl %in% sub_pctl] %>% 
#           cbind(institute_rcm = x %>% path_file() %>% path_ext_remove(),
#                 bads = i_path %>% path_file)
#       }) %>% rbindlist()
#   }) %>% rbindlist
# 
# 
# dat_bads2 <- dat_bads[bads %in% unique(c(bads_sub, bads_sub_hn)) & 
#                         !(variable %in% c("pr", "hn") & pctl < 0.5)]

# saveRDS(dat_bads2, "/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-season-icell/sub-pctl.rds")
dat_bads2 <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-season-icell/sub-pctl.rds")

# ba only -----------------------------------------------------------------


dat_plot <- dat_rcm_raw[pctl %in% sub_pctl] %>% 
  cbind(ba = "raw") %>% 
  rbind(dat_only_ba[pctl %in% sub_pctl])


dat_plot[variable == "pr" & season == "JJA" & pctl == 0.75] %>% 
  merge(dat_aux_011) %>% 
  ggplot(aes(lon, lat, fill = qval))+
  geom_raster()+
  scale_fill_viridis_c()+
  facet_grid(ba ~ institute_rcm)+
  theme_bw()+
  coord_fixed()




# tasmin ----------------------------------------------------------------------


dat_plot1 <- dat_bads2[variable == "tasmin" & bads %in% bads_sub] %>% 
  merge(dat_crespi[variable == "tasmin", .(icell, season, variable, qval_crespi = qval, pctl)])


dat_plot1[season == "JJA" & pctl == 0.99] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = qval - qval_crespi))+
  geom_raster()+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  facet_grid(bads ~ institute_rcm)+
  theme_bw()+
  coord_fixed()

# tasmax ----------------------------------------------------------------------


dat_plot2 <- dat_bads2[variable == "tasmax" & bads %in% bads_sub] %>% 
  merge(dat_crespi[variable == "tasmax", .(icell, season, variable, qval_crespi = qval, pctl)])


dat_plot2[season == "JJA" & pctl == 0.5] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = qval - qval_crespi))+
  geom_raster()+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  facet_grid(bads ~ institute_rcm)+
  theme_bw()+
  coord_fixed()



# pr ----------------------------------------------------------------------


dat_plot3 <- dat_bads2[variable == "pr" & bads %in% bads_sub] %>% 
  merge(dat_crespi[variable == "pr", .(icell, season, variable, qval_crespi = qval, pctl)])


dat_plot3[season %in% c("DJF", "JJA") & pctl == 0.99] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = (qval - qval_crespi)/qval_crespi))+
  geom_raster()+
  scale_fill_scico(palette = "vik", midpoint = 0, limits = c(-2, 2), oob = scales::oob_squish)+
  facet_grid(season + bads ~ institute_rcm)+
  theme_bw()+
  coord_fixed()



# hn ----------------------------------------------------------------------

dat_plot4 <- dat_bads2[variable == "hn" & bads %in% bads_sub_hn] %>% 
  merge(dat_crespi[variable == "hn", .(icell, season, variable, qval_crespi = qval, pctl)])


dat_plot4[season %in% c("MAM") & pctl == 0.75] %>% 
  merge(dat_aux, by = "icell") %>% 
  ggplot(aes(x,y, fill = (qval - qval_crespi)/qval_crespi))+
  geom_raster()+
  scale_fill_scico(palette = "vik", midpoint = 0, limits = c(-2, 2), oob = scales::oob_squish)+
  facet_grid(season + bads ~ institute_rcm)+
  theme_bw()+
  coord_fixed()



