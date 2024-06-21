# some figures for EGU24

library(eurocordexr)
library(data.table)
setDTthreads(4)
library(magrittr)
library(ggplot2)
library(patchwork)
library(scico)

# data --------------------------------------------------------------------

dat_icell <- readRDS("/home/climatedata/downscaling/validation-cv/rdata-summary-v1/eval-08-aux-icell-1km-011deg.rds")
dat_aux <- dat_icell[!is.na(orog), .(icell = icell_1km, x, y, orog)]


dt_qm <- nc_grid_to_dt("/home/climatedata/downscaling/validation-cv/data-clim/qm_tasmin_3_SMHI-RCA4_MOHC-HadGEM2-ES_rcp85.nc",
                       icell_raster_pkg = F)
dt_gam <- nc_grid_to_dt("/home/climatedata/downscaling/validation-cv/data-clim/lrvar_tasmin_3_SMHI-RCA4_MOHC-HadGEM2-ES_rcp85.nc",
                        icell_raster_pkg = F)
dt_lr <- nc_grid_to_dt("/home/climatedata/downscaling/validation-cv/data-clim/lrfix_tasmin_3_SMHI-RCA4_MOHC-HadGEM2-ES_rcp85.nc",
                       icell_raster_pkg = F)

dt_ds <- rbind(
  dt_qm[, .(icell, date, tasmin, ff = "qm")],
  dt_gam[, .(icell, date, tasmin = tasmin - 273.15, ff = "gam")],
  dt_lr[, .(icell, date, tasmin = tasmin - 273.15, ff = "lr")]
) %>% 
  merge(dat_aux)

dt_ds[, month_fct := mitmatmisc::month_fct(month(date))]

dt_crespi <- nc_grid_to_dt("/home/climatedata/downscaling/validation-cv/data-clim/crespi_tasmin.nc",
                           icell_raster_pkg = F)
dt_crespi[, month_fct := mitmatmisc::month_fct(month(date))]


# plot --------------------------------------------------------------------

i_month <- "Jan"
i_month <- "Jul"


gg_ds <-
dt_ds[month_fct == i_month] %>% 
  ggplot(aes(x, y, fill = tasmin))+
  geom_raster()+
  scale_fill_viridis_c(direction = -1, option = "A")+
  facet_grid(month_fct ~ ff)+
  coord_fixed()+
  theme_bw(20)+
  xlab(NULL)+ylab(NULL)

# dt_ds[month_fct == "Jul"] %>% 
#   ggplot(aes(x, y, fill = tasmin))+
#   geom_raster()+
#   scale_fill_viridis_c(direction = -1, option = "A")+
#   facet_grid(month_fct ~ ff)+
#   coord_fixed()+
#   theme_bw()+
#   xlab(NULL)+ylab(NULL)

dt_ds2 <- dt_ds %>% 
  dcast(icell + month_fct + x + y + orog ~ ff, value.var = "tasmin")

lims_diff <- range(dt_ds2[, gam - qm], dt_ds2[, gam - lr], dt_ds2[, lr - qm])

gg_ds_diff1 <- dt_ds2[month_fct == i_month] %>% 
  ggplot(aes(x, y, fill = gam - qm))+
  geom_raster()+
  scale_fill_scico(palette = "vik", limits = lims_diff, midpoint = 0)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)

gg_ds_diff2 <- dt_ds2[month_fct == i_month] %>% 
  ggplot(aes(x, y, fill = gam - lr))+
  geom_raster()+
  scale_fill_scico(palette = "vik", limits = lims_diff, midpoint = 0)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)

gg_ds_diff3 <- dt_ds2[month_fct == i_month] %>% 
  ggplot(aes(x, y, fill = lr - qm))+
  geom_raster()+
  scale_fill_scico(palette = "vik", limits = lims_diff, midpoint = 0)+
  coord_fixed()+
  theme_bw()+
  xlab(NULL)+ylab(NULL)


gg_out2 <- gg_ds_diff1 | gg_ds_diff2 | gg_ds_diff3



ggsave(paste0("fig/example-downscaling/egu24-03-clim-", i_month, ".png"), gg_ds, width = 16, height = 4)
ggsave(paste0("fig/example-downscaling/egu24-04-climdiff-", i_month, ".png"), gg_out2, width = 16, height = 4)



# diff crespi -------------------------------------------------------------

# dt_crespi[month_fct == i_month] %>% 
#   merge(dat_aux) %>% 
#   ggplot(aes(x,y,fill = temperature))+
#   geom_raster()

dt_ds_crespi <- dt_ds %>% 
  merge(dt_crespi[, .(icell, month_fct, tasmin_crespi = temperature)], by = c("icell", "month_fct"))


dt_ds_crespi[month_fct == i_month] %>% 
  ggplot(aes(x, y, fill = tasmin - tasmin_crespi))+
  geom_raster()+
  scale_fill_scico(palette = "vik", midpoint = 0)+
  facet_grid(month_fct ~ ff)+
  coord_fixed()+
  theme_bw(20)+
  xlab(NULL)+ylab(NULL)

