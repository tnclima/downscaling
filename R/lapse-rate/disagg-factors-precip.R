# calc disaggregation factors precipitation

library(terra)
library(magrittr)



rs_crespi <- rast("/home/climatedata/obs/CRESPI/daily_1km_lonlat/Climatologies_1991_2020_MonthlyPrec.nc")
rs_rcm <- rast("/home/climatedata/downscaling/rcm_lonlat_tnaa/orog/orog_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r0i0p0_CLMcom-CCLM4-8-17_v1_fx.nc")
rs_apgd <- rast("/home/climatedata/obs/APGD/APGD_laea_vertices_monthly_clim.nc")

rs_rcm[] <- 1:ncell(rs_rcm)
rs_rcm_obs <- resample(rs_rcm, rs_crespi, "near")


# apgd --------------------------------------------------------------------

# with bilinear interpolation of APGD onto 1km Crespi grid

rs_apgd_obs <- project(rs_apgd, rs_crespi)

rs_apgd_obs_avg <- zonal(rs_apgd_obs, rs_rcm_obs, na.rm = T, as.raster = T)
rs_frac_apgd <- rs_apgd_obs / rs_apgd_obs_avg

# rs_frac[is.na(rs)] <- NA
# rs_frac %>% plot

writeCDF(rs_frac_apgd, "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/apgd_precip_disagg_factors.nc")


# crespi ------------------------------------------------------------------


# not working, needs fraction relative to whole rcm cell

rs_crespi[[1]] %>% plot
rs_frac_crespi[[1]] %>% plot

rs_crespi_avg <- zonal(rs_crespi, rs_rcm_obs, na.rm = T, as.raster = T)

rs_frac_crespi <- rs_crespi/rs_crespi_avg




