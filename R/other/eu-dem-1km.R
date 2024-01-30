# EU-DEM 1km

library(terra)

rs_tnaa <- rast("/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc")
rs_gar <- rast("/home/climatedata/obs/orography/eu_dem_gar_1km.nc")

rs_gar_tnaa <- project(rs_gar, rs_tnaa)

writeCDF(rs_gar_tnaa, 
         "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc",
         varname = "orog", unit = "m")
