# run an example lapse-rate downscaling

library(terra)
library(magrittr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(fs)
source("R/functions/create_empty_netcdf.R")
source("R/functions/lapse_rate_varying.R")

# settings - variables ----------------------------------------------------

kk_basis <- 5

i_var <- "tasmax"
l_nc_info <- list(name = i_var,
                  units = "degC",
                  longname = "Daily Maximum Near-Surface Air Temperature",
                  cell_methods = "time: maximum",
                  standard_name = "air_temperature")

file_rcm_orog <- "/home/climatedata/downscaling/rcm_lonlat_tnaa/orog/orog_EUR-11_CNRM-CERFACS-CNRM-CM5_rcp85_r1i1p1_CNRM-ALADIN63_v2_fx.nc"
file_rcm <- "/home/climatedata/downscaling/rcm_lonlat_tnaa/tasmax/tasmax_EUR-11_CNRM-CERFACS-CNRM-CM5_rcp26_r1i1p1_CNRM-ALADIN63_v2_day_19510101-21001231.nc"
# file_rcm_orog <- "/home/climatedata/downscaling/rcm_lonlat_tnaa/orog/orog_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_DMI-HIRHAM5_v1_fx.nc"
# file_rcm <- "/home/climatedata/downscaling/rcm_lonlat_tnaa/tasmax/tasmax_EUR-11_MOHC-HadGEM2-ES_rcp26_r1i1p1_DMI-HIRHAM5_v2_day_19510101-20991230.nc"

file_obs_orog <- "/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc"

file_out <- "/home/climatedata/downscaling/zz_temp/test-lr-tasmax-kk5.nc"







# data --------------------------------------------------------------------

rs_rcm_orog <- rast(file_rcm_orog)
rs_obs_orog <- rast(file_obs_orog)

cells_obs <- which(!is.na(rs_obs_orog[]))
cells_obs_na <- which(is.na(rs_obs_orog[]))

rs_rcm <- rast(file_rcm)
mat_rcm <- values(rs_rcm, mat = T)
# vals_rcm_orog <- values(rs_rcm_orog, mat = F)
# vals_obs_orog <- values(rs_obs_orog, mat = F)

dt_rcm_orog <- as.data.table(rs_rcm_orog, xy = T, na.rm = F)
dt_obs_orog <- as.data.table(rs_obs_orog, xy = T, na.rm = F)

# time stuff --------------------------------------------------------------

# non-standard cal workaround
nc_rcm <- nc_open(file_rcm)
raw_times <- ncdf4.helpers::nc.get.time.series(nc_rcm)
mapped_times <- eurocordexr::map_non_standard_calendar(raw_times)
dates_rcm <- mapped_times$dates_full
nc_close(nc_rcm)



# outfile ---------------------------------------------------------


create_emtpy_netcdf(file_template = file_obs_orog, 
                    file_out = file_out, 
                    l_varinfo = l_nc_info, 
                    date_period = range(dates_rcm),
                    overwrite = F)


# main proc ---------------------------------------------------------------

nc_out <- nc_open(file_out, write = T)

for(i_date in seq_along(dates_rcm)){
  
  i_rcm <- mapped_times[i_date, idx_pcict] # for non-standard cal
  vals_rcm <- mat_rcm[, i_rcm]
  
  if(i_var == "pr"){
    vals_rcm <- vals_rcm*24*3600
  } else {
    vals_rcm <- vals_rcm-273.15
  }
  
  vals_out <- lapse_rate_varying(dt_rcm_orog, dt_obs_orog, vals_rcm, kk_basis)
  
  ncvar_put(nc_out, varid = i_var, vals = vals_out, 
            start = c(1, 1, i_date), count = c(-1, -1, 1))
  
  nc_sync(nc_out)
  
  cat(sprintf("%s - date done: %s", date(), dates_rcm[i_date]), "\n")
  
}

nc_close(nc_out)




