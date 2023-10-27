# create empty downscaling netcdf template


# info: -------------------- #
# adapted from: https://pjbartlein.github.io/REarthSysSci/netCDF.html
# using CF conventions: https://cfconventions.org/

# valid units
# zz_units <- units::valid_udunits()
# units::valid_udunits_prefixes()
# -------------------------- #

library(ncdf4)
source("R/functions/chunk_shape_3D.R")

create_emtpy_netcdf <- function(file_template, 
                                file_out, 
                                l_varinfo, 
                                date_period = as.Date(c("1971-01-01", "2100-12-31")),
                                overwrite = F){
  
  if(!overwrite && file.exists(file_out)) {
    stop("file_out already exists, use 'overwrite=TRUE' to overwrite it")
  }
  stopifnot(hasName(l_varinfo, c("name", "units", "longname", 
                                 "cell_methods", "standard_name")))

  # helpers
  nc_template <- nc_open(file_template)  
  values_lon <- ncvar_get(nc_template, "longitude")
  values_lat <- ncvar_get(nc_template, "latitude")
  nc_close(nc_template)
  
  
  # time stuff
  timestring <- "days since 1949-12-01"
  values_date <- seq(date_period[1], date_period[2], by = "day")
  cf_time <- RNetCDF::utinvcal.nc(timestring, as.POSIXct(values_date))
  
  # define dimensions
  dim_lon <- ncdim_def("lon", "degrees_east", values_lon) 
  dim_lat <- ncdim_def("lat", "degrees_north", values_lat) 
  dim_time <- ncdim_def("time", timestring, cf_time)
  
  # get chunk size
  shape_txy <- c(length(values_date), length(values_lon), length(values_lat))
  chunks_txy <- chunk_shape_3D(shape_txy)
  
  # define variables
  var_out <- ncvar_def(name = l_varinfo$name,
                       units = l_varinfo$units,
                       dim = list(dim_lon, dim_lat, dim_time),
                       missval = 1e32, 
                       longname = l_varinfo$longname,
                       prec = "float",
                       chunksizes = chunks_txy[c(2,3,1)]) # chunksizes xyzt
  
  # create netCDF file and put arrays
  nc_out <- nc_create(file_out, var_out, force_v4=TRUE)
  
  # put variables (later?)
  # ncvar_put(nc_out, varid = var_out, vals = )
  
  # put additional attributes into dimension and data variables
  ncatt_put(nc_out, "lon", "axis", "X") 
  ncatt_put(nc_out, "lat", "axis", "Y")
  ncatt_put(nc_out, "time", "axis", "T")
  ncatt_put(nc_out, var_out, "cell_methods", l_varinfo$cell_methods)
  ncatt_put(nc_out, var_out, "standard_name", l_varinfo$standard_name)
  
  # add global attributes
  ncatt_put(nc_out, 0, "title", "Downscaled RCM data")
  ncatt_put(nc_out, 0, "institution", "DICAM, UniTN, Italy")
  # ncatt_put(nc_out, 0, "source",datasource$value)
  # ncatt_put(nc_out,0,"references",references$value)
  history <- paste("M. Matiu", date(), sep=", ")
  ncatt_put(nc_out, 0, "history", history)
  ncatt_put(nc_out, 0, "Conventions", "CF-1.4")
  
  # Get a summary of the created file:
  # nc_out
  
  
  # close the file, writing data to disk
  nc_close(nc_out)
  
}



