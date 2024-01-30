# get 1d time series from nc-file (time dimension)
library(ncdf4)

get_nc_1d <- function(nc_file, i_row, i_col){
  nc1 <- nc_open(nc_file)
  vv <- ncvar_get(nc1, start = c(i_col, i_row, 1), count = c(1, 1, -1))
  nc_close(nc1)
  vv
}