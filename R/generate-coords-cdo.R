# generate coords file for CDO to remap RCMs to regular latlon

# taken from topoCLIM
# write remapbil config file
# remapping is done to grid centres thats why we add half a gridbox (res/2)

res <- 0.11
buffer <- 2*res
library(sf)
library(stringr)

# get latlon from TNAA study domain
sf_tnaa <- readRDS("data/sf-tnaa/tnaa-latlon.rds")
# sf_tnaa <- readRDS("data/sf-tnaa/tnaa-latlon-buff6km.rds")
bb <- st_bbox(sf_tnaa)
lonE <- bb[3]
lonW <- bb[1]
latN <- bb[4]
latS <- bb[2]

file_content <- str_c(
  "gridtype = lonlat\n",
  "xsize = ", round((lonE - lonW + 2*buffer) / res), "\n",
  "ysize = ", round((latN - latS + 2*buffer) / res), "\n",
  "xfirst = ", (lonW - buffer + res / 2), "\n",
  "xinc = ", res, "\n",
  "yfirst = ", (latS - buffer + res / 2), "\n",
  "yinc = ", res, "\n"
)
cat(file_content)
cat(file_content, file = "data/coords_tnaa.txt")

