# match high and low res gridcells based on some elev threshold (for PCA/KED tas*)


# library(terra)
library(magrittr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(fs)
library(foreach)
library(stringr)
library(sf)
library(eurocordexr)
library(ggplot2)

# data --------------------------------------------------------------------

dat_011_orog <- nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_crespi_temperature.nc",
                              add_xy = T)
dat_011_orog[, date := NULL]

file_obs_orog <- "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc"
dat_obs_orog <- nc_grid_to_dt(file_obs_orog, add_xy = T)
dat_obs_orog[, date := NULL]
setnames(dat_obs_orog, c("icell", "orog", "lon", "lat"))

sf_obs_orog <- dat_obs_orog %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
setnames(sf_obs_orog, "orog", "orog_1km")
setnames(sf_obs_orog, "icell", "icell_1km")

sf_tnaa <- readRDS("data/sf-tnaa/tnaa-latlon.rds") %>% st_as_sf()

sf_011_orog <- dat_011_orog %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_join(sf_tnaa, left = F)




# match cells -------------------------------------------------------------

th_elev_seq <- seq(50, 200, by = 50)
th_dist_seq <- seq(1500, 6000, by = 1500) # in meters
stopifnot(length(th_dist_seq) == length(th_elev_seq))

sf_new <- foreach(
  i = 1:nrow(sf_011_orog),
  # i = 1:100,
  .final = dplyr::bind_rows
) %do% {
  
  i_orog <- sf_011_orog$orog[i]
  
  for(i_th in seq_along(th_elev_seq)){
    th_elev <- th_elev_seq[i_th]
    th_dist <- th_dist_seq[i_th]
    
    sf_orog_i <- sf_obs_orog %>% 
      dplyr::filter(abs(orog_1km - i_orog) < th_elev)
    
    sf_orog_i2 <- st_join(sf_orog_i,
                          sf_011_orog[i, ], 
                          join = st_is_within_distance,
                          dist = th_dist)
    sf_orog_i3 <- sf_orog_i2 %>% 
      dplyr::filter(!is.na(icell)) %>% 
      dplyr::select(-icell, -orog)
    
    if(nrow(sf_orog_i3) == 0 & th_elev == max(th_elev_seq)) return(NULL)
    if(nrow(sf_orog_i3) != 0) break
  }
  
  sf_i <- st_join(sf_011_orog[i, ], 
                  sf_orog_i3,
                  join = st_nearest_feature)
  
  sf_i 
  # sf_i %>%
  # # sf_orog_i2 %>%
  #   st_drop_geometry() %>% 
  #   dplyr::left_join(sf_obs_orog)
  
}



# checks ------------------------------------------------------------------
# 
# 
# sf_new2 <- st_as_sf(sf_new)
# 
# sf_new2 <- sf_new %>%
#   st_drop_geometry() %>%
#   # dplyr::select(-geometry) %>%
#   dplyr::left_join(sf_obs_orog) %>%
#   st_as_sf
# 
# # test
# ggplot()+
#   geom_sf(data = sf_011_orog, aes(colour = "orig"))+
#   geom_sf(data = sf_new2, aes(colour = "new"))+
#   coord_sf()+
#   theme_bw()




# save --------------------------------------------------------------------


dat_out <- sf_new %>% 
  st_drop_geometry() %>% 
  dplyr::select(icell, icell_1km)


saveRDS(dat_out, "data/ked-gridcells-temperature-obs-011.rds")

