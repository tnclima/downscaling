# download shape file TNAA


library(giscoR)
library(sf)

# trentino shape with buffer
sf_tn <- gisco_get_nuts(resolution = "01", nuts_id = "ITH2")
sf_tn %>% 
  st_transform(crs = 3035) %>% 
  st_buffer(6000) %>% 
  st_transform(crs = 4326) -> sf_tn_buff

saveRDS(sf_tn, "data/sf-tnaa/tn-latlon.rds")
saveRDS(sf_tn_buff, "data/sf-tnaa/tn-latlon-buff6km.rds")


# tnaa shape with buffer
sf_tnaa <- gisco_get_nuts(resolution = "01", nuts_id = c("ITH1","ITH2"))
sf_tnaa_united <- st_union(sf_tnaa)

sf_tnaa_united %>% 
  st_transform(crs = 3035) %>% 
  st_buffer(6000) %>% 
  st_transform(crs = 4326) -> sf_tnaa_united_buff

saveRDS(sf_tnaa_united, "data/sf-tnaa/tnaa-latlon.rds")
saveRDS(sf_tnaa_united_buff, "data/sf-tnaa/tnaa-latlon-buff6km.rds")


