# 

library(sf)
library(stars)
library(dplyr)
library(giscoR)
library(ggplot2)
library(scico)
library(patchwork)

# data --------------------------------------------------------------------



sf_ita <- gisco_get_nuts(country = "Italy", nuts_level = 2, resolution = 10)
sf_ita <- sf_ita %>% mutate(lgl_tnaa = NUTS_ID %in% c("ITH1","ITH2"))

sf_tnaa <- gisco_get_nuts(resolution = "01", nuts_id = c("ITH1","ITH2"))
sf_tnaa_united <- st_union(sf_tnaa)


rs_10km <- read_stars("/home/climatedata/dem/eu_dem_gar_10km.tif")
rs_1km <- read_stars("/home/climatedata/dem/eu_dem_gar_1km.tif")
rs_1km_etopo_globe <- read_stars("/home/climatedata/dem/ETOPO_2022_v1_60s_N90W180_bed.tif")
rs_100m <- read_stars("/home/climatedata/dem/eu_dem_gar_100m.tif")
names(rs_1km) <- "elev"
names(rs_100m) <- "elev"
names(rs_1km_etopo_globe) <- "elev"

sf_tnaa_united2 <- sf_tnaa_united %>% st_transform(st_crs(rs_100m))
rs_tnaa <- rs_100m[sf_tnaa_united2]


sf_world <- spData::world %>% 
  mutate(lgl_ita = name_long == "Italy")  
st_crs(sf_world) <- st_crs(4326)


sf_gar <- rs_10km %>% 
  st_transform(crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

sf_gar2 <- rs_10km %>%
  st_transform(crs = st_crs(rs_1km_etopo_globe)) %>% 
  st_bbox() %>%
  st_as_sfc()



countries_sub <- st_transform(gisco_countries, st_crs(rs_1km)) %>% 
  st_crop(rs_1km)

sf_gar_countries <- gisco_get_countries(epsg = "4326",
                                        resolution = "03",
                                        country = countries_sub$ISO3_CODE)

rs_1km_etopo <- rs_1km_etopo_globe %>% st_crop(st_bbox(sf_gar2) + c(-2,-1, 2, 1 )) %>% st_as_stars()
# sf_gar_countries_etopo <- st_transform(sf_gar_countries, st_crs(rs_1km_etopo))
# sf_tn_etopo <- st_transform(sf_tn, st_crs(rs_1km_etopo))

rs_1km_etopo_4326 <- st_warp(rs_1km_etopo, crs = 4326)


# TN + Italy --------------------------------------------------------------------



gg_tnaa <-
ggplot()+
  geom_stars(data = rs_tnaa, na.action = na.omit, downsample = 1)+
  geom_sf(data = sf_tnaa_united2, fill = NA, colour = "black", linewidth = 0.4)+
  # scale_fill_viridis_c("Elev [m]", alpha = 0.8)+
  scale_fill_scico("Elevation\n[m a.s.l.]", palette = "oleron", midpoint = 0)+
  scale_x_continuous(breaks = seq(10.5, 12, by = 0.5))+
  coord_sf()+
  theme_classic()+
  theme(legend.position = c(0.90, 0.25))+
  # theme(legend.position = "top")+
  xlab(NULL)+ylab(NULL)

gg_ita <- ggplot()+
  geom_sf(data = sf_ita, aes(fill = lgl_tnaa), 
          colour = "black", linewidth = 0.2)+
  scale_fill_manual(values = c("TRUE" = "grey50", "FALSE" = "white"))+
  coord_sf()+
  # theme_classic()+
  theme_void()+
  theme(legend.position = "none")



gg_out <- (gg_tnaa+gg_ita)+
  plot_layout(widths = c(0.7, 0.5))


ggsave("fig/study-region/TNAA-elev-Italy.png", gg_out, width = 8, height = 5)




# TN + Italy + World -------------------------------------------------------------


gg_world <- ggplot()+
  geom_sf(data = sf_world, aes(fill = lgl_ita), 
          colour = "black", linewidth = 0.2)+
  scale_fill_manual(values = c("TRUE" = "grey50", "FALSE" = "white"))+
  # coord_sf(crs = "+proj=laea +lon_0=10 +lat_0=45")+
  coord_sf(crs = "+proj=ortho +lon_0=10 +lat_0=45")+
  theme_bw()+
  theme(panel.border = element_blank(),
        legend.position = "none")

gg_out <- (gg_tnaa+(gg_ita/gg_world))+
  plot_layout(widths = c(0.7, 0.3))


ggsave(filename = "fig/study-region/TNAA-elev-Italy-World.png", gg_out, width = 8, height = 5)



# TN + GAR + World --------------------------------------------------------

col_range <- range(rs_1km_etopo_4326$elev, na.rm = T)


gg_world_gar <-
ggplot()+
  geom_sf(data = sf_world, linewidth = 0.2, fill = "grey93")+
  geom_sf(data = sf_gar, fill = NA, colour = "black", linewidth = 0.5)+
  # scale_fill_manual(values = c("TRUE" = "grey50", "FALSE" = "white"))+
  # coord_sf(crs = "+proj=laea +lon_0=10 +lat_0=45")+
  coord_sf(crs = "+proj=ortho +lon_0=10 +lat_0=45")+
  theme_bw()+
  theme(panel.border = element_blank(),
        legend.position = "none")


gg_gar <-
ggplot()+
  geom_stars(data = rs_1km_etopo_4326, na.action = na.omit, downsample = 1)+
  geom_sf(data = sf_gar_countries, fill = NA, colour = "grey", linewidth = 0.2)+
  geom_sf(data = sf_tnaa_united, fill = NA, colour = "black", linewidth = 0.3)+
  # scale_fill_viridis_c("Elev [m]", alpha = 0.8)+
  scale_fill_scico("Elevation\n[m a.s.l.]", palette = "oleron", midpoint = 0, limits = col_range)+
  # scale_x_continuous(breaks = seq(10.5, 12, by = 0.5))+
  coord_sf(expand = F,
           xlim = st_bbox(sf_gar)[c("xmin", "xmax")] - c(-3,1), 
           ylim = st_bbox(sf_gar)[c("ymin", "ymax")] - c(-1,1))+
  theme_void()+
  # cowplot::theme_map()+
  # theme_classic()+
  # theme(legend.position = c(0.95, 0.25))+
  theme(legend.position = "none")+
  xlab(NULL)+ylab(NULL)



gg_out <-
  (gg_tnaa + (gg_gar / gg_world_gar))+
  plot_layout(widths = c(0.8, 0.4))



ggsave("fig/study-region/TNAA-elev-GAR-world.png", gg_out, width = 8, height = 5)


