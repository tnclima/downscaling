
# single elements ---------------------------------------------------------


dat_o_wide <- dat_1mod %>% 
  dcast(icell + month + variable + centers + ba ~ period20, value.var = "mean_value")
setnames(dat_o_wide, c("1981-2000", "2001-2020"), str_c("ba_", c("1981_2000", "2001_2020")))
dat_o_wide <- dat_o_wide %>% 
  merge(dat_clim_rcm2_1km, by = c("icell", "month", "variable", "centers")) %>% 
  merge(dat_clim_crespi2) %>% 
  merge(dat_aux, by = "icell")

dat_o_wide[month == 1 & variable == "tasmax"] %>% 
  ggplot(aes(x, y, fill = (crespi_2001_2020 - crespi_1981_2000)))+
  geom_raster()+
  scale_fill_scico("", palette = "vik", midpoint = 0)+
  facet_grid(. ~ ba)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)


dat_o_wide[month == 1 & variable == "tasmax"] %>% 
  ggplot(aes(x, y, fill = (rcm_2001_2020 - rcm_1981_2000)))+
  geom_raster()+
  scale_fill_scico("", palette = "vik", midpoint = 0)+
  facet_grid(. ~ ba)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)

dat_o_wide[month == 1 & variable == "tasmax"] %>% 
  ggplot(aes(x, y, fill = (rcm_2001_2020 - rcm_1981_2000) - (crespi_2001_2020 - crespi_1981_2000)))+
  geom_raster()+
  scale_fill_scico("", palette = "vik", midpoint = 0)+
  facet_grid(. ~ ba)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)


dat_o_wide[month == 1 & variable == "tasmax"] %>% 
  ggplot(aes(x, y, fill = (ba_2001_2020 - ba_1981_2000) - (crespi_2001_2020 - crespi_1981_2000)))+
  geom_raster()+
  scale_fill_scico("", palette = "vik", midpoint = 0)+
  facet_grid(. ~ ba)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)

dat_o_wide[month == 1 & variable == "tasmax"] %>% 
  ggplot(aes(x, y, fill = (ba_2001_2020 - (rcm_1981_2000 - rcm_2001_2020)) - crespi_1981_2000))+
  geom_raster()+
  scale_fill_scico("", palette = "vik", midpoint = 0)+
  facet_grid(. ~ ba)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)


dat_o_wide[month == 2 & variable == "tasmax"] %>% 
  ggplot(aes(x, y, fill = (ba_2001_2020 - ba_1981_2000) - (rcm_2001_2020 - rcm_1981_2000) +
               (crespi_2001_2020 - crespi_1981_2000)))+
  geom_raster()+
  scale_fill_scico("", palette = "vik", midpoint = 0)+
  facet_grid(. ~ ba)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)


dat_o_wide[month == 1 & variable == "tasmax"] %>% 
  ggplot(aes(x, y, fill = (ba_2001_2020 - crespi_1981_2000) - (rcm_2001_2020 - rcm_1981_2000) - crespi_2001_2020))+
  geom_raster()+
  scale_fill_scico("", palette = "vik", midpoint = 0)+
  facet_grid(. ~ ba)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)



dat_o_wide[month == 2 & variable == "tasmax"] %>% 
  ggplot(aes(x, y, fill = + (ba_2001_2020 - crespi_2001_2020) - (ba_1981_2000 - crespi_1981_2000)  ))+
  geom_raster()+
  scale_fill_scico("", palette = "vik", midpoint = 0)+
  facet_grid(. ~ ba)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)

dat_o_wide[month == 2 & variable == "tasmax"] %>% 
  ggplot(aes(x, y, fill = (ba_2001_2020 - ba_1981_2000)  - (rcm_2001_2020 - rcm_1981_2000) +
               (crespi_2001_2020 - crespi_1981_2000)))+
  geom_raster()+
  scale_fill_scico("", palette = "vik", midpoint = 0)+
  facet_grid(. ~ ba)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)

dat_o_wide[month == 2 & variable == "tasmax"] %>% 
  ggplot(aes(x, y, fill = (ba_2001_2020 - crespi_1981_2000) - (rcm_2001_2020 - rcm_1981_2000) ))+
  # ggplot(aes(x, y, fill = (crespi_2001_2020 - ba_1981_2000) - (rcm_2001_2020 - rcm_1981_2000) ))+
  geom_raster()+
  scale_fill_scico("", palette = "vik", midpoint = 0)+
  facet_grid(. ~ ba)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)



# single elements p* ---------------------------------------------------------


dat_clim_rcm2 <- dat_clim_rcm %>% 
  dcast(icell_011deg + month + variable + centers ~ period20, value.var = "p50")
setnames(dat_clim_rcm2, c("1981-2000", "2001-2020"), str_c("rcm_", c("1981_2000", "2001_2020")))
# setnames(dat_clim_rcm2, "icell", "icell_011deg")
dat_clim_rcm2_1km <- dat_clim_rcm2 %>% 
  merge(dat_icell[!is.na(orog), .(icell = icell_1km, icell_011deg)], 
        by = "icell_011deg", allow.cartesian = T)

dat_clim_crespi2 <- dat_clim_crespi %>% 
  dcast(icell + month + variable ~ period20, value.var = "p50")
setnames(dat_clim_crespi2, c("1981-2000", "2001-2020"), str_c("crespi_", c("1981_2000", "2001_2020")))


dat_o_wide <- dat_1mod %>% 
  dcast(icell + month + variable + centers + ba ~ period20, value.var = "p50")
setnames(dat_o_wide, c("1981-2000", "2001-2020"), str_c("ba_", c("1981_2000", "2001_2020")))
dat_o_wide <- dat_o_wide %>% 
  merge(dat_clim_rcm2_1km, by = c("icell", "month", "variable", "centers")) %>% 
  merge(dat_clim_crespi2) %>% 
  merge(dat_aux, by = "icell")

dat_o_wide[variable == "pr",
           mean((ba_2001_2020 - ba_1981_2000)  - (rcm_2001_2020 - rcm_1981_2000) +
             (crespi_2001_2020 - crespi_1981_2000)),
           .(month, ba)] %>% 
  ggplot(aes(month, V1, colour = ba))+
  geom_line()


dat_o_wide[month == 9 & variable == "tasmax"] %>% 
  ggplot(aes(x, y, fill = (ba_2001_2020 - ba_1981_2000)  - (rcm_2001_2020 - rcm_1981_2000) +
               (crespi_2001_2020 - crespi_1981_2000)))+
  geom_raster()+
  scale_fill_scico("", palette = "vik", midpoint = 0)+
  facet_grid(. ~ ba)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)

dat_o_wide[month == 6 & variable == "tasmin"] %>% 
  ggplot(aes(x, y, fill = (ba_2001_2020 - crespi_1981_2000) - (rcm_2001_2020 - rcm_1981_2000) ))+
  # ggplot(aes(x, y, fill = (ba_1981_2000 - crespi_2001_2020) + (rcm_2001_2020 - rcm_1981_2000) ))+
  geom_raster()+
  scale_fill_scico("", palette = "vik", midpoint = 0)+
  facet_grid(. ~ ba)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)



# ba-only: single elements p* ---------------------------------------------------------

value_ptcl <- "p50"

dat_clim_rcm2 <- dat_clim_rcm %>% 
  dcast(icell + month + variable + centers ~ period20, value.var = value_ptcl)
setnames(dat_clim_rcm2, c("1981-2000", "2001-2020"), str_c("rcm_", c("1981_2000", "2001_2020")))
# setnames(dat_clim_rcm2, "icell", "icell_011deg")

dat_eobs2 <- dat_eobs %>% 
  dcast(icell + month + variable ~ period20, value.var = value_ptcl)
setnames(dat_eobs2, c("1981-2000", "2001-2020"), str_c("eobs_", c("1981_2000", "2001_2020")))


dat_o_wide <- dat_1mod %>% 
  dcast(icell + month + variable + centers + ba ~ period20, value.var = value_ptcl)
setnames(dat_o_wide, c("1981-2000", "2001-2020"), str_c("ba_", c("1981_2000", "2001_2020")))
dat_o_wide <- dat_o_wide %>% 
  merge(dat_clim_rcm2, by = c("icell", "month", "variable", "centers")) %>% 
  merge(dat_eobs2) %>% 
  merge(dat_aux, by = "icell")

dat_o_wide[variable == "tasmin",
           mean((ba_2001_2020 - ba_1981_2000)  - (rcm_2001_2020 - rcm_1981_2000) +
                  (eobs_2001_2020 - eobs_1981_2000)),
           .(month, ba)] %>% 
  ggplot(aes(month, V1, colour = ba))+
  geom_line()


dat_o_wide[month == 9 & variable == "tasmax"] %>% 
  ggplot(aes(lon, lat, fill = (ba_2001_2020 - ba_1981_2000)))+
  geom_raster()+
  scale_fill_scico("", palette = "vik", midpoint = 0)+
  facet_grid(. ~ ba)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)

dat_o_wide[month == 9 & variable == "tasmax"] %>% 
  ggplot(aes(lon, lat, fill = (eobs_2001_2020 - eobs_1981_2000)))+
  geom_raster()+
  scale_fill_scico("", palette = "vik", midpoint = 0)+
  facet_grid(. ~ ba)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)

dat_o_wide[month == 9 & variable == "tasmax"] %>% 
  ggplot(aes(lon, lat, fill = (rcm_2001_2020 - rcm_1981_2000)))+
  geom_raster()+
  scale_fill_scico("", palette = "vik", midpoint = 0)+
  facet_grid(. ~ ba)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)


dat_o_wide[month == 9 & variable == "tasmax"] %>% 
  ggplot(aes(lon, lat, fill = (ba_2001_2020 - ba_1981_2000) - (rcm_2001_2020 - rcm_1981_2000) +
               (eobs_2001_2020 - eobs_1981_2000)))+
  geom_raster()+
  scale_fill_scico("", palette = "vik", midpoint = 0)+
  facet_grid(. ~ ba)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)

dat_o_wide[month == 6 & variable == "tasmin"] %>% 
  ggplot(aes(lon, lat, fill = (ba_2001_2020 - eobs_1981_2000) - (rcm_2001_2020 - rcm_1981_2000) ))+
  # ggplot(aes(lon, lat, fill = (ba_1981_2000 - eobs_2001_2020) + (rcm_2001_2020 - rcm_1981_2000) ))+
  geom_raster()+
  scale_fill_scico("", palette = "vik", midpoint = 0)+
  facet_grid(. ~ ba)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)

dat_o_wide[month == 6 & variable == "tasmin"] %>% 
  ggplot(aes(lon, lat, fill = (ba_2001_2020 - eobs_1981_2000) - (rcm_2001_2020 - rcm_1981_2000) ))+
  # ggplot(aes(lon, lat, fill = (ba_1981_2000 - eobs_2001_2020)  ))+
  # ggplot(aes(lon, lat, fill = (ba_1981_2000 - eobs_1981_2000) ))+
  geom_raster()+
  scale_fill_scico("", palette = "vik", midpoint = 0)+
  facet_grid(. ~ ba)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)


