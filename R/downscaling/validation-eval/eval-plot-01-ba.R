# 

# library(eurocordexr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(magrittr)
library(ggplot2)
library(fs)
library(stringr)
library(purrr)
library(foreach)

dat_aux <- eurocordexr::nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eobs.nc")
dat_aux[, date := NULL]
dat_aux$elevation %>% hist(30)
dat_aux$elevation %>% summary
dat_aux[, elev_fct := cut(elevation, breaks = seq(0, 3000, by = 500), dig.lab = 5)]

dat <- readRDS("/home/climatedata/downscaling/validation-cv/rdata-summary/eval-01-ba-only.rds")

dat <- merge(dat, dat_aux)

with(dat, table(variable, ba))

# period
dat[variable == "pr" & centers == 1] %>% 
  ggplot(aes(bias_rel, colour = ba, linetype = period20))+
  geom_freqpoly()+
  facet_wrap(~month)+
  theme_bw()

dat[variable == "pr" & centers == 1] %>% 
  ggplot(aes(bias, colour = ba, linetype = period20))+
  geom_freqpoly()+
  facet_wrap(~month)+
  theme_bw()

# centers
dat[variable == "tasmax" & month == 1] %>% 
  ggplot(aes(bias, colour = ba, linetype = period20))+
  geom_freqpoly()+
  facet_wrap(~centers)+
  theme_bw()

# -> check why differences between periods


# variables
dat[centers == 1 & month == 1] %>% 
  ggplot(aes(bias, colour = ba))+
  geom_freqpoly()+
  facet_wrap(~variable)+
  theme_bw()

dat[centers == 1 & month == 1] %>% 
  ggplot(aes(wassersteindist, colour = ba))+
  geom_freqpoly()+
  facet_wrap(~variable)+
  theme_bw()

dat[centers == 1 & month %in% c(1, 4, 11)] %>% 
  ggplot(aes(bias, colour = ba))+
  geom_freqpoly()+
  facet_grid(variable ~ month)+
  theme_bw()

dat[centers == 1 & month %in% c(1, 4, 11)] %>% 
  ggplot(aes(wassersteindist, colour = ba))+
  geom_freqpoly()+
  facet_grid(variable ~ month)+
  theme_bw()



# domain averages ---------------------------------------------------------

dat_avg <- dat[, lapply(.SD, mean), .(period20, month, variable, centers, ba)]

dat_avg %>% 
  ggplot(aes(month, wassersteindist, colour = ba, linetype = period20))+
  # geom_point()+
  geom_line()+
  facet_grid(variable ~ centers, scales = "free_y")+
  theme_bw()

dat_avg %>% 
  ggplot(aes(month, bias, colour = ba, linetype = period20))+
  # geom_point()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_line()+
  facet_grid(variable ~ centers, scales = "free_y")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

# avg periods, too
dat_avg2 <- dat[, 
                lapply(.SD, mean), 
                .(month, variable, centers, ba), 
                .SDcols = c("bias", "wassersteindist", "bias_rel")]

dat_avg2 %>% 
  ggplot(aes(month, bias, colour = ba))+
  # geom_point()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_line()+
  facet_grid(variable ~ centers, scales = "free_y")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

dat_avg2 %>% 
  ggplot(aes(month, wassersteindist, colour = ba))+
  # geom_point()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_line()+
  facet_grid(variable ~ centers, scales = "free_y")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

dat_avg2 %>% 
  ggplot(aes(month, bias_rel, colour = ba))+
  # geom_point()+
  geom_hline(yintercept = 1, linetype = "dashed")+
  geom_line()+
  facet_grid(variable ~ centers, scales = "free_y")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

# avg periods, too, by elev
dat_avg3 <- dat[, 
                lapply(.SD, mean), 
                .(month, variable, centers, ba, elev_fct), 
                .SDcols = c("bias", "wassersteindist", "bias_rel")]

dat_avg2[variable == "hn"] %>% 
  ggplot(aes(month, bias_rel, colour = ba))+
  # geom_point()+
  geom_hline(yintercept = 1, linetype = "dashed")+
  geom_line()+
  facet_grid(elev_fct ~ centers, scales = "free_y")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())
