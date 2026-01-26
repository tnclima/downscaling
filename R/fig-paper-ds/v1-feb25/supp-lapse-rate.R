# compare lapse rates from lit, crespi, stations

library(data.table)
setDTthreads(4)
library(magrittr)
library(forcats)
library(ggplot2)

dat_micromet <- data.table(
  month = 1:12,
  lr = c(4.4, 5.9, 7.1, 7.8, 8.1, 8.2, 8.1, 8.1, 7.7, 6.8, 5.5, 4.7)
)
dat_rolland <- data.table(
  month = 1:12,
  tmin = c(5.0, 5.4, 5.9, 6.1, 6.0, 6.0, 6.0, 5.9, 5.6, 5.2, 4.9, 4.7),
  tmean = c(4.7, 5.1, 5.8, 6.3, 6.4, 6.5, 6.5, 6.5, 6.0, 5.6, 5.0, 4.6),
  tmax = c(4.1, 4.8, 5.7, 6.4, 6.7, 6.9, 6.9, 6.9, 6.4, 5.8, 5.1, 4.4)
)

dat <- readRDS("data/lapse-rates-01-daily-crespi-highlander-station.rds")
# dat[, .N, .(vv, ds)]
dat[, month := month(date)]
dat[, month_fct := mitmatmisc::month_fct(month)]

dat_avg <- dat[,
               .(lr = mean(lr)),
               .(vv, ds, month, month_fct)]

dat_clim <- readRDS("data/lapse-rates-02-clim-crespi-station.rds")
dat_clim[, month_fct := mitmatmisc::month_fct(month)]

# temperatures ------------------------------------------------------------

dat_plot_temp <- rbind(fill = T, 
                       dat_avg[vv != "prec" & ds != "highlander", 
                               .(ds, vv, month, lr = lr*-1000)],
                       dat_clim[vv != "prec" & ds != "highlander",
                                .(ds, vv, month, lr = lr*-1000)],
                       dat_micromet %>% cbind(ds = "lit_micromet", vv = "tmin"),
                       dat_micromet %>% cbind(ds = "lit_micromet", vv = "tmax"),
                       dat_rolland %>% 
                         melt(id.vars = "month", measure.vars = c("tmin", "tmax"), 
                              value.name = "lr", variable.name = "vv") %>% 
                         cbind(ds = "lit_rolland")
)
dat_plot_temp[, month_fct := mitmatmisc::month_fct(month)]

dat_plot_temp[, vv2 := fct_recode(vv, tasmin = "tmin", tasmax = "tmax")]
dat_plot_temp[, ds2 := fct_recode(ds, 
                                  "Obs_Grid" = "crespi",
                                  "Obs_GridClim" = "crespi_clim",
                                  "Obs_Station" = "station",
                                  "Obs_StationClim" = "station_clim",
                                  "Lit_Rolland2003" = "lit_rolland", 
                                  "Lit_ListonElder2006" = "lit_micromet")]
dat_plot_temp[, ds3 := factor(ds2, levels = c("Obs_Grid", "Obs_GridClim",
                                              "Obs_Station", "Obs_StationClim",
                                              "Lit_Rolland2003", "Lit_ListonElder2006"))]

cols <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#984ea3", "#ff7f00")

dat_plot_temp %>% 
  ggplot(aes(month_fct, lr, colour = ds3))+
  geom_point()+
  geom_line(aes(group = ds))+
  # scale_color_brewer(NULL, palette = "Set1")+
  scale_color_manual(NULL, values = cols)+
  facet_wrap(~vv2)+
  theme_bw()+
  xlab(NULL)+
  ylab("Average temperature lapse rate [-degC/km]")

ggsave("fig/paper-ds/supp-lapse-rate.png",
       width = 10, height = 4)
