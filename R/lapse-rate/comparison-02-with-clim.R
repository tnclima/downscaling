# compare lapse rates from lit, crespi, highlander, stations

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
  dat_avg[vv != "prec", .(ds, vv, month, lr = lr*-1000)],
  dat_clim[vv != "prec", .(ds, vv, month, lr = lr*-1000)],
  dat_micromet %>% cbind(ds = "lit_micromet", vv = "tmin"),
  dat_micromet %>% cbind(ds = "lit_micromet", vv = "tmax"),
  dat_rolland %>% 
    melt(id.vars = "month", measure.vars = c("tmin", "tmax"), 
         value.name = "lr", variable.name = "vv") %>% 
    cbind(ds = "lit_rolland")
)
dat_plot_temp[, month_fct := mitmatmisc::month_fct(month)]

dat_plot_temp %>% 
  ggplot(aes(month_fct, lr, colour = fct_inorder(ds)))+
  geom_point()+
  geom_line(aes(group = ds))+
  scale_color_brewer("source", palette = "Set1")+
  facet_wrap(~vv)+
  theme_bw()+
  xlab(NULL)+
  ylab("Average temperature lapse rate [-degC/km]")

ggsave("fig/lapse-rate/summary-05-temperature-avg-clim.png",
       width = 10, height = 4)


# precip ------------------------------------------------------------------

dat_plot_prec1 <- rbind(
  dat[vv == "prec" & ds == "station" & nn_stn > 20],
  dat[vv == "prec" & ds == "highlander" & frac_nonzero > 0.1],
  dat[vv == "prec" & ds == "crespi"]
)


dat_plot_prec1_avg <- rbind(
  dat_plot_prec1[, 
                 .(lr = mean(lr)), 
                 .(month, month_fct, ds)],
  dat_clim[vv == "prec", .(lr, month, month_fct, ds)]
)
  
dat_plot_prec1_avg %>% 
  ggplot(aes(month_fct, lr*1000, colour = ds))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point()+
  geom_line(aes(group = ds))+
  scale_color_brewer("source", palette = "Set1")+
  theme_bw()+
  xlab(NULL)+
  ylab("Precipitation lapse rate [mm/km]")

ggsave("fig/lapse-rate/summary-06-precipitation-avg-clim.png",
       width = 6, height = 4)
