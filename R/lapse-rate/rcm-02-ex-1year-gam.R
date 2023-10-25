# calculate local lapse rates for an example year of 1 RCM

library(terra)
library(magrittr)
library(lubridate)
library(stars)
library(ggplot2)
library(data.table)
setDTthreads(4)
library(foreach)
library(mgcv)
library(ggh4x)


# k smooth
# kk <- 5
# kk <- 10

# function to compute gams
fun_gm <- function(x, kk){
  dat1 <- as.data.table(x, cell = T)
  setnames(dat1, c("cell", "value"))
  dat2 <- merge(dat1, dat_orog)
  
  gm1 <- gam(value ~ s(x, k = kk) + s(y, k = kk) + s(orog, k = kk), data = dat2)
  dat_plot <- plot(gm1, pages = 1)

  lapply(dat_plot, function(x){
    data.table(xx = x$x, fit = as.vector(x$fit), xvar = x$xlab, yvar = x$ylab)
  }) %>% rbindlist
}



rr_orog <- rast("/home/climatedata/downscaling/rcm_lonlat_tnaa/orog/orog_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r0i0p0_CLMcom-CCLM4-8-17_v1_fx.nc")
dates <- seq(ymd("1950-01-01"), ymd("2005-12-31"), by = "day")
i_dates_1year <- which(year(dates) == "2001")

dat_orog <- as.data.table(rr_orog, xy = T, cells = T)


mitmatmisc::init_parallel_ubuntu(16)

# tmin --------------------------------------------------------------------



# tmax --------------------------------------------------------------------


## data --------------------------------------------------------------------



rr_rcm <- rast("/home/climatedata/downscaling/rcm_lonlat_tnaa/tasmax/tasmax_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_19500101-20051231.nc",
               lyrs = i_dates_1year)

rr_in <- lapply(rr_rcm, wrap, proxy = TRUE)

l_fit5 <- foreach(
  i = seq_along(rr_in)
) %dopar% {
  rr_in[[i]] %>% 
    unwrap() %>% 
    fun_gm(5)
}

names(l_fit5) <- dates[i_dates_1year]
dat_fit5 <- rbindlist(l_fit5, idcol = "date")
dat_fit5[, date := ymd(date)]

l_fit10 <- foreach(
  i = seq_along(rr_in)
) %dopar% {
  rr_in[[i]] %>% 
    unwrap() %>% 
    fun_gm(10)
}

names(l_fit10) <- dates[i_dates_1year]
dat_fit10 <- rbindlist(l_fit10, idcol = "date")
dat_fit10[, date := ymd(date)]



## plots -------------------------------------------------------------------

date_sample <- sample(dates[i_dates_1year], 9)

dat_fit5[date %in% date_sample] %>% 
  ggplot(aes(xx, fit))+
  geom_line()+
  facet_grid2(xvar ~ date, scales = "free", independent = "x")+
  theme_bw()

dat_fit_510 <- rbind(
  cbind(dat_fit5, kk = 5),
  cbind(dat_fit10, kk = 10)
)

date_sample <- sample(dates[i_dates_1year], 9)

dat_fit_510[date %in% date_sample] %>% 
  ggplot(aes(xx, fit, colour = factor(kk)))+
  geom_line()+
  facet_grid2(xvar ~ date, scales = "free", independent = "x")+
  theme_bw()

dat_fit_510[xvar == "orog" & month(date) == 11] %>% 
  ggplot(aes(xx, fit, colour = factor(kk)))+
  geom_line()+
  facet_wrap(~ date, nrow = 4)+
  theme_bw()



# l_fit <- foreach(
#   i = seq_along(rr_in)
# ) %dopar% {
# 
#   rr1 <- unwrap(rr_in[[i]])
#   dat1 <- as.data.table(rr1, cell = T)
#   setnames(dat1, c("cell", "value"))
#   dat2 <- merge(dat1, dat_orog)
#   
#   gm1 <- gam(value ~ s(x, k = kk) + s(y, k = kk) + s(orog, k = kk), data = dat2)
#   # gm1 <- gam(value ~ s(x, k = kk, bs = "ts") + s(y, k = kk, bs = "ts") + s(orog, k = kk, bs = "ts"), data = dat2)
#   # gm2 <- gam(value ~ te(x,y) + s(orog), data = dat2)
#   # gm1 <- gam(value ~ s(x) + s(y), data = dat2)
#   # summary(gm1)
#   dat_plot <- plot(gm1, pages = 1)
#   # gam.check(gm1)
#   # plot(dat2$orog, resid(gm1))
#   # plot(dat2$x, resid(gm1))
#   # plot(dat2$y, resid(gm1))
#   
#   lapply(dat_plot, function(x){
#     data.table(xx = x$x, fit = as.vector(x$fit), xvar = x$xlab, yvar = x$ylab)
#   }) %>% rbindlist
#   
# }
# 
# names(l_fit) <- dates[i_dates_1year]
# dat_fit <- rbindlist(l_fit, idcol = "date")
# dat_fit[, date := ymd(date)]
# 
# date_sample <- sample(dates[i_dates_1year], 9)
# 
# dat_fit[date %in% date_sample] %>% 
#   ggplot(aes(xx, fit))+
#   geom_line()+
#   facet_grid2(xvar ~ date, scales = "free", independent = "x")+
#   theme_bw()



# prec --------------------------------------------------------------------

## data --------------------------------------------------------------------

rr_rcm <- rast("/home/climatedata/downscaling/rcm_lonlat_tnaa/pr/pr_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_19500101-20051231.nc",
               lyrs = i_dates_1year)

rr_in <- lapply(rr_rcm, wrap, proxy = TRUE)

l_fit5 <- foreach(
  i = seq_along(rr_in)
) %dopar% {
  rr_in[[i]] %>% 
    unwrap() %>% 
    fun_gm(5)
}

names(l_fit5) <- dates[i_dates_1year]
dat_fit5 <- rbindlist(l_fit5, idcol = "date")
dat_fit5[, date := ymd(date)]

l_fit10 <- foreach(
  i = seq_along(rr_in)
) %dopar% {
  rr_in[[i]] %>% 
    unwrap() %>% 
    fun_gm(10)
}

names(l_fit10) <- dates[i_dates_1year]
dat_fit10 <- rbindlist(l_fit10, idcol = "date")
dat_fit10[, date := ymd(date)]



## plots -------------------------------------------------------------------

date_sample <- sample(dates[i_dates_1year], 9)

dat_fit5[date %in% date_sample] %>% 
  ggplot(aes(xx, fit))+
  geom_line()+
  facet_grid2(xvar ~ date, scales = "free", independent = "x")+
  theme_bw()

dat_fit_510 <- rbind(
  cbind(dat_fit5, kk = 5),
  cbind(dat_fit10, kk = 10)
)

date_sample <- sample(dates[i_dates_1year], 9)

dat_fit_510[date %in% date_sample] %>% 
  ggplot(aes(xx, fit, colour = factor(kk)))+
  geom_line()+
  facet_grid2(xvar ~ date, scales = "free", independent = "x")+
  theme_bw()

dat_fit_510[xvar == "orog" & month(date) == 11] %>% 
  ggplot(aes(xx, fit, colour = factor(kk)))+
  geom_line()+
  facet_wrap(~ date, nrow = 4)+
  theme_bw()



# l_fit <- foreach(
#   i = seq_along(rr_in)
# ) %dopar% {
# 

i <- 31  # north rain ~ 30mm

rr1 <- unwrap(rr_in[[i]])
  dat1 <- as.data.table(rr1, cell = T)
  setnames(dat1, c("cell", "value"))
  dat1[, value := value*86400]
  dat2 <- merge(dat1, dat_orog)
  dat2[, value2 := round(value*10)]
  
  plot(rr1*86400)
  pairs(dat2)
  
  
  kk <- 5
  kk <- 10
  gm1 <- gam(value ~ s(x, k = kk) + s(y, k = kk) + s(orog, k = kk), data = dat2)
  gm1 <- gam(value ~ s(x, k = kk) + s(y, k = kk) + s(orog, k = kk), data = dat2, family = Gamma())
  gm1 <- gam(value2 ~ s(x, k = kk) + s(y, k = kk) + s(orog, k = kk), data = dat2, family = poisson())
  gm1 <- gam(list(value2 ~ s(x, k = kk) + s(y, k = kk) + s(orog, k = kk),
                  ~ s(x, k = kk) + s(y, k = kk)), 
             data = dat2, family = ziplss())
  gm1 <- gam(list(value2 ~ s(x, k = kk) + s(y, k = kk) + s(orog, k = kk),
                  ~ te(x, y)), 
             data = dat2, family = ziplss())
  gm1 <- gam(list(value2 ~ te(x, y) + s(orog, k = kk),
                  ~ te(x, y)), 
             data = dat2, family = ziplss())
  
  # gm1 <- gam(value ~ s(x, k = kk, bs = "ts") + s(y, k = kk, bs = "ts") + s(orog, k = kk, bs = "ts"), data = dat2)
  gm1 <- gam(value ~ te(x,y) + s(orog), data = dat2)
  gm1 <- gam(value ~ s(x) + s(orog), data = dat2)
  summary(gm1)
  dat_plot <- plot(gm1, pages = 1)
  dat_plot <- plot(gm1, pages = 1, scale = 0)
  gam.check(gm1)
  plot(dat2$orog, resid(gm1))
  plot(dat2$x, resid(gm1))
  plot(dat2$y, resid(gm1))
#   
#   lapply(dat_plot, function(x){
#     data.table(xx = x$x, fit = as.vector(x$fit), xvar = x$xlab, yvar = x$ylab)
#   }) %>% rbindlist
#   
# }
# 
# names(l_fit) <- dates[i_dates_1year]
# dat_fit <- rbindlist(l_fit, idcol = "date")
# dat_fit[, date := ymd(date)]
# 
# date_sample <- sample(dates[i_dates_1year], 9)
# 
# dat_fit[date %in% date_sample] %>% 
#   ggplot(aes(xx, fit))+
#   geom_line()+
#   facet_grid2(xvar ~ date, scales = "free", independent = "x")+
#   theme_bw()
