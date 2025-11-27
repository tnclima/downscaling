# check coef of pcalm

library(data.table)
library(ggplot2)

dat_coef <- readRDS("data/coef-bathends-pcalm.rds")

dat1 <- dat_coef[ba == "qdm" & variable == "tasmax"]
dat1 <- dat_coef[ba == "qdm" & variable == "tasmin"]

dat1[, season := mitmatmisc::season_fct(month(date))]


dat1 |> 
  ggplot(aes(estimate))+
  # geom_histogram()+
  geom_freqpoly(aes(colour = institute_rcm))+
  facet_grid(season~term, scales = "free")+
  theme_bw()



dat1 <- dat_coef[ba == "qdm" & variable == "pr"]
dat1[, season := mitmatmisc::season_fct(month(date))]

dat1[p.value < 0.05 & term != "(Intercept)"] |> 
  ggplot(aes(estimate))+
  # geom_histogram()+
  geom_freqpoly(aes(colour = institute_rcm))+
  facet_grid(season~term, scales = "free")+
  theme_bw()
