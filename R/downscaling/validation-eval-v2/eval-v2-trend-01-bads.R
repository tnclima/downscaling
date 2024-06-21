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
library(patchwork)
library(forcats)
library(scico)

path_in <- "/home/climatedata/downscaling/validation-cv/rdata-summary-v2/eval-bads/"

pctl <- c(0, 0.05, 0.5, 0.95, 1)


# aux data ----------------------------------------------------------------

dat_icell <- readRDS("/home/climatedata/downscaling/validation-cv/rdata-summary-v1/eval-08-aux-icell-1km-011deg.rds")
dat_aux <- dat_icell[!is.na(orog), .(icell = icell_1km, x, y, orog)]



# crespi
dat_crespi_month <- readRDS("/home/climatedata/downscaling/validation-cv/rdata-summary-v2/eval-crespi-month.rds")
setnames(dat_crespi_month, "variable", "vv")
dat_crespi_month[, period20 := ifelse(period20 == "1981-2000", "period1", "period2")]

dat_crespi1 <- dat_crespi_month %>% 
  melt(measure.vars = c(str_c("p", pctl*100), "mean_value"),
       variable.factor = F) %>% 
  dcast(... ~ period20)
setnames(dat_crespi1, c("period1", "period2"), str_c("crespi_", c("period1", "period2")))


# rcm-raw
dat_rcm_month <- readRDS("/home/climatedata/downscaling/validation-cv/rdata-summary-v2/eval-rcm-raw-month.rds")
setnames(dat_rcm_month, "variable", "vv")
dat_rcm_month[, period20 := ifelse(period20 == "1981-2000", "period1", "period2")]

dat_rcm1 <- dat_rcm_month %>% 
  melt(measure.vars = c(str_c("p", pctl*100), "mean_value"),
       variable.factor = F) %>% 
  dcast(... ~ period20)
setnames(dat_rcm1, c("period1", "period2"), str_c("rcm_", c("period1", "period2")))

setnames(dat_rcm1, "icell", "icell_011deg")
dat_rcm1_1km <- dat_rcm1 %>% 
  merge(dat_icell[!is.na(orog), .(icell = icell_1km, icell_011deg)], 
        by = "icell_011deg", allow.cartesian = T)

# rcm-raw orog
dat_aux_rcm <- readRDS("/home/climatedata/downscaling/validation-cv/rdata-summary-v1/eval-09-aux-rcm.rds")
dat_aux_rcm_1km <- dat_aux_rcm[, .(centers, icell_011deg = icell, orog_rcm = orog)] %>% 
  merge(dat_icell[!is.na(orog), .(icell = icell_1km, icell_011deg, orog)], 
        by = "icell_011deg", allow.cartesian = T)


# files -------------------------------------------------------------------


dat_files <- data.table(filename = dir_ls(path_in))
dat_files[, c("period20", "ba", "centers", "freq") := tstrsplit(path_file(filename) %>% path_ext_remove(), "_")]
dat_files




# 1mod example -----------------------------------------------------------------

i_center <- 1

dat_files_1mod <- dat_files[centers == i_center & freq == "month" & ba %in% c("qm", "qdm")]

dat_1mod <- lapply(dat_files_1mod$filename, readRDS) %>% rbindlist
setnames(dat_1mod, "variable", "vv")
dat_1mod[, period20 := ifelse(period20 == "1981-2000", "period1", "period2")]

dat1 <- dat_1mod %>% 
  melt(measure.vars = c(str_c("p", pctl*100), "mean_value"),
       variable.factor = F) %>% 
  dcast(... ~ period20)



dat_plot <- dat1[month %in% c(1, 4, 7, 10),
                 .(icell, month, ba, variable, vv, period1, period2)] %>% 
  rbind(dat_rcm1_1km[month %in% c(1, 4, 7, 10), 
                     .(icell, month, ba = "rcm-raw", variable, vv, 
                       period1 = rcm_period1, period2 = rcm_period2)]) %>% 
  rbind(dat_crespi1[month %in% c(1, 4, 7, 10),
                    .(icell, month, ba = "crespi", variable, vv, 
                      period1 = crespi_period1, period2 = crespi_period2)]) %>% 
  merge(dat_aux, by = "icell")

dat_plot[, ba_fct := fct_relevel(ba, "rcm-raw", "crespi")]
dat_plot[, month_fct := mitmatmisc::month_fct(month)]

## adjusted by train ------------------------------------------

# temp

gg1 <- dat_plot[variable == "p95" & vv == "tasmax"] %>% 
  ggplot(aes(x, y, fill = period2 - period1))+
  geom_raster()+
  scale_fill_scico("", palette = "vik", midpoint = 0)+
  facet_grid(month_fct ~ ba_fct)+
  theme_bw()

dat_plot2 <- dat1[variable == "p95" & vv == "tasmax" & month %in% c(1, 4, 7, 10)] %>% 
  merge(dat_rcm1_1km, by = c("icell", "month", "vv", "variable", "centers")) %>% 
  merge(dat_crespi1) %>% 
  merge(dat_aux, by = "icell")
dat_plot2[, month_fct := mitmatmisc::month_fct(month)]
dat_plot2[, ba_fct := str_c(ba, "_adj")]

dat_plot3 <- rbind(
  dat_plot[variable == "p95" & vv == "tasmax", 
           .(x, y, month_fct, ba_fct, rr = period2 - period1)],
  dat_plot2[, .(x, y, month_fct, ba_fct,
                rr = period2 - period1 - 2*(rcm_period2 - rcm_period1) + (crespi_period2 - crespi_period1))]
)

dat_plot3 %>% 
  ggplot(aes(x, y, fill = rr))+
  geom_raster()+
  scale_fill_scico("trend", palette = "vik", midpoint = 0)+
  facet_grid(month_fct ~ ba_fct)+
  theme_bw()+
  xlab(NULL)+ylab(NULL)



# data all mod ------------------------------------------------------------

dat_files_allmod <- dat_files[freq == "month" & ba %in% c("qm", "qdm")]

dat_allmod <- lapply(dat_files_allmod$filename, readRDS) %>% rbindlist
setnames(dat_allmod, "variable", "vv")
dat_allmod[, period20 := ifelse(period20 == "1981-2000", "period1", "period2")]


# data tmin, tmax ---------------------------------------------------------

dat_allmod_longsub <- dat_allmod[month %in% c(1, 4, 7, 10) & 
                                   vv %in% c("tasmin", "tasmax")] %>% 
  melt(measure.vars = c(str_c("p", pctl*100), "mean_value"),
       variable.factor = F) %>% 
  dcast(... ~ period20)

dat_crespi1_subrep <- lapply(1:6, \(x) cbind(
  dat_crespi1[month %in% c(1, 4, 7, 10) & vv %in% c("tasmin", "tasmax")],
  centers = x
)) %>% rbindlist()


dat_plot1 <- dat_allmod_longsub[,
                                .(icell, month, centers, ba, variable, vv, period1, period2)] %>% 
  rbind(dat_rcm1_1km[month %in% c(1, 4, 7, 10) & vv %in% c("tasmin", "tasmax"), 
                     .(icell, month, centers, ba = "rcm-raw", variable, vv, 
                       period1 = rcm_period1, period2 = rcm_period2)]) %>% 
  rbind(dat_crespi1_subrep[,
                           .(icell, month, centers, ba = "crespi", variable, vv, 
                             period1 = crespi_period1, period2 = crespi_period2)]) %>% 
  merge(dat_aux, by = "icell")

dat_plot1[, ba_fct := fct_relevel(ba, "rcm-raw", "crespi")]


dat_plot2 <- dat_allmod_longsub %>% 
  merge(dat_rcm1_1km, by = c("icell", "month", "vv", "variable", "centers")) %>% 
  merge(dat_crespi1_subrep) %>% 
  merge(dat_aux, by = "icell")

dat_plot2[, ba_fct := str_c(ba, "_adj")]

dat_plot3 <- rbind(
  dat_plot1[, .(icell, x, y, month, ba_fct, variable, vv, centers,
                rr = period2 - period1)],
  dat_plot2[, .(icell, x, y, month, ba_fct, variable, vv, centers,
                rr = period2 - period1 - 2*(rcm_period2 - rcm_period1) + (crespi_period2 - crespi_period1))]
)
dat_plot3[, month_fct := mitmatmisc::month_fct(month)]



# loop tmin,tmax ----------------------------------------------------------



dat_loop <- dat_plot3[, .(variable, vv, centers)] %>% unique

for(i in 1:nrow(dat_loop)){
  
  dat_i <- dat_plot3 %>% merge(dat_loop[i])
  tit <- dat_loop[i, str_c(vv, ": ", variable, ", model: ", centers)]
  fn <- path("fig/validation-eval/trend-preserv/", 
             dat_loop[i, str_c(vv, "_", variable, "_mod", centers)],
             ext = "png")
  
  gg <- dat_i %>% 
    ggplot(aes(x, y, fill = rr))+
    geom_raster()+
    scale_fill_scico("trend", palette = "vik", midpoint = 0)+
    facet_grid(month_fct ~ ba_fct)+
    coord_fixed()+
    theme_bw()+
    xlab(NULL)+ylab(NULL)+
    ggtitle(tit)
  
  ggsave(fn, gg, width = 12, height = 6)
  
}




# summary -----------------------------------------------------------------


gg1 <- dat_plot3[ba_fct %in% c("qdm_adj", "qm_adj") & vv == "tasmax"] %>% 
  ggplot(aes(rr))+
  geom_histogram()+
  scale_x_continuous(limits = c(-5, 5), oob = scales::oob_squish)+
  facet_grid(ba_fct ~ variable, scale = "free_y")+
  theme_bw()+
  xlab("Trend modification")+
  ylab("count (tot = ~13k cells * 4 months * 6 models)")+
  ggtitle("tasmax")

ggsave("fig/validation-eval/trend-preserv-summary/tasmax-hist.png", 
       gg1, width = 12, height = 4)



gg1 <- dat_plot3[ba_fct %in% c("qdm_adj", "qm_adj") & vv == "tasmin"] %>% 
  ggplot(aes(rr))+
  geom_histogram()+
  scale_x_continuous(limits = c(-5, 5), oob = scales::oob_squish)+
  facet_grid(ba_fct ~ variable, scale = "free_y")+
  theme_bw()+
  xlab("Trend modification")+
  ylab("count (tot = ~13k cells * 4 months * 6 models)")+
  ggtitle("tasmin")

ggsave("fig/validation-eval/trend-preserv-summary/tasmin-hist.png", 
       gg1, width = 12, height = 4)




# any dependency with elevation? ------------------------------------------

dat_sub_elev <- dat_plot3[vv == "tasmax" & 
                            # variable %in% c("p0", "p50") & 
                            ba_fct %in% c("qdm_adj", "qm_adj")]


dat_sub1 <- dat_sub_elev[centers == 1 & month == 4]
dat_sub1 %>% 
  merge(dat_aux_rcm_1km) %>% 
  ggplot(aes(orog - orog_rcm, rr))+
  geom_hline(yintercept = 0)+
  geom_bin2d()+
  geom_smooth(method = lm, se = F, colour = "black", linetype = "dashed")+
  scale_fill_viridis_c(trans = "log10")+
  facet_grid(ba_fct ~ variable)+
  theme_bw()

dat_sub2 <- dat_plot3[vv == "tasmax" & 
                        variable == "p0" & 
                        ba_fct == "qm_adj"]
dat_sub2 %>% 
  merge(dat_aux_rcm_1km) %>% 
  ggplot(aes(orog, rr))+
  geom_hline(yintercept = 0)+
  geom_bin2d()+
  # geom_smooth(method = lm, se = F, colour = "black", linetype = "dashed")+
  scale_fill_viridis_c(trans = "log10")+
  facet_grid(month_fct ~ centers, scales = "free_y")+
  theme_bw()


# -> some dependency with orog_rcm (not orog, not difference)
# depends on model and month (season), stronger dependence for qm vs qdm




# data prec, hn ---------------------------------------------------------

dat_allmod_longsub <- dat_allmod[month %in% c(1, 4, 7, 10) & 
                                   !vv %in% c("tasmin", "tasmax")] %>% 
  melt(measure.vars = c(str_c("p", pctl*100), "mean_value"),
       variable.factor = F) %>% 
  dcast(... ~ period20)

dat_crespi1_subrep <- lapply(1:6, \(x) cbind(
  dat_crespi1[month %in% c(1, 4, 7, 10) & ! vv %in% c("tasmin", "tasmax")],
  centers = x
)) %>% rbindlist()


dat_plot1 <- dat_allmod_longsub[,
                                .(icell, month, centers, ba, variable, vv, period1, period2)] %>% 
  rbind(dat_rcm1_1km[month %in% c(1, 4, 7, 10) & !vv %in% c("tasmin", "tasmax"), 
                     .(icell, month, centers, ba = "rcm-raw", variable, vv, 
                       period1 = rcm_period1, period2 = rcm_period2)]) %>% 
  rbind(dat_crespi1_subrep[,
                           .(icell, month, centers, ba = "crespi", variable, vv, 
                             period1 = crespi_period1, period2 = crespi_period2)]) %>% 
  merge(dat_aux, by = "icell")

dat_plot1[, ba_fct := fct_relevel(ba, "rcm-raw", "crespi")]


dat_plot2 <- dat_allmod_longsub %>% 
  merge(dat_rcm1_1km, by = c("icell", "month", "vv", "variable", "centers")) %>% 
  merge(dat_crespi1_subrep) %>% 
  merge(dat_aux, by = "icell")

dat_plot2[, ba_fct := str_c(ba, "_adj")]

dat_plot3 <- rbind(
  dat_plot1[, .(icell, x, y, month, ba_fct, variable, vv, centers,
                rr = (period2 - period1)/period1)],
  dat_plot2[, .(icell, x, y, month, ba_fct, variable, vv, centers,
                rr = (period2 - period1)/period1 -
                  2*((rcm_period2 - rcm_period1)/rcm_period1) - 
                  (crespi_period1 - crespi_period2)/crespi_period2)]
)
dat_plot3[, month_fct := mitmatmisc::month_fct(month)]




# prec check formula -----------------------------------------------------------
# 
# dat_plot1[vv == "pr" & variable == "p95" & centers == 4] %>% 
#   ggplot(aes(x, y, fill = (period2 - period1)/period1))+
#   geom_raster()+
#   # scale_fill_scico("", palette = "vik", midpoint = 0)+
#   scale_fill_scico("", palette = "vik", midpoint = 0, limits = c(NA, 1), oob = scales::oob_squish)+
#   # scale_fill_viridis_c()+
#   facet_grid(month ~ ba_fct)+
#   theme_bw()
# 
# 
dat_plot2[vv == "pr" & variable == "p95" & centers == 4] %>%
  # ggplot(aes(x, y, fill = (period2 - period1)/period1))+
  # ggplot(aes(x, y, fill = (rcm_period2 - rcm_period1)/rcm_period1))+
  # ggplot(aes(x, y, fill = (rcm_period1 - rcm_period2)/rcm_period2 + (rcm_period2 - rcm_period1)/rcm_period1))+
  # ggplot(aes(x, y, fill = (crespi_period2 - crespi_period1)/crespi_period1))+
  # ggplot(aes(x, y, fill = (period2 - period1)/period1 + (crespi_period2 - crespi_period1)/crespi_period1))+
  # ggplot(aes(x, y, fill = (period2 - period1)/period1 - (crespi_period1 - crespi_period2)/crespi_period2))+
  # ggplot(aes(x, y, fill = (period2 - period1)/period1 - 2*((rcm_period2 - rcm_period1)/rcm_period1)))+
  ggplot(aes(x, y, fill = (period2 - period1)/period1 - 2*((rcm_period2 - rcm_period1)/rcm_period1) - (crespi_period1 - crespi_period2)/crespi_period2 ))+
  # ggplot(aes(x, y, fill = (period1 - period2)/period2 - 2*((rcm_period1 - rcm_period2)/rcm_period2) - (crespi_period2 - crespi_period1)/crespi_period1))+
  # ggplot(aes(x, y, fill = (period2 - period1)/period1 - 2*((rcm_period2 - rcm_period1)/rcm_period1) + (crespi_period2 - crespi_period1)/crespi_period1))+
  # ggplot(aes(x, y, fill = (period2 - period1)/period1 -
               # ((rcm_period2 - rcm_period1)/rcm_period1) + ((rcm_period1 - rcm_period2)/rcm_period2)))+
  geom_raster()+
  scale_fill_scico("", palette = "vik", midpoint = 0)+
  # scale_fill_scico("", palette = "vik", midpoint = 0, limits = c(NA, 2), oob = scales::oob_squish)+
  # scale_fill_viridis_c()+
  facet_grid(month ~ ba)+
  theme_bw()
#   
#   
#   
# dat_plot2[vv == "pr" & variable == "p95" & centers == 3] %>% 
#   # ggplot(aes(x, y, fill = period2/period1 + (rcm_period2/rcm_period1) * (crespi_period1/crespi_period2) ))+
#   # ggplot(aes(x, y, fill = period2 - (rcm_period2-rcm_period1)/rcm_period1 * crespi_period1 ))+
#   ggplot(aes(x, y, fill = period1 - (rcm_period1-rcm_period2)/rcm_period2 * crespi_period2 ))+
#   geom_raster()+
#   scale_fill_scico("", palette = "vik", midpoint = 0)+
#   # scale_fill_scico("", palette = "vik", midpoint = 0, limits = c(NA, 2), oob = scales::oob_squish)+
#   # scale_fill_viridis_c()+
#   facet_grid(month ~ ba)+
#   theme_bw()
# 


# loop prec ----------------------------------------------------------------------


dat_loop <- dat_plot3[, .(variable, vv, centers)] %>% unique



for(i in 1:nrow(dat_loop)){
  
  dat_i <- dat_plot3 %>% merge(dat_loop[i])
  tit <- dat_loop[i, str_c(vv, ": ", variable, ", model: ", centers)]
  fn <- path("fig/validation-eval/trend-preserv/", 
             dat_loop[i, str_c(vv, "_", variable, "_mod", centers)],
             ext = "png")
  
  gg <- dat_i %>% 
    ggplot(aes(x, y, fill = rr))+
    geom_raster()+
    scale_fill_scico("trend", palette = "vik", midpoint = 0, labels = scales::label_percent(),
                     limits = c(-2,2), oob = scales::oob_squish)+
    facet_grid(month_fct ~ ba_fct)+
    coord_fixed()+
    theme_bw()+
    xlab(NULL)+ylab(NULL)+
    ggtitle(tit)
  
  ggsave(fn, gg, width = 12, height = 6)
  
}

# not so good with the relative changes...

