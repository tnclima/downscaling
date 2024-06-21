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


# data all mod ------------------------------------------------------------

dat_files_allmod <- dat_files[freq == "month" & ba %in% c("mbcn", "qdm")]

dat_allmod <- lapply(dat_files_allmod$filename, readRDS) %>% rbindlist
setnames(dat_allmod, "variable", "vv")
dat_allmod[, period20 := ifelse(period20 == "1981-2000", "period1", "period2")]



# data hn: period avg ---------------------------------------------------------

dat_allmod_avg <- dat_allmod[month %in% c(11:12, 1:5) & vv == "hn"] %>% 
  melt(measure.vars = c("p50", "p95", "p100", "mean_value"),
       variable.factor = F) %>% 
  .[, .(value = mean(value)), .(icell, month, variable, vv, centers, ba)]

dat_crespi_avg <- dat_crespi_month[month %in% c(11:12, 1:5) & vv == "hn"] %>% 
  melt(measure.vars = c("p50", "p95", "p100", "mean_value"),
       variable.factor = F) %>% 
  .[, .(value = mean(value)), .(icell, month, variable, vv)]

dat_crespi_avg_rep <- lapply(1:6, \(x) cbind(dat_crespi_avg, centers = x)) %>% rbindlist()


dat_plot1 <- dat_allmod_avg %>% 
  rbind(cbind(dat_crespi_avg_rep, ba = "crespi")) %>% 
  merge(dat_aux)
dat_plot1[, month_fct := mitmatmisc::month_fct(month, 10)]

dat_plot2 <- dat_plot1 %>% 
  dcast(... ~ ba, value.var = "value")
# abs
# dat_plot2[, qdm_crespi := qdm - crespi]
# dat_plot2[, mbcn_crespi := mbcn - crespi]
# dat_plot2[, mbcn_qdm := mbcn - qdm]
# rel
dat_plot2[, qdm_crespi := (qdm - crespi)/crespi]
dat_plot2[, mbcn_crespi := (mbcn - crespi)/crespi]
dat_plot2[, mbcn_qdm := (mbcn - qdm)/qdm]

dat_plot3 <- melt(dat_plot2, 
                  id.vars = c("icell", "month", "variable", "vv", "centers", "x", "y", "orog", "month_fct"),
                  measure.vars = c("qdm_crespi", "mbcn_crespi", "mbcn_qdm"),
                  variable.name = "ff_diff")

# loop ---------------------------------------------------------------


dat_loop <- dat_plot3[, .(variable, centers)] %>% unique

for(i in 1:nrow(dat_loop)){
  
  dat_i1 <- dat_plot1 %>% merge(dat_loop[i], by = names(dat_loop))
  dat_i3 <- dat_plot3 %>% merge(dat_loop[i])
  
  tit <- dat_loop[i, str_c(variable, ", model: ", centers)]
  fn <- path("fig/validation-eval/multivar/", 
             dat_loop[i, str_c(variable, "_mod", centers)],
             ext = "png")
  
  
  
  gg1 <- dat_i1 %>%   
    ggplot(aes(x, y, fill = value))+
    geom_raster()+
    # scale_fill_viridis_b()+
    scale_fill_viridis_b(n.breaks = 7)+
    coord_equal()+
    facet_grid(ba ~ month_fct)+
    theme_bw()+
    xlab(NULL)+ylab(NULL)+
    ggtitle(tit)
  
  
  
  gg2 <- dat_i3 %>%   
    ggplot(aes(x, y, fill = value))+
    geom_raster()+
    # scale_fill_scico("", palette = "vik", midpoint = 0)+
    scale_fill_scico("", palette = "vik", midpoint = 0, 
                     limits = c(NA, 2), oob = scales::oob_squish, labels = scales::label_percent())+
    coord_equal()+
    facet_grid(ff_diff ~ month_fct)+
    theme_bw()+
    xlab(NULL)+ylab(NULL)
  
  gg <- gg1/gg2
  
  ggsave(fn, gg, width = 14, height = 8)
  
}



# summary -----------------------------------------------------------------

dat_summ1 <- dat_plot1[, .(value = mean(value)), .(month, month_fct, variable, centers, ba)]
dat_summ1 %>% 
  ggplot(aes(month_fct, value, colour = ba))+
  geom_line(aes(group = paste0(ba, centers)))+
  facet_grid(variable ~ centers, scale = "free_y")+
  theme_bw()+
  xlab(NULL)+ylab("mean daily snowfall [mm SWE]")

ggsave("fig/validation-eval/multivar-summary/hn-month-raw.png",
       width = 16, height = 8)

dat_summ3 <- dat_plot3[, .(value = mean(value)), .(month, month_fct, variable, centers, ff_diff)]
dat_summ3[ff_diff != "mbcn_qdm" & variable != "p50"] %>% 
  ggplot(aes(month_fct, value, colour = ff_diff))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_line(aes(group = paste0(ff_diff, centers)))+
  scale_y_continuous(labels = scales::label_percent())+
  facet_grid(variable ~ centers, scale = "free_y")+
  theme_bw()+
  xlab(NULL)+ylab("Difference")

ggsave("fig/validation-eval/multivar-summary/hn-month-diff.png",
       width = 16, height = 6)
