# 


library(data.table)
setDTthreads(4)
library(ggplot2)
library(magrittr)
library(fs)
library(stringr)
library(purrr)
library(scico)
library(forcats)
library(patchwork)





# data --------------------------------------------------------------------

dat_aux_011 <- eurocordexr::nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eobs.nc",
                                          add_xy = T)
dat_aux_011[, date := NULL]


dat_crespi_011 <- readRDS("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/icell/mean-pctl/crespi-011.rds")


dat_raw <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/icell/mean-pctl/raw"),
  \(x){
    rcm <- x %>% path_file %>% path_ext_remove
    readRDS(x) %>% 
      cbind(institute_rcm = rcm)
  }
) %>% rbindlist()

dat_ba <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/icell/mean-pctl/", glob = "*/ba*"),
  \(path_ba){
    map(dir_ls(path_ba),
        \(x){
          rcm <- x %>% path_file %>% path_ext_remove
          readRDS(x) %>% 
            cbind(institute_rcm = rcm)
        }) %>% rbindlist(fill = T) %>% cbind(bads = path_file(path_ba))
  }
) %>% rbindlist(fill = T)


# prep --------------------------------------------------------------------

dat_plot <- rbind(
  dat_raw %>% cbind(bads = "raw"),
  dat_ba[bads %in% c("ba-qdm", "ba-mbcn")]
) %>% 
  melt(id.vars = c("season", "icell", "institute_rcm", "bads"),
       variable.name = "variable_pctl") %>% 
  .[, c("variable", "pctl") := tstrsplit(variable_pctl, "_")]

dat_plot[, bads_fct := fct_relevel(bads, "raw", "ba-qdm")]

dat_plot[, rcm_short := eurocordexr::shortnames_rcm[institute_rcm]]
dat_plot[, institute := str_remove(institute_rcm, rcm_short)]
dat_plot[, institute := str_remove(institute, "-$")]


dat_plot_crespi <- dat_crespi_011 %>% 
  melt(id.vars = c("season", "icell"),
       variable.name = "variable_pctl") %>% 
  .[, c("variable", "pctl") := tstrsplit(variable_pctl, "_", keep = c(1,3))]
setnames(dat_plot_crespi, "value", "value_crespi")
dat_plot_crespi[, variable_pctl := NULL]


# maps tasmax p50 --------------------------------------------------------------

dat_i <- dat_plot[variable == "tasmax" & season == "JJA" & pctl == "p50"]
dat_i_crespi <- dat_plot_crespi[variable == "tasmax" & season == "JJA" & pctl == "p50"]

gg1 <-
  dat_i %>% 
  merge(dat_aux_011, by = "icell") %>% 
  ggplot(aes(lon, lat))+
  geom_raster(aes(fill = value))+
  facet_grid(bads_fct ~ institute + rcm_short)+
  scale_fill_viridis_c("JJA\np50\ntasmax")+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())+
  xlab(NULL)+ylab(NULL)


gg2 <-
dat_i %>% 
  merge(dat_i_crespi) %>% 
  merge(dat_aux_011, by = "icell") %>% 
  ggplot(aes(lon, lat))+
  geom_raster(aes(fill = value - value_crespi))+
  scale_fill_scico("diff\nw.r.t.\nobs", palette = "vik", midpoint = 0, limits = c(-5, 5), oob = scales::oob_squish)+
  facet_grid(bads_fct ~ institute + rcm_short)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())+
  xlab(NULL)+ylab(NULL)


gg_out <- (gg1/gg2)+plot_annotation(tag_levels = "a", tag_suffix = ")")

ggsave("fig/paper-ds/ba-only_JJA-p50-tasmax.png", gg_out, width = 17, height = 7)



# metrics, only bias  ----------------------------------------------------------------



dat_raw <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/tnaa/metrics/raw"),
  \(x){
    rcm <- x %>% path_file %>% path_ext_remove
    readRDS(x) %>% 
      cbind(institute_rcm = rcm)
  }
) %>% rbindlist()



dat_ba <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/tnaa/metrics/", glob = "*/ba*"),
  \(path_ba){
    map(dir_ls(path_ba),
        \(x){
          rcm <- x %>% path_file %>% path_ext_remove
          readRDS(x) %>% 
            cbind(institute_rcm = rcm)
        }) %>% rbindlist(fill = T) %>% cbind(bads = path_file(path_ba))
  }
) %>% rbindlist(fill = T)


dat_plot <-  rbind(
  dat_raw %>% cbind(bads = "raw"),
  dat_ba[bads %in% c("ba-qdm", "ba-mbcn")]
)

dat_plot[, bads_fct := fct_relevel(bads, "raw", "ba-qdm")]

dat_plot[, yy := ifelse(str_starts(variable, "tas"), bias, bias_rel)]


l_gg <- map(c("tasmin", "tasmax", "pr", "hn"), \(i_var){
  
  gg <- dat_plot[variable == i_var] %>% 
    ggplot(aes(bads_fct, bias))+
    geom_hline(yintercept = 0, linetype = "dashed")+
    geom_boxplot()+
    facet_grid(. ~ season, scales = "free_y")+
    theme_bw()+
    xlab(NULL)+
    ylab(str_c(i_var, " bias"))
  
  
  if(i_var %in% c("pr", "hn")){
    gg <- gg+
      scale_y_continuous(labels = scales::label_percent())
  }
  
  gg
  
})

gg_out <- wrap_plots(l_gg, ncol = 1)


ggsave("fig/paper-ds/ba-only_metrics.png", gg_out, width = 9, height = 8)


# numbers -----------------------------------------------------------------

dat_plot[, as.list(fivenum(yy)), .(season, variable, bads_fct)]
dat_plot[, as.list(range(yy)), .(variable, bads_fct)]

dat_plot[variable == "hn" & season %in% c("DJF", "MAM"), 
         as.list(fivenum(yy)), .(season, variable, bads_fct)]


# model with best (raw) performance tasmax
dat_zz <- dat_plot[variable == "tasmax" & bads == "raw"]
setorder(dat_zz, season, bias)