# 

library(eurocordexr)
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
library(foreach)






# data & general --------------------------------------------------------------------

source("R/functions/inv_sub_reanalysis.R")
source("R/functions/snowfall.R")

dat_aux <- nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc",
                         add_xy = T)
dat_aux <- dat_aux[!is.na(orog), .(icell, x = longitude, y = latitude, orog)]

dat_aux_011 <- nc_grid_to_dt("/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eobs.nc",
                             add_xy = T)
dat_aux_011[, date := NULL]

path_in <- "/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v2/"
path_in_v1 <- "/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v1/"


l_file_crespi_011 <- list(
  tasmax = "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/tasmax_crespi.nc",
  tasmin = "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/tasmin_crespi.nc",
  pr = "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/pr_crespi.nc"
)

l_file_crespi <- list(
  tasmax = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MaxTemp.nc",
  tasmin = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MinTemp.nc",
  pr = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_Prec.nc"
)


# inventory 

dat_inv <- inv_sub_reanalysis()
dat_inv_loop <- dat_inv[variable %in% c("tasmin", "tasmax", "pr")]

dat_inv_loop_mod <- dat_inv_loop[, .(gcm, institute_rcm, experiment, 
                                     ensemble, downscale_realisation)] %>% unique()

# helper fun --------------------------------------------------------------

f_read <- function(l_files, date1 = i_date1, date2 = i_date2, raw = F){
  
  if("pr" %in% names(l_files)){
    
    dat_pr <- nc_grid_to_dt(l_files$pr, date_range = c(date1, date2))
    setnames(dat_pr, 3, "pr")
    
    dat_tasmin <- nc_grid_to_dt(l_files$tasmin, date_range = c(date1, date2))
    setnames(dat_tasmin, 3, "tasmin")
    
    dat_tasmax <- nc_grid_to_dt(l_files$tasmax, date_range = c(date1, date2))
    setnames(dat_tasmax, 3, "tasmax")
    
    dat_i <- cbind(dat_pr, tasmax = dat_tasmax$tasmax, tasmin = dat_tasmin$tasmin)
    dat_i <- dat_i[!is.na(pr)]
    
    if(raw){
      dat_i[, pr := pr*86400]
      dat_i[, tasmax := tasmax-273.15]
      dat_i[, tasmin := tasmin-273.15]
    }
    
    dat_i[, hn := snowfall(pr, tasmax, tasmin)]
    
  } else {
    
    dat_tasmin <- nc_grid_to_dt(l_files$tasmin, date_range = c(date1, date2))
    setnames(dat_tasmin, 3, "tasmin")
    
    dat_tasmax <- nc_grid_to_dt(l_files$tasmax, date_range = c(date1, date2))
    setnames(dat_tasmax, 3, "tasmax")
    
    dat_i <- cbind(dat_tasmax, tasmin = dat_tasmin$tasmin)
    dat_i <- dat_i[!is.na(tasmax)]
    
  }
  
  
  return(dat_i) 
}


files_ba_qdm <- dir_ls(path(path_in, "ba-qdm"))
files_ba_mbcn <- dir_ls(path(path_in, "ba-mbcn"))

path_bads <- dir_ls(path_in) %>%
  str_subset("ba-qdm$", negate = T) %>%
  str_subset("ba-mbcn$", negate = T) %>% 
  c(dir_ls(path_in_v1) %>% str_subset("bads"))

path_bads_uni <- dir_ls(path_in) %>%
  str_subset("ba-qdm$", negate = T) %>%
  str_subset("ba-mbcn", negate = T) %>% 
  c(dir_ls(path_in_v1) %>% str_subset("bads-qdm"))


# tasmax ------------------------------------------------------------------

i_var <- "tasmax"
i_date <- "2003-06-06"
i_rcm_name <- "GERICS-REMO2015"

# crespi
dat_crespi <- f_read(l_file_crespi, i_date, i_date)
dat_crespi_011 <- f_read(l_file_crespi_011, i_date, i_date)

# raw
l_files <- dat_inv_loop[institute_rcm == i_rcm_name, list_files]
names(l_files) <- dat_inv_loop[institute_rcm == i_rcm_name, variable]
dat_raw <- f_read(l_files, i_date, i_date, raw = T)

# qdm
l_files <- str_subset(files_ba_qdm, fixed(i_rcm_name)) %>% sort %>% as.list
names(l_files) <- c("pr", "tasmax", "tasmin")
dat_qdm <- f_read(l_files, i_date, i_date)

# mbcn
# l_files <- str_subset(files_ba_mbcn, fixed(i_rcm_name)) %>% sort %>% as.list
# names(l_files) <- c("pr", "tasmax", "tasmin")
# dat_mbcn <- f_read(l_files, i_date, i_date)

dat_plot_011 <- rbind(
  cbind(dat_raw, ff = "raw"),
  cbind(dat_qdm, ff = "ba-qdm")
  # cbind(dat_mbcn, ff = "ba-mbcn")
)
dat_plot_011[, ff_fct := fct_inorder(ff)]

dat_plot_011 <- dat_plot_011[icell %in% dat_qdm$icell]

# bads
dat_bads <- foreach(i_path = path_bads_uni) %do% {
  
  files_bads <- dir_ls(i_path)
  i_bads <- path_file(i_path)
  
  files_read <- str_subset(files_bads, fixed(i_rcm_name)) %>% sort %>% as.list
  
  if(length(files_read) == 2){
    # no pr
    names(files_read) <- c("tasmax", "tasmin")
    dat_i <- f_read(files_read, i_date, i_date)
    
  } else {
    # with pr
    names(files_read) <- c("pr", "tasmax", "tasmin")
    dat_i <- f_read(files_read, i_date, i_date)
    
  }
  
  cbind(dat_i, bads = i_bads)
} %>% rbindlist(fill = T)

dat_bads[, bads_multi := str_detect(bads, "mbcn")]
dat_bads[, bads_multi_chr := ifelse(bads_multi, "multi", "uni")]
dat_bads[, bads_ds := bads %>% forcats::fct_recode(
  "ds-pcalm" = "ba-qdm-ds-pcalm",
  # "ds-pcalm" = "ba-mbcn-ds-pcalm",
  "ds-qdm" = "ba-qdm-ds-qdm",
  # "ds-qdm" = "ba-mbcn-ds-qdm",
  "ds-qdm2" = "ba-qdm-ds-qdm2",
  # "ds-qdm2" = "ba-mbcn-ds-qdm2",
  "ds-gam" = "ba-qdm-ds-gam",
  # "ds-gam" = "ba-mbcn-ds-gam",
  "ds-lr" = "ba-qdm-ds-lr",
  # "ds-lr" = "ba-mbcn-ds-lr",
  "bads" = "bads-qdm"#,
  # "bads" = "bads-mbcn"   
)]

dat_bads[, bads_ds := factor(bads_ds, levels = c(
  "bads", "ds-pcalm", "ds-qdm", "ds-qdm2", "ds-lr", "ds-gam"
))]

## plot --------------------------------------------------------------------


dat_plot_crespi <- dat_crespi
dat_plot_crespi_011 <- dat_crespi_011
# dat_plot_crespi <- if(i_var %in%  c("tasmin", "tasmax")) dat_crespi else dat_crespi1
# lbl_suffix <- if(i_var %in%  c("tasmin", "tasmax")) "" else " (day+1)"

lims_col <- range(dat_bads[[i_var]], dat_plot_011[[i_var]], dat_plot_crespi[[i_var]],
                  na.rm = T)
lims_x <- range(dat_aux$x, dat_aux_011$lon)
lims_y <- range(dat_aux$y, dat_aux_011$lat)

gg_bads <-
dat_bads %>% 
  merge(dat_aux) %>% 
  ggplot(aes(x, y, fill = !!sym(i_var)))+
  geom_raster()+
  # scale_fill_viridis_c(limits = lims_col)+
  scale_fill_scico(palette = "lajolla", limits = lims_col)+
  facet_wrap( ~ bads_ds, dir = "v", nrow = 2)+
  xlim(lims_x)+ylim(lims_y)+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())+
  coord_fixed()+
  xlab(NULL)+ylab(NULL)

gg_011 <-
  dat_plot_011 %>% 
  merge(dat_aux_011) %>% 
  ggplot(aes(lon, lat, fill = !!sym(i_var)))+
  geom_raster()+
  # scale_fill_viridis_c(limits = lims_col)+
  scale_fill_scico(palette = "lajolla", limits = lims_col)+
  facet_grid(. ~ ff_fct)+
    xlim(lims_x)+ylim(lims_y)+
    theme_bw()+
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank())+
  coord_fixed()+
  xlab(NULL)+ylab(NULL)

gg_crespi <- 
  rbind(
    dat_plot_crespi %>% 
      cbind(ff = "obs (1km)") %>% 
      merge(dat_aux),
    dat_plot_crespi_011 %>% 
      cbind(ff = "obs (0.11°)") %>% 
      merge(dat_aux_011[, .(icell, x = lon, y = lat, orog = elevation)])
  ) %>% 
  ggplot(aes(x, y, fill = !!sym(i_var)))+
  geom_raster()+
  # scale_fill_viridis_c(limits = lims_col)+
  scale_fill_scico(palette = "lajolla", limits = lims_col)+
  facet_grid(. ~ fct_inorder(ff))+
  xlim(lims_x)+ylim(lims_y)+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())+
  coord_fixed()+
  xlab(NULL)+ylab(NULL)

gg_out <-
wrap_plots( #wrap_plots(gg_crespi, gg_crespi_011, nrow = 1), 
           gg_crespi,
           gg_011, 
           gg_bads, 
           ncol = 1,  heights = c(1,1,1.5), guides = "collect")+
  plot_annotation(tag_levels = "a", tag_suffix = ")")


ggsave("fig/paper-ds/ds-example-maps_tasmax.png", 
       gg_out, width = 6.5, height = 8)





# tasmin ------------------------------------------------------------------

i_var <- "tasmin"
i_date <- "2003-12-26"
i_rcm_name <- "GERICS-REMO2015"

# crespi
dat_crespi <- f_read(l_file_crespi, i_date, i_date)
dat_crespi_011 <- f_read(l_file_crespi_011, i_date, i_date)

# raw
l_files <- dat_inv_loop[institute_rcm == i_rcm_name, list_files]
names(l_files) <- dat_inv_loop[institute_rcm == i_rcm_name, variable]
dat_raw <- f_read(l_files, i_date, i_date, raw = T)

# qdm
l_files <- str_subset(files_ba_qdm, fixed(i_rcm_name)) %>% sort %>% as.list
names(l_files) <- c("pr", "tasmax", "tasmin")
dat_qdm <- f_read(l_files, i_date, i_date)

# mbcn
# l_files <- str_subset(files_ba_mbcn, fixed(i_rcm_name)) %>% sort %>% as.list
# names(l_files) <- c("pr", "tasmax", "tasmin")
# dat_mbcn <- f_read(l_files, i_date, i_date)

dat_plot_011 <- rbind(
  cbind(dat_raw, ff = "raw"),
  cbind(dat_qdm, ff = "ba-qdm")
  # cbind(dat_mbcn, ff = "ba-mbcn")
)
dat_plot_011[, ff_fct := fct_inorder(ff)]

dat_plot_011 <- dat_plot_011[icell %in% dat_qdm$icell]

# bads
dat_bads <- foreach(i_path = path_bads_uni) %do% {
  
  files_bads <- dir_ls(i_path)
  i_bads <- path_file(i_path)
  
  files_read <- str_subset(files_bads, fixed(i_rcm_name)) %>% sort %>% as.list
  
  if(length(files_read) == 2){
    # no pr
    names(files_read) <- c("tasmax", "tasmin")
    dat_i <- f_read(files_read, i_date, i_date)
    
  } else {
    # with pr
    names(files_read) <- c("pr", "tasmax", "tasmin")
    dat_i <- f_read(files_read, i_date, i_date)
    
  }
  
  cbind(dat_i, bads = i_bads)
} %>% rbindlist(fill = T)

dat_bads[, bads_multi := str_detect(bads, "mbcn")]
dat_bads[, bads_multi_chr := ifelse(bads_multi, "multi", "uni")]
dat_bads[, bads_ds := bads %>% forcats::fct_recode(
  "ds-pcalm" = "ba-qdm-ds-pcalm",
  # "ds-pcalm" = "ba-mbcn-ds-pcalm",
  "ds-qdm" = "ba-qdm-ds-qdm",
  # "ds-qdm" = "ba-mbcn-ds-qdm",
  "ds-qdm2" = "ba-qdm-ds-qdm2",
  # "ds-qdm2" = "ba-mbcn-ds-qdm2",
  "ds-gam" = "ba-qdm-ds-gam",
  # "ds-gam" = "ba-mbcn-ds-gam",
  "ds-lr" = "ba-qdm-ds-lr",
  # "ds-lr" = "ba-mbcn-ds-lr",
  "bads" = "bads-qdm"#,
  # "bads" = "bads-mbcn"   
)]

dat_bads[, bads_ds := factor(bads_ds, levels = c(
  "bads", "ds-pcalm", "ds-qdm", "ds-qdm2", "ds-lr", "ds-gam"
))]

## plot --------------------------------------------------------------------


dat_plot_crespi <- dat_crespi
dat_plot_crespi_011 <- dat_crespi_011
# dat_plot_crespi <- if(i_var %in%  c("tasmin", "tasmax")) dat_crespi else dat_crespi1
# lbl_suffix <- if(i_var %in%  c("tasmin", "tasmax")) "" else " (day+1)"

lims_col <- range(dat_bads[[i_var]], dat_plot_011[[i_var]], dat_plot_crespi[[i_var]],
                  na.rm = T)
lims_x <- range(dat_aux$x, dat_aux_011$lon)
lims_y <- range(dat_aux$y, dat_aux_011$lat)

gg_bads <-
  dat_bads %>% 
  merge(dat_aux) %>% 
  ggplot(aes(x, y, fill = !!sym(i_var)))+
  geom_raster()+
  # scale_fill_viridis_c(limits = lims_col)+
  scale_fill_scico(palette = "lajolla", limits = lims_col)+
  facet_wrap( ~ bads_ds, dir = "v", nrow = 2)+
  xlim(lims_x)+ylim(lims_y)+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())+
  coord_fixed()+
  xlab(NULL)+ylab(NULL)

gg_011 <-
  dat_plot_011 %>% 
  merge(dat_aux_011) %>% 
  ggplot(aes(lon, lat, fill = !!sym(i_var)))+
  geom_raster()+
  # scale_fill_viridis_c(limits = lims_col)+
  scale_fill_scico(palette = "lajolla", limits = lims_col)+
  facet_grid(. ~ ff_fct)+
  xlim(lims_x)+ylim(lims_y)+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())+
  coord_fixed()+
  xlab(NULL)+ylab(NULL)

gg_crespi <- 
  rbind(
    dat_plot_crespi %>% 
      cbind(ff = "obs (1km)") %>% 
      merge(dat_aux),
    dat_plot_crespi_011 %>% 
      cbind(ff = "obs (0.11°)") %>% 
      merge(dat_aux_011[, .(icell, x = lon, y = lat, orog = elevation)])
  ) %>% 
  ggplot(aes(x, y, fill = !!sym(i_var)))+
  geom_raster()+
  # scale_fill_viridis_c(limits = lims_col)+
  scale_fill_scico(palette = "lajolla", limits = lims_col)+
  facet_grid(. ~ fct_inorder(ff))+
  xlim(lims_x)+ylim(lims_y)+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())+
  coord_fixed()+
  xlab(NULL)+ylab(NULL)

gg_out <-
  wrap_plots( #wrap_plots(gg_crespi, gg_crespi_011, nrow = 1), 
    gg_crespi,
    gg_011, 
    gg_bads, 
    ncol = 1,  heights = c(1,1,1.5), guides = "collect")+
  plot_annotation(tag_levels = "a", tag_suffix = ")")


ggsave("fig/paper-ds/ds-example-maps_tasmin.png", 
       gg_out, width = 6.5, height = 8)







# pr ----------------------------------------------------------------------

i_var <- "pr"
i_date <- as.Date("2003-11-01")
i_rcm_name <- "GERICS-REMO2015"

# crespi day+1 for pr and hn
dat_crespi <- f_read(l_file_crespi, i_date + 1, i_date + 1)
dat_crespi_011 <- f_read(l_file_crespi_011, i_date + 1, i_date + 1)

# raw
l_files <- dat_inv_loop[institute_rcm == i_rcm_name, list_files]
names(l_files) <- dat_inv_loop[institute_rcm == i_rcm_name, variable]
dat_raw <- f_read(l_files, i_date, i_date, raw = T)

# qdm
l_files <- str_subset(files_ba_qdm, fixed(i_rcm_name)) %>% sort %>% as.list
names(l_files) <- c("pr", "tasmax", "tasmin")
dat_qdm <- f_read(l_files, i_date, i_date)

# mbcn
# l_files <- str_subset(files_ba_mbcn, fixed(i_rcm_name)) %>% sort %>% as.list
# names(l_files) <- c("pr", "tasmax", "tasmin")
# dat_mbcn <- f_read(l_files, i_date, i_date)

dat_plot_011 <- rbind(
  cbind(dat_raw, ff = "raw"),
  cbind(dat_qdm, ff = "ba-qdm")
  # cbind(dat_mbcn, ff = "ba-mbcn")
)
dat_plot_011[, ff_fct := fct_inorder(ff)]

dat_plot_011 <- dat_plot_011[icell %in% dat_qdm$icell]

# bads
dat_bads <- foreach(i_path = path_bads_uni) %do% {
  
  files_bads <- dir_ls(i_path)
  i_bads <- path_file(i_path)
  
  files_read <- str_subset(files_bads, fixed(i_rcm_name)) %>% sort %>% as.list
  
  if(length(files_read) == 2){
    # no pr
    names(files_read) <- c("tasmax", "tasmin")
    dat_i <- f_read(files_read, i_date, i_date)
    
  } else {
    # with pr
    names(files_read) <- c("pr", "tasmax", "tasmin")
    dat_i <- f_read(files_read, i_date, i_date)
    
  }
  
  cbind(dat_i, bads = i_bads)
} %>% rbindlist(fill = T)

dat_bads[, bads_multi := str_detect(bads, "mbcn")]
dat_bads[, bads_multi_chr := ifelse(bads_multi, "multi", "uni")]
dat_bads[, bads_ds := bads %>% forcats::fct_recode(
  "ds-pcalm" = "ba-qdm-ds-pcalm",
  # "ds-pcalm" = "ba-mbcn-ds-pcalm",
  "ds-qdm" = "ba-qdm-ds-qdm",
  # "ds-qdm" = "ba-mbcn-ds-qdm",
  "ds-qdm2" = "ba-qdm-ds-qdm2",
  # "ds-qdm2" = "ba-mbcn-ds-qdm2",
  "ds-gam" = "ba-qdm-ds-gam",
  # "ds-gam" = "ba-mbcn-ds-gam",
  "ds-lr" = "ba-qdm-ds-lr",
  # "ds-lr" = "ba-mbcn-ds-lr",
  "bads" = "bads-qdm"#,
  # "bads" = "bads-mbcn"   
)]

dat_bads[, bads_ds := factor(bads_ds, levels = c(
  "bads", "ds-pcalm", "ds-qdm", "ds-qdm2", "ds-lr", "ds-gam"
))]


## plot --------------------------------------------------------------------


dat_plot_crespi <- dat_crespi
dat_plot_crespi_011 <- dat_crespi_011
# dat_plot_crespi <- if(i_var %in%  c("tasmin", "tasmax")) dat_crespi else dat_crespi1
# lbl_suffix <- if(i_var %in%  c("tasmin", "tasmax")) "" else " (day+1)"

lims_col <- range(dat_bads[[i_var]], dat_plot_011[[i_var]], dat_plot_crespi[[i_var]],
                  na.rm = T)
lims_x <- range(dat_aux$x, dat_aux_011$lon)
lims_y <- range(dat_aux$y, dat_aux_011$lat)

gg_bads <-
  dat_bads[!is.na(pr)] %>% 
  merge(dat_aux) %>% 
  ggplot(aes(x, y, fill = !!sym(i_var)))+
  geom_raster()+
  scale_fill_scico(palette = "brocO", limits = lims_col, direction = -1, midpoint = 0)+
  facet_wrap( ~ bads_ds, dir = "v", nrow = 2)+
  xlim(lims_x)+ylim(lims_y)+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())+
  coord_fixed()+
  xlab(NULL)+ylab(NULL)

gg_011 <-
  dat_plot_011 %>% 
  merge(dat_aux_011) %>% 
  ggplot(aes(lon, lat, fill = !!sym(i_var)))+
  geom_raster()+
  scale_fill_scico(palette = "brocO", limits = lims_col, direction = -1, midpoint = 0)+
  facet_grid(. ~ ff_fct)+
  xlim(lims_x)+ylim(lims_y)+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())+
  coord_fixed()+
  xlab(NULL)+ylab(NULL)

gg_crespi <- 
  rbind(
    dat_plot_crespi %>% 
      cbind(ff = "obs (1km)") %>% 
      merge(dat_aux),
    dat_plot_crespi_011 %>% 
      cbind(ff = "obs (0.11°)") %>% 
      merge(dat_aux_011[, .(icell, x = lon, y = lat, orog = elevation)])
  ) %>% 
  ggplot(aes(x, y, fill = !!sym(i_var)))+
  geom_raster()+
  scale_fill_scico(palette = "brocO", limits = lims_col, direction = -1, midpoint = 0)+
  facet_grid(. ~ fct_inorder(ff))+
  xlim(lims_x)+ylim(lims_y)+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())+
  coord_fixed()+
  xlab(NULL)+ylab(NULL)

gg_out <-
  wrap_plots( #wrap_plots(gg_crespi, gg_crespi_011, nrow = 1), 
    gg_crespi,
    gg_011, 
    gg_bads, 
    ncol = 1,  heights = c(1,1,2.2), guides = "collect")+
  plot_annotation(tag_levels = "a", tag_suffix = ")")


ggsave("fig/paper-ds/ds-example-maps_pr.png", 
       gg_out, width = 5.5, height = 8)




# hn ----------------------------------------------------------------------

# with multivar

