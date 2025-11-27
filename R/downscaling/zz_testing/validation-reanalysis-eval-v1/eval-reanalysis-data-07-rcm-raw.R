# 

library(eurocordexr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(magrittr)
library(ggplot2)
library(fs)
library(stringr)
library(purrr)
library(foreach)
library(terra)

source("R/functions/inv_sub_reanalysis.R")

source("R/functions/snowfall.R")
source("R/functions/etccdi.R")

path_in <- "/home/climatedata/downscaling/validation-cv-reanalysis/data-daily/"

date_sub <- as.Date(c("1989-01-02", "2008-12-31"))
# l_years_train_period <- readRDS("data/random-years-reanalysis.rds")
pctl <- seq(0, 1, by=0.01)

elev_breaks <- c(500, 1000, 1500, 2000, 3000)

# inv ---------------------------------------------------------------------

dat_inv <- inv_sub_reanalysis()
dat_inv_loop <- dat_inv[variable %in% c("tasmin", "tasmax", "pr")]

dat_inv_loop_mod <- dat_inv_loop[, .(gcm, institute_rcm, experiment, 
                                     ensemble, downscale_realisation)] %>% unique()



# tnaa icell --------------------------------------------------------------

sf_tnaa <- readRDS("data/sf-tnaa/tnaa-latlon.rds") %>% sf::st_as_sf()
rs_rcm <- rast("/home/climatedata/downscaling/rcm_lonlat_tnaa_eraint/orog/orog_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r0i0p0_CLMcom-CCLM4-8-17_v1_fx.nc")
rs_rcm_tnaa <- mask(rs_rcm, sf_tnaa)
mat_rcm <- values(rs_rcm_tnaa)
icell_tnaa <- which(!is.na(mat_rcm))

# check
# dt_rcm_orog <- nc_grid_to_dt("/home/climatedata/downscaling/rcm_lonlat_tnaa_eraint/orog/orog_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r0i0p0_CLMcom-CCLM4-8-17_v1_fx.nc",
#                              add_xy = T)
# dt_rcm_orog[icell %in% icell_tnaa] %>% ggplot(aes(lon, lat, fill = orog))+geom_raster()
# dat_pr[date == "1990-05-05" & icell %in% icell_tnaa] %>% merge(dt_rcm_orog, by = "icell") %>% 
#   ggplot(aes(lon, lat, fill = pr))+geom_raster()




# loop --------------------------------------------------------------------


l_season <- list()
l_season_icell <- list()
l_season_elev <- list()

l_etccdi <- list()


for(i in 1:nrow(dat_inv_loop_mod)){
  
  i_rcm_name <- dat_inv_loop_mod[i, institute_rcm]
  
  file_rcm_orog <- dat_inv[variable == "orog" & institute_rcm == i_rcm_name, list_files[[1]]]
  dat_rcm_orog <- nc_grid_to_dt(file_rcm_orog, add_xy = T)
  dat_rcm_orog <- dat_rcm_orog[icell %in% icell_tnaa]
  dat_rcm_orog[, elev_fct := cut(orog, breaks = elev_breaks, dig.lab = 5)]
  
  dat_pr <- nc_grid_to_dt(dat_inv_loop[institute_rcm == i_rcm_name & variable == "pr", list_files[[1]]],
                          date_range = date_sub)
  setnames(dat_pr, 3, "pr")
  dat_pr <- dat_pr[icell %in% icell_tnaa]
  
  dat_tasmin <- nc_grid_to_dt(dat_inv_loop[institute_rcm == i_rcm_name & variable == "tasmin", list_files[[1]]],
                              date_range = date_sub)
  setnames(dat_tasmin, 3, "tasmin")
  dat_tasmin <- dat_tasmin[icell %in% icell_tnaa]
  
  dat_tasmax <- nc_grid_to_dt(dat_inv_loop[institute_rcm == i_rcm_name & variable == "tasmax", list_files[[1]]],
                              date_range = date_sub)
  setnames(dat_tasmax, 3, "tasmax")
  dat_tasmax <- dat_tasmax[icell %in% icell_tnaa]
  
  dat_i <- cbind(dat_pr, tasmax = dat_tasmax$tasmax, tasmin = dat_tasmin$tasmin)
  
  dat_i[, pr := pr*86400]
  dat_i[, tasmax := tasmax-273.15]
  dat_i[, tasmin := tasmin-273.15]
  
  dat_i[, hn := snowfall(pr, tasmax, tasmin)]
  
  dat_i[, season := mitmatmisc::season_fct(month(date))]
  dat_i <- dat_i %>% merge(dat_rcm_orog[, .(icell, elev_fct)])
  
  dat_summ <- map(c("pr", "tasmin", "tasmax", "hn"), \(x){
    dat_i[, 
          .(qval = quantile(value, pctl),
            pctl = pctl,
            variable = x),
          .(season),
          env = list(value = x)]
  }) %>% rbindlist
  
  dat_summ_icell <- map(c("pr", "tasmin", "tasmax", "hn"), \(x){
    dat_i[, 
          .(qval = quantile(value, pctl),
            pctl = pctl,
            variable = x),
          .(icell, season),
          env = list(value = x)]
  }) %>% rbindlist
  
  dat_summ_elev <- map(c("pr", "tasmin", "tasmax", "hn"), \(x){
    dat_i[, 
          .(qval = quantile(value, pctl),
            pctl = pctl,
            variable = x),
          .(elev_fct, season),
          env = list(value = x)]
  }) %>% rbindlist
  
  # dat_summ[, centers := i_centers]
  # dat_summ[, ba := i_ba]
  
  l_season[[i_rcm_name]] <- dat_summ
  l_season_icell[[i_rcm_name]] <- dat_summ_icell
  l_season_elev[[i_rcm_name]] <- dat_summ_elev
  
  
  dat_summ_etccdi <- map(
    c("tr", "su", "id", "fd", "rx1day", "r20mm", "wd", "sdii", "cdd"), 
    \(ind){
      xx <- formalArgs(ind)
      fun <- get(ind)
      dat_i[,
            .(val = fun(xpar)), 
            .(icell, year(date)), 
            env = list(xpar = xx)] %>% 
        .[, .(value = mean(val), variable = ind), .(icell)]
    }) %>% rbindlist
  
  l_etccdi[[i_rcm_name]] <- dat_summ_etccdi
  
}


dat_out_season <- rbindlist(l_season, idcol = "institute_rcm")
dat_out_season_icell <- rbindlist(l_season_icell, idcol = "institute_rcm")
dat_out_season_elev <- rbindlist(l_season_elev, idcol = "institute_rcm")
dat_out_etccdi <- rbindlist(l_etccdi, idcol = "institute_rcm")

saveRDS(dat_out_season, 
        "/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-rcm-raw-season.rds")
saveRDS(dat_out_season_icell, 
        "/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-rcm-raw-season-icell.rds")
saveRDS(dat_out_season_elev, 
        "/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-rcm-raw-season-elev.rds")

saveRDS(dat_out_etccdi, 
        "/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-rcm-raw-etccdi.rds")

