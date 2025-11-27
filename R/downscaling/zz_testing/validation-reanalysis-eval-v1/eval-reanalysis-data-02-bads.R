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

source("R/functions/inv_sub_reanalysis.R")

source("R/functions/snowfall.R")
source("R/functions/etccdi.R")

path_in <- "/home/climatedata/downscaling/validation-cv-reanalysis/data-daily/"
paths_out <- str_c("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary/eval-", 
                   c("season", "season-elev", "season-icell", "etccdi"))
names(paths_out) <- c("season", "season-elev", "season-icell", "etccdi")

path_bads <- dir_ls(path_in) %>% 
  str_subset("ba-qdm$", negate = T) %>% 
  str_subset("ba-mbcn$", negate = T)
  

# l_years_train_period <- readRDS("data/random-years-reanalysis.rds")
pctl <- seq(0, 1, by=0.01)

elev_breaks <- seq(0, 3500, by = 250)



# inv ---------------------------------------------------------------------

dat_inv <- inv_sub_reanalysis()
dat_inv_loop <- dat_inv[variable %in% c("tasmin", "tasmax", "pr")]

dat_inv_loop_mod <- dat_inv_loop[, .(gcm, institute_rcm, experiment, 
                                     ensemble, downscale_realisation)] %>% unique()



# elev --------------------------------------------------------------------

file_orog <- "/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc"
# file_orog <- "/home/climatedata/obs/orography/crespi_lonlat_1km_precipitation.nc"

dat_orog <- nc_grid_to_dt(file_orog, add_xy = T)
dat_orog <- dat_orog[!is.na(orog)]

# dat_orog$orog %>% hist(50)
# dat_orog$orog %>% summary

elev_breaks <- seq(0, 3500, by = 250)
dat_orog[, elev_fct := cut(orog, breaks = elev_breaks, dig.lab = 5)]
# dat_orog %>% ggplot(aes(longitude, latitude, fill = orog))+geom_raster()
# summary(dat_orog$elev_fct)


# main loop ---------------------------------------------------------------


foreach(i_path = path_bads) %do% {
  
  files_bads <- dir_ls(i_path)
  
  foreach(i = 1:nrow(dat_inv_loop_mod)) %do% {
    
    i_rcm_name <- dat_inv_loop_mod[i, institute_rcm]
    files_read <- str_subset(files_bads, fixed(i_rcm_name))
    
    files_out <- path(paths_out,
                      path_file(i_path),
                      i_rcm_name,
                      ext = "rds")
    names(files_out) <- names(paths_out)
    dir_create(path_dir(files_out))
    
    if(all(file_exists(files_out))) return(NULL)
    
    if(any(str_detect(files_read, fixed("pr")))){
      
      dat_pr <- nc_grid_to_dt(str_subset(files_read, "pr"))
      setnames(dat_pr, 3, "pr")
      dat_pr <- dat_pr[!is.na(pr)]
      
      dat_tasmin <- nc_grid_to_dt(str_subset(files_read, "tasmin"))
      setnames(dat_tasmin, 3, "tasmin")
      dat_tasmin <- dat_tasmin[!is.na(tasmin)]
      
      dat_tasmax <- nc_grid_to_dt(str_subset(files_read, "tasmax"))
      setnames(dat_tasmax, 3, "tasmax")
      dat_tasmax <- dat_tasmax[!is.na(tasmax)]
      
      dat_i <- cbind(dat_pr, tasmax = dat_tasmax$tasmax, tasmin = dat_tasmin$tasmin)
      # rm(dat_pr, dat_tasmin, dat_tasmax);gc();
      
      dat_i[, hn := snowfall(pr, tasmax, tasmin)]

      dat_i[, season := mitmatmisc::season_fct(month(date))]
      dat_i <- dat_i %>% merge(dat_orog[, .(icell, elev_fct)])
      
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
      
      saveRDS(dat_summ, files_out["season"])
      saveRDS(dat_summ_icell, files_out["season-icell"])
      saveRDS(dat_summ_elev, files_out["season-elev"])
      saveRDS(dat_summ_etccdi, files_out["etccdi"])
      
    } else {
      
      # dat_pr <- nc_grid_to_dt(str_subset(files_read, "pr"))
      # setnames(dat_pr, 3, "pr")
      # dat_pr <- dat_pr[!is.na(pr)]
      
      dat_tasmin <- nc_grid_to_dt(str_subset(files_read, "tasmin"))
      setnames(dat_tasmin, 3, "tasmin")
      dat_tasmin <- dat_tasmin[!is.na(tasmin)]
      
      dat_tasmax <- nc_grid_to_dt(str_subset(files_read, "tasmax"))
      setnames(dat_tasmax, 3, "tasmax")
      dat_tasmax <- dat_tasmax[!is.na(tasmax)]
      
      dat_i <- cbind(dat_tasmax, tasmin = dat_tasmin$tasmin)
      # dat_i <- cbind(dat_pr, tasmax = dat_tasmax$tasmax, tasmin = dat_tasmin$tasmin)
      # rm(dat_pr, dat_tasmin, dat_tasmax);gc();
      
      # dat_i[, hn := snowfall(pr, tasmax, tasmin)]
      
      dat_i[, season := mitmatmisc::season_fct(month(date))]
      dat_i <- dat_i %>% merge(dat_orog[, .(icell, elev_fct)])
      
      dat_summ <- map(c("tasmin", "tasmax"), \(x){
        dat_i[, 
              .(qval = quantile(value, pctl),
                pctl = pctl,
                variable = x),
              .(season),
              env = list(value = x)]
      }) %>% rbindlist
      
      dat_summ_icell <- map(c("tasmin", "tasmax"), \(x){
        dat_i[, 
              .(qval = quantile(value, pctl),
                pctl = pctl,
                variable = x),
              .(icell, season),
              env = list(value = x)]
      }) %>% rbindlist
      
      dat_summ_elev <- map(c("tasmin", "tasmax"), \(x){
        dat_i[, 
              .(qval = quantile(value, pctl),
                pctl = pctl,
                variable = x),
              .(elev_fct, season),
              env = list(value = x)]
      }) %>% rbindlist
      
      dat_summ_etccdi <- map(
        c("tr", "su", "id", "fd"), 
        \(ind){
          xx <- formalArgs(ind)
          fun <- get(ind)
          dat_i[,
                .(val = fun(xpar)), 
                .(icell, year(date)), 
                env = list(xpar = xx)] %>% 
            .[, .(value = mean(val), variable = ind), .(icell)]
        }) %>% rbindlist
      
      saveRDS(dat_summ, files_out["season"])
      saveRDS(dat_summ_icell, files_out["season-icell"])
      saveRDS(dat_summ_elev, files_out["season-elev"])
      saveRDS(dat_summ_etccdi, files_out["etccdi"])
      
      
    }
    
    
    
  }
  
  
}




