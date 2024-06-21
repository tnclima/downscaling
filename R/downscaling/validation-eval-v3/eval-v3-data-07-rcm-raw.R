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

source("R/functions/inv_sub.R")
source("R/functions/etccdi.R")
source("R/functions/snowfall.R")
# pctl <- c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)
pctl <- c(0, 0.05, 0.5, 0.95, 1)
# l_years_train_period <- list(c(1981,2000), c(2001,2020))
# l_years_train_period <- list(c(1981:2000), c(2001:2020))
l_years_train_period <- list(c(1981+0:19*2), c(1981+0:19*2+1))


dat_inv <- inv_sub()
dat_inv_loop <- dat_inv[experiment == "rcp85" & 
                          variable %in% c("tasmin", "tasmax", "pr")]
dat_inv_loop_mod <- dat_inv_loop[, .(gcm, institute_rcm, experiment, 
                                     ensemble, downscale_realisation, centers)] %>% unique()

# month ------------------------------------------------------------------


dat_out <- foreach(
  i_inv = 1:nrow(dat_inv_loop_mod),
  .final = rbindlist
) %do% {
  # i_rcm_name <- dat_inv_loop_mod[i_inv, institute_rcm]
  
  # file_rcm_orog <- dat_inv[variable == "orog" & institute_rcm == i_rcm_name, list_files[[1]]]
  dat_inv_loop_mod[i_inv, ] %>% 
    merge(dat_inv_loop) %>% 
    {setNames(unlist(.$list_files), .$variable)} -> files_rcm
  
  dat_rcm <- map(files_rcm, 
                 \(x) nc_grid_to_dt(x, 
                                    date_range = c("1981-01-01", "2020-12-31"),
                                    interpolate_to_standard_calendar = T)) %>% 
    reduce(merge)
  
  dat_rcm[, pr := pr*86400]
  dat_rcm[, tasmax := tasmax-273.15]
  dat_rcm[, tasmin := tasmin-273.15]
  
  dat_rcm[, hn := snowfall(pr, tasmax, tasmin)]
  
  dat_rcm[, period20 := "odd"]
  dat_rcm[year(date) %in% l_years_train_period[[2]], period20 := "even"]
  
  dat_summ <- map(c("pr", "tasmin", "tasmax", "hn"), \(x){
    dat_rcm[, 
            c(mitmatmisc::calc_pctl(value, pctl),
              mean_value = mean(value),
              variable = x),
            .(icell, period20, month(date)),
            env = list(value = x)]
  }) %>% rbindlist
  
  dat_summ[, centers := dat_inv_loop_mod[i_inv, centers]]
  
}


saveRDS(dat_out, "/home/climatedata/downscaling/validation-cv/rdata-summary-v3/eval-rcm-raw-month.rds")



# etccdi ------------------------------------------------------------------


dat_out <- foreach(
  i_inv = 1:nrow(dat_inv_loop_mod),
  .final = rbindlist
) %do% {
  # i_rcm_name <- dat_inv_loop_mod[i_inv, institute_rcm]
  
  # file_rcm_orog <- dat_inv[variable == "orog" & institute_rcm == i_rcm_name, list_files[[1]]]
  dat_inv_loop_mod[i_inv, ] %>% 
    merge(dat_inv_loop) %>% 
    {setNames(unlist(.$list_files), .$variable)} -> files_rcm
  
  dat_rcm <- map(files_rcm, 
                 \(x) nc_grid_to_dt(x, 
                                    date_range = c("1981-01-01", "2020-12-31"),
                                    interpolate_to_standard_calendar = T)) %>% 
    reduce(merge)
  
  dat_rcm[, pr := pr*86400]
  dat_rcm[, tasmax := tasmax-273.15]
  dat_rcm[, tasmin := tasmin-273.15]
  
  # dat_rcm[, hn := snowfall(pr, tasmax, tasmin)]
  
  dat_rcm[, period20 := "odd"]
  dat_rcm[year(date) %in% l_years_train_period[[2]], period20 := "even"]
  
  dat_summ_annual <- map(
    c("tr", "su", "id", "fd", "rx1day", "r20mm", "wd", "sdii", "cdd"), 
    \(ind){
      xx <- formalArgs(ind)
      fun <- get(ind)
      dat_rcm[,
              .(val = fun(xpar)), 
              .(icell, period20, year(date)),
              env = list(xpar = xx)] %>% 
        .[, .(value = mean(val), variable = ind), .(period20, icell)]
    }) %>% rbindlist
  
  dat_summ_annual[, centers := dat_inv_loop_mod[i_inv, centers]]
  
}


saveRDS(dat_out, "/home/climatedata/downscaling/validation-cv/rdata-summary-v3/eval-rcm-raw-etccdi.rds")
