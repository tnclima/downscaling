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

source("R/functions/snowfall.R")
source("R/functions/etccdi.R")

path_in <- "/home/climatedata/downscaling/validation-cv/data-daily-v3/"
path_out <- "/home/climatedata/downscaling/validation-cv/rdata-summary-v3/eval-bads/"
dir_create(path_out)

ba_variants <- c("qm", "qdm", "mbcn")
# l_years_train_period <- list(c(1981,2000), c(2001,2020))
# l_years_train_period <- list(c(1981:2000), c(2001:2020))
l_years_train_period <- list(c(1981+0:19*2), c(1981+0:19*2+1))
# pctl <- c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)
pctl <- c(0, 0.05, 0.5, 0.95, 1)


foreach(i_ba = ba_variants) %do% {
  
  files_bads <- dir_ls(path(path_in, str_c("bads-", i_ba)))
  
  foreach(i_centers = 1:6) %do% {
    
    file_out <- path(path_out,
                     str_c(i_ba, i_centers, "month", sep = "_"),
                     ext = "rds")
    
    file_out_annual <- path(path_out,
                            str_c(i_ba, i_centers, "etccdi", sep = "_"),
                            ext = "rds")
    if(file_exists(file_out_annual)) return(NULL)
    
    files_read <- str_subset(files_bads, str_c("_", i_centers, "_"))
    
    dat_pr <- nc_grid_to_dt(str_subset(files_read, "pr"), icell_raster_pkg = F)
    setnames(dat_pr, 3, "pr")
    dat_pr <- dat_pr[!is.na(pr)]
    
    dat_tasmin <- nc_grid_to_dt(str_subset(files_read, "tasmin"), icell_raster_pkg = F)
    setnames(dat_tasmin, 3, "tasmin")
    dat_tasmin <- dat_tasmin[!is.na(tasmin)]
    
    dat_tasmax <- nc_grid_to_dt(str_subset(files_read, "tasmax"), icell_raster_pkg = F)
    setnames(dat_tasmax, 3, "tasmax")
    dat_tasmax <- dat_tasmax[!is.na(tasmax)]
    
    dat_i <- cbind(dat_pr, tasmax = dat_tasmax$tasmax, tasmin = dat_tasmin$tasmin)
    rm(dat_pr, dat_tasmin, dat_tasmax);gc();
    
    dat_i[, hn := snowfall(pr, tasmax, tasmin)]
    
    dat_i[, period20 := "odd"]
    dat_i[year(date) %in% l_years_train_period[[2]], period20 := "even"]
    
    dat_summ <- map(c("pr", "tasmin", "tasmax", "hn"), \(x){
      dat_i[, 
            c(mitmatmisc::calc_pctl(value, pctl),
              mean_value = mean(value),
              variable = x),
            .(period20, icell, month(date)),
            env = list(value = x)]
    }) %>% rbindlist
    
    dat_summ[, centers := i_centers]
    dat_summ[, ba := i_ba]
    
    saveRDS(dat_summ, file_out)
    
    
    dat_summ_annual <- map(
      c("tr", "su", "id", "fd", "rx1day", "r20mm", "wd", "sdii", "cdd"), 
      \(ind){
        xx <- formalArgs(ind)
        fun <- get(ind)
        dat_i[,
              .(val = fun(xpar)), 
              .(period20, icell, year(date)), 
              env = list(xpar = xx)] %>% 
          .[, .(value = mean(val), variable = ind), .(period20, icell)]
      }) %>% rbindlist
    
    dat_summ_annual[, centers := i_centers]
    dat_summ_annual[, ba := i_ba]
    
    
    saveRDS(dat_summ_annual, file_out_annual)
    
    
  }
  
  
}




