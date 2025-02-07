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

path_in <- "/home/climatedata/downscaling/validation-cv/data-daily-v2/"
ba_variants <- c("qm", "qdm", "mbcn")
l_years_train_period <- list(c(1981,2000), c(2001,2020))
# pctl <- c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)
pctl <- c(0, 0.05, 0.5, 0.95, 1)

# loop --------------------------------------------------------------------


l_month <- list()
l_etccdi <- list()



for(i_ba in ba_variants){
  
  files_ba <- dir_ls(path(path_in, str_c("ba-", i_ba)))
  
  for(i_centers in 1:6){
    
    files_read <- str_subset(files_ba, str_c("_", i_centers, "_"))
    
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
    
    dat_i[, hn := snowfall(pr, tasmax, tasmin)]
    dat_i[, period20 := ifelse(year(date) <= 2000, "1981-2000", "2001-2020")]
    
    
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
    
    l_month[[i_ba]][[i_centers]] <- dat_summ
    
    
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
    
    l_etccdi[[i_ba]][[i_centers]] <- dat_summ_annual
    
  }
  
}

dat_out_month <- l_month %>% lapply(rbindlist) %>% rbindlist()
dat_out_etccdi <- l_etccdi %>% lapply(rbindlist) %>% rbindlist()

saveRDS(dat_out_month, 
        "/home/climatedata/downscaling/validation-cv/rdata-summary-v2/eval-ba-month.rds")

saveRDS(dat_out_etccdi, 
        "/home/climatedata/downscaling/validation-cv/rdata-summary-v2/eval-ba-etccdi.rds")

