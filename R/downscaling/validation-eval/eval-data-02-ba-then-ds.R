# compare BA then DS

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

path_in <- "/home/climatedata/downscaling/validation-cv/data-daily/"
ba_variants <- c("qm", "qdm", "mbcn")
ds_variants <- c("lrfix", "lrvar")
l_years_train_period <- list(c(1981,2000), c(2001,2020))

l_file_obs <- list(
  tasmax = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MaxTemp.nc",
  tasmin = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MinTemp.nc",
  pr = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_Prec.nc"
)

foreach(i_years = l_years_train_period) %do% {
  
  date_range <- c(str_c(i_years[1], "-01-01"), str_c(i_years[2], "-12-31"))
  
  dat_pr <- nc_grid_to_dt(l_file_obs[["pr"]], date_range = date_range)
  setnames(dat_pr, 3, "pr")
  dat_pr <- dat_pr[!is.na(pr)]
  
  dat_tasmin <- nc_grid_to_dt(l_file_obs[["tasmin"]], date_range = date_range)
  setnames(dat_tasmin, 3, "tasmin")
  dat_tasmin <- dat_tasmin[!is.na(tasmin)]
  
  dat_tasmax <- nc_grid_to_dt(l_file_obs[["tasmax"]], date_range = date_range)
  setnames(dat_tasmax, 3, "tasmax")
  dat_tasmax <- dat_tasmax[!is.na(tasmax)]
  
  dat_ref <- cbind(dat_pr, tasmax = dat_tasmax$tasmax, tasmin = dat_tasmin$tasmin)
  
  dat_ref[, hn := snowfall(pr, tasmax, tasmin)]
  dat_ref_long <- melt(dat_ref, 
                       id.vars = c("icell", "date"), 
                       value.name = "value_ref")
  rm(dat_pr, dat_tasmin, dat_tasmax, dat_ref);gc();
  
  setkey(dat_ref_long, icell, date, variable)
  all_icell <- unique(dat_ref_long$icell)
  all_icell_grp <- cut_number(all_icell, 20, labels = F)
  gc()
  
  # loop --------------------------------------------------------------------
  
  
  foreach(i_ba = ba_variants) %do% {
    
    foreach(i_ds = ds_variants) %do% {
      
      files_bads <- dir_ls(path(path_in, str_c("ba-", i_ba, "-ds-", i_ds)))
      
      foreach(i_centers = 1:6) %do% {
        
        files_read <- str_subset(files_bads, str_c("_", i_centers, "_"))
        
        dat_pr <- nc_grid_to_dt(str_subset(files_read, "pr"), date_range = date_range)
        setnames(dat_pr, 3, "pr")
        dat_pr <- dat_pr[!is.na(pr)]
        
        dat_tasmin <- nc_grid_to_dt(str_subset(files_read, "tasmin"), date_range = date_range)
        setnames(dat_tasmin, 3, "tasmin")
        dat_tasmin <- dat_tasmin[!is.na(tasmin)]
        
        dat_tasmax <- nc_grid_to_dt(str_subset(files_read, "tasmax"), date_range = date_range)
        setnames(dat_tasmax, 3, "tasmax")
        dat_tasmax <- dat_tasmax[!is.na(tasmax)]
        
        dat_i <- cbind(dat_pr, tasmax = dat_tasmax$tasmax, tasmin = dat_tasmin$tasmin)
        rm(dat_pr, dat_tasmin, dat_tasmax);gc();
        
        dat_i[, hn := snowfall(pr, tasmax, tasmin)]
        dat_i_long <- melt(dat_i, 
                             id.vars = c("icell", "date"), 
                             value.name = "value")
        rm(dat_i);gc();
        
        setkey(dat_i_long, icell, date, variable)
        gc()
        
        dat_out <- foreach(i_grp = unique(all_icell_grp), .final = rbindlist) %do% {
          
          i_icell <- all_icell[all_icell_grp == i_grp]
          
          dat_i_long_sub <- dat_i_long[.(i_icell)]
          dat_ref_long_sub <- dat_ref_long[.(i_icell)]
          dat_merge <- merge(dat_i_long_sub, dat_ref_long_sub)
          
          dat_summ <- dat_merge[, 
                                c(map2(mitmatmisc::calc_pctl(value, 0:10/10),
                                       mitmatmisc::calc_pctl(value_ref, 0:10/10),
                                       \(x,y) x - y),
                                  mean_value = mean(value),
                                  mean_value_ref = mean(value_ref),
                                  # bias = mean(value) - mean(value_ref),
                                  # bias_rel = mean(value)/mean(value_ref),
                                  wassersteindist = transport::wasserstein1d(value, value_ref)
                                ),
                                .(icell, month(date), variable)]
          
          dat_summ[, centers := i_centers]
          dat_summ[, ba := i_ba]
          dat_summ[, ds := i_ds]
          dat_summ[, period20 := str_c(i_years, collapse = "-")]
          
          dat_summ
          
        }
        
        file_out <- path("/home/climatedata/downscaling/validation-cv/rdata-summary/eval-ba-then-ds/",
                         str_c(str_c(i_years, collapse = "-"),
                               i_ba, i_ds, i_centers, sep = "_"),
                         ext = "rds")
        
        saveRDS(dat_out, file_out)
        
        rm(dat_i_long); gc();
        
      }
      
      
      
    }
    
    
    
    
    
  }
  
  
  rm(dat_ref_long); gc();
}



