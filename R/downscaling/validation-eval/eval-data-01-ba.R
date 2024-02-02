# compare BA only step

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
# l_years_train_period <- list(c(1981,2000), c(2001,2020))
pctl <- c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)


# ref data ----------------------------------------------------------------

# eobs
l_file_obs <- list(
  tasmax = "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/tasmax_eobs_v26.nc",
  tasmin = "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/tasmin_eobs_v26.nc",
  pr = "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/pr_eobs_v26.nc"
)

dat_ref <- l_file_obs %>% 
  map(\(f) nc_grid_to_dt(f, date_range = c("1981-01-01", "2020-12-31"))) %>% 
  reduce(merge)
dat_ref[, period20 := ifelse(year(date) <= 2000, "1981-2000", "2001-2020")]

# setnames(dat_ref, c("pr", "tasmin", "tasmax"), c("pr_ref", "tasmin_ref", "tasmax_ref"))
# dat_ref[!is.na(tasmax_ref), hn_ref := snowfall(pr_ref, tasmax_ref, tasmin_ref)]

dat_ref[!is.na(tasmax), hn := snowfall(pr, tasmax, tasmin)]
# takes long, does not really work (ignore warnings with POSIX), since no real forest fire conditions
# dat_ref[!is.na(tasmax),
#         kbdi := ClimInd:::kbdindex(format(date, "%m/%d/%y"), 
#                                    (tasmax+tasmin)/2,
#                                    pr, 
#                                    start.date = min(date)),
#         .(icell)]



dat_ref_long <- melt(dat_ref[!is.na(tasmax)], 
                     id.vars = c("icell", "date", "period20"), 
                     value.name = "value_ref")

# loop --------------------------------------------------------------------


dat_out <- foreach(i_ba = ba_variants, .final = rbindlist) %do% {
  
  files_ba <- dir_ls(path(path_in, str_c("ba-", i_ba)))
  
  foreach(i_centers = 1:6, .final = rbindlist) %do% {
    
    files_read <- str_subset(files_ba, str_c("_", i_centers, "_"))
    dat_i <- files_read %>% 
      map(nc_grid_to_dt) %>% 
      reduce(merge)
    
    dat_i[!is.na(tasmax), hn := snowfall(pr, tasmax, tasmin)]
    
    dat_i_long <- melt(dat_i[!is.na(tasmax)], 
                       id.vars = c("icell", "date"), 
                       value.name = "value")
    
    dat_merge <- merge(dat_i_long, dat_ref_long)
    
    dat_summ <- dat_merge[, 
                          c(mitmatmisc::calc_pctl(value, pctl),
                            mitmatmisc::calc_pctl(value_ref, pctl, prefix = "ref_p"),
                            mean_value = mean(value),
                            mean_value_ref = mean(value_ref),
                            # bias = mean(value) - mean(value_ref),
                            # bias_rel = mean(value)/mean(value_ref),
                            wassersteindist = transport::wasserstein1d(value, value_ref)
                          ),
                          .(icell, period20, month(date), variable)]
    
    dat_summ[, centers := i_centers]
    dat_summ[, ba := i_ba]
    dat_summ
  }
  
}


saveRDS(dat_out, "/home/climatedata/downscaling/validation-cv/rdata-summary/eval-01-ba-only.rds")
