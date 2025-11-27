# 

library(terra)
library(magrittr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(fs)
library(purrr)
library(foreach)
library(stringr)

source("R/functions/inv_sub_reanalysis.R")



# settings ----------------------------------------------------------------

date_rcm_sub <- as.Date(c("1989-01-02", "2008-12-31"))

path_in <- "/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v2/"
path_in_v1 <- "/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v1/"


path_bads <- dir_ls(path_in) %>%
  str_subset("ba-qdm$", negate = T) %>%
  str_subset("ba-mbcn$", negate = T) %>% 
  str_subset("gam$", negate = T) %>% 
  str_subset("lr$", negate = T) %>% 
  str_subset("qdm$", negate = T) %>% 
  c(dir_ls(path_in_v1) %>% str_subset("bads"))



# inventory ---------------------------------------------------------------

dat_inv <- inv_sub_reanalysis()
dat_inv_loop <- dat_inv[variable %in% c("tasmin", "tasmax", "pr")]

dat_inv_loop_mod <- dat_inv_loop[, .(gcm, institute_rcm, experiment, 
                                     ensemble, downscale_realisation)] %>% unique()



# bads ---------------------------------------------------------------------

dat_corr_bads <- foreach(
  i_path_ba = path_bads,
  .final = rbindlist
) %do% {
  
  files_ba <- dir_ls(i_path_ba)
  
  foreach(
    i_inv = 1:nrow(dat_inv_loop_mod),
    .final = rbindlist
  ) %do% {
    
    i_rcm_name <- dat_inv_loop_mod[i_inv, institute_rcm]
    
    # file_rcm_orog <- dat_inv[variable == "orog" & institute_rcm == i_rcm_name, list_files[[1]]]

    i_files_ba <- str_subset(files_ba, fixed(i_rcm_name))
    i_files_ba |> path_file() |> str_split_i("_", 1) -> names(i_files_ba)
    
    dat_rcm <- i_files_ba |> 
      map(nc_grid_to_dt, date_range = date_rcm_sub) |> 
      reduce(merge)
    
    dat_corr <- dat_rcm[!is.na(pr),
                        .(corr_tasmin_tasmax = cor(tasmin, tasmax),
                          corr_pr_tasmin = cor(tasmin, pr),
                          corr_pr_tasmax = cor(tasmax, pr)),
                        .(icell, month(date))]
    
    cbind(dat_corr, rcm = i_rcm_name, bads = path_file(i_path_ba))
    
    
  }
  
  
}



# obs ---------------------------------------------------------------------

l_file_crespi <- list(
  tasmax = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MaxTemp.nc",
  tasmin = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MinTemp.nc",
  pr = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_Prec.nc"
)



dat_obs <- l_file_crespi |> 
  imap(\(x, idx) {
    dat1 <- nc_grid_to_dt(x, date_range = date_rcm_sub)
    setnames(dat1, 3, idx)
    dat1
  }) |> 
  reduce(merge)

dat_corr <- dat_obs[!is.na(pr),
                    .(corr_tasmin_tasmax = cor(tasmin, tasmax),
                      corr_pr_tasmin = cor(tasmin, pr),
                      corr_pr_tasmax = cor(tasmax, pr)),
                    .(icell, month(date))]

dat_corr_obs <- cbind(dat_corr, rcm = "obs", bads = "obs")



# final -------------------------------------------------------------------

dat_out <- rbindlist(list(
  dat_corr_obs, dat_corr_bads
))

saveRDS(dat_out, "data/corr-tp-rcm-1km.rds")

