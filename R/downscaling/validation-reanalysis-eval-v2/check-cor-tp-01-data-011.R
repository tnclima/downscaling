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



# raw ---------------------------------------------------------------------

dat_inv <- inv_sub_reanalysis()
dat_inv_loop <- dat_inv[variable %in% c("tasmin", "tasmax", "pr")]

dat_inv_loop_mod <- dat_inv_loop[, .(gcm, institute_rcm, experiment, 
                                     ensemble, downscale_realisation)] %>% unique()



dat_corr_raw <- foreach(
  i_inv = 1:nrow(dat_inv_loop_mod),
  .final = rbindlist
) %do% {
  
  i_rcm_name <- dat_inv_loop_mod[i_inv, institute_rcm]
  
  # file_rcm_orog <- dat_inv[variable == "orog" & institute_rcm == i_rcm_name, list_files[[1]]]
  dat_inv_loop_mod[i_inv, ] %>% 
    merge(dat_inv_loop) %>% 
    {setNames(unlist(.$list_files), .$variable)} -> files_rcm
  
  dat_rcm <- files_rcm |> 
    map(nc_grid_to_dt, date_range = date_rcm_sub) |> 
    reduce(merge)
  
  dat_corr <- dat_rcm[, .(corr_tasmin_tasmax = cor(tasmin, tasmax),
              corr_pr_tasmin = cor(tasmin, pr),
              corr_pr_tasmax = cor(tasmax, pr)) , .(icell, month(date))]
  
  cbind(dat_corr, rcm = i_rcm_name, ff = "raw")
  
}


# qdm ---------------------------------------------------------------------

files_ba <- dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v2/ba-qdm/")

dat_corr_qdm <- foreach(
  i_inv = 1:nrow(dat_inv_loop_mod),
  .final = rbindlist
) %do% {
  
  i_rcm_name <- dat_inv_loop_mod[i_inv, institute_rcm]
  
  i_files_ba <- str_subset(files_ba, fixed(i_rcm_name))
  i_files_ba |> path_file() |> str_split_i("_", 1) -> names(i_files_ba)
  
  dat_rcm <- i_files_ba |> 
    map(nc_grid_to_dt, date_range = date_rcm_sub) |> 
    reduce(merge)
  
  dat_corr <- dat_rcm[, .(corr_tasmin_tasmax = cor(tasmin, tasmax),
                          corr_pr_tasmin = cor(tasmin, pr),
                          corr_pr_tasmax = cor(tasmax, pr)) , .(icell, month(date))]
  
  cbind(dat_corr, rcm = i_rcm_name, ff = "qdm")
  
}


# mbcn --------------------------------------------------------------------

files_ba <- dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v2/ba-mbcn/")

dat_corr_mbcn <- foreach(
  i_inv = 1:nrow(dat_inv_loop_mod),
  .final = rbindlist
) %do% {
  
  i_rcm_name <- dat_inv_loop_mod[i_inv, institute_rcm]
  
  i_files_ba <- str_subset(files_ba, fixed(i_rcm_name))
  i_files_ba |> path_file() |> str_split_i("_", 1) -> names(i_files_ba)
  
  dat_rcm <- i_files_ba |> 
    map(nc_grid_to_dt, date_range = date_rcm_sub) |> 
    reduce(merge)
  
  dat_corr <- dat_rcm[, .(corr_tasmin_tasmax = cor(tasmin, tasmax),
                          corr_pr_tasmin = cor(tasmin, pr),
                          corr_pr_tasmax = cor(tasmax, pr)) , .(icell, month(date))]
  
  cbind(dat_corr, rcm = i_rcm_name, ff = "mbcn")
  
}



# obs ---------------------------------------------------------------------


l_file_obs <- list(
  tasmax = "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/tasmax_crespi.nc",
  tasmin = "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/tasmin_crespi.nc",
  pr = "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/pr_crespi.nc"
)


dat_obs <- l_file_obs |> 
  imap(\(x, idx) {
    dat1 <- nc_grid_to_dt(x, date_range = date_rcm_sub)
    setnames(dat1, 3, idx)
    dat1
  }) |> 
  reduce(merge)

dat_corr <- dat_obs[, .(corr_tasmin_tasmax = cor(tasmin, tasmax),
                        corr_pr_tasmin = cor(tasmin, pr),
                        corr_pr_tasmax = cor(tasmax, pr)) , .(icell, month(date))]

dat_corr_obs <- cbind(dat_corr, rcm = "obs", ff = "obs")



# final -------------------------------------------------------------------

dat_out <- rbindlist(list(
  dat_corr_obs, dat_corr_raw, dat_corr_qdm, dat_corr_mbcn
))

saveRDS(dat_out, "data/corr-tp-rcm-011.rds")

