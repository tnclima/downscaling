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
library(twosamples)
library(terra)

source("R/functions/inv_sub_reanalysis.R")

source("R/functions/snowfall.R")
source("R/functions/etccdi.R")

# out data
# 1: mean-pctl
# 2: ecdf
# 3: dist_stat
# 4: etccdi



# settings ----------------------------------------------------------------


path_in <- "/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v2/"
ba_variants <- c("qdm", "mbcn")

path_out <- "/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/"

date_sub <- as.Date(c("1989-01-02", "2008-12-31"))  

pctl_pr <- c(0.95, 0.99, 1)
pctl_tas <- c(0, 0.01, 0.05, 0.5, 0.95, 0.99, 1)
pctl_ecdf <- seq(0, 1, by=0.01)

elev_breaks <- seq(0, 3500, by = 500)

dist_stat_nboots <- 1000 # number of bootstrap iterations for AD, KS, ... test (default pkg twosamples: 2000)


# inv ---------------------------------------------------------------------

dat_inv <- inv_sub_reanalysis()
dat_inv_loop <- dat_inv[variable %in% c("tasmin", "tasmax", "pr")]

dat_inv_loop_mod <- dat_inv_loop[, .(gcm, institute_rcm, experiment, 
                                     ensemble, downscale_realisation)] %>% unique()


# elev --------------------------------------------------------------------

# rcm dependent -> in loop



# common icell upscaled crespi --------------------------------------------

dat1 <- nc_grid_to_dt("/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v2/ba-qdm/pr_CLMcom-CCLM4-8-17_ECMWF-ERAINT_evaluation.nc",
                      date_range = c("2000-01-01", "2000-01-01"))
dat2 <- nc_grid_to_dt("/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v1/ba-qdm/pr_CLMcom-CCLM4-8-17_ECMWF-ERAINT_evaluation.nc",
                      date_range = c("2000-01-01", "2000-01-01"))
icell_common <- intersect(dat1[!is.na(pr), icell], dat2[!is.na(pr), icell])

# tnaa icell --------------------------------------------------------------

# not needed, since common is less area

# sf_tnaa <- readRDS("data/sf-tnaa/tnaa-latlon.rds") %>% sf::st_as_sf()
# rs_rcm <- rast("/home/climatedata/downscaling/rcm_lonlat_tnaa_eraint/orog/orog_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r0i0p0_CLMcom-CCLM4-8-17_v1_fx.nc")
# rs_rcm_tnaa <- mask(rs_rcm, sf_tnaa)
# mat_rcm <- values(rs_rcm_tnaa)
# icell_tnaa <- which(!is.na(mat_rcm))
# 
# rs_rcm_crespi <- rs_rcm
# rs_rcm_crespi[!( 1:ncell(rs_rcm) %in% icell_common)] <- NA


# check
# dt_rcm_orog <- nc_grid_to_dt("/home/climatedata/downscaling/rcm_lonlat_tnaa_eraint/orog/orog_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r0i0p0_CLMcom-CCLM4-8-17_v1_fx.nc",
#                              add_xy = T)
# dt_rcm_orog[icell %in% icell_tnaa] %>% ggplot(aes(lon, lat, fill = orog))+geom_raster()
# dat_pr[date == "1990-05-05" & icell %in% icell_tnaa] %>% merge(dt_rcm_orog, by = "icell") %>% 
#   ggplot(aes(lon, lat, fill = pr))+geom_raster()




# loop raw --------------------------------------------------------------------

dir_create(path(path_out, "tnaa", c("mean-pctl", "ecdf"), "raw"))
dir_create(path(path_out, "elev", c("mean-pctl", "ecdf"), "raw"))
dir_create(path(path_out, "icell", c("mean-pctl", "etccdi"), "raw"))

l_raw <- list()
l_raw_tnaa <- list()
l_raw_elev <- list()


foreach(i = 1:nrow(dat_inv_loop_mod)) %do% {
  
  i_rcm_name <- dat_inv_loop_mod[i, institute_rcm]
  file_rcm_orog <- dat_inv[variable == "orog" & institute_rcm == i_rcm_name, list_files[[1]]]
  dat_rcm_orog <- nc_grid_to_dt(file_rcm_orog, add_xy = T)
  dat_rcm_orog <- dat_rcm_orog[icell %in% icell_common]
  dat_rcm_orog[, elev_fct := cut(orog, breaks = elev_breaks, dig.lab = 5)]
  
  dat_pr <- nc_grid_to_dt(dat_inv_loop[institute_rcm == i_rcm_name & variable == "pr", list_files[[1]]],
                          date_range = date_sub)
  setnames(dat_pr, 3, "pr")
  dat_pr <- dat_pr[icell %in% icell_common]
  
  dat_tasmin <- nc_grid_to_dt(dat_inv_loop[institute_rcm == i_rcm_name & variable == "tasmin", list_files[[1]]],
                              date_range = date_sub)
  setnames(dat_tasmin, 3, "tasmin")
  dat_tasmin <- dat_tasmin[icell %in% icell_common]
  
  dat_tasmax <- nc_grid_to_dt(dat_inv_loop[institute_rcm == i_rcm_name & variable == "tasmax", list_files[[1]]],
                              date_range = date_sub)
  setnames(dat_tasmax, 3, "tasmax")
  dat_tasmax <- dat_tasmax[icell %in% icell_common]
  
  dat_i <- cbind(dat_pr, tasmax = dat_tasmax$tasmax, tasmin = dat_tasmin$tasmin)
  
  dat_i[, pr := pr*86400]
  dat_i[, tasmax := tasmax-273.15]
  dat_i[, tasmin := tasmin-273.15]
  
  dat_i[, hn := snowfall(pr, tasmax, tasmin)]
  
  dat_i[, season := mitmatmisc::season_fct(month(date))]
  dat_i <- dat_i %>% merge(dat_rcm_orog[, .(icell, elev_fct)]) 
  
  
  dat_i_tnaa <- dat_i[icell %in% icell_common,
                      lapply(.SD, mean),
                      .(date, season),
                      .SDcols = c("pr", "hn", "tasmax", "tasmin")]
  
  dat_i_elev <- dat_i[icell %in% icell_common,
                      lapply(.SD, mean),
                      .(date, season, elev_fct),
                      .SDcols = c("pr", "hn", "tasmax", "tasmin")]
  
  # ** tnaa --------------------------------------------------------------------
  
  dat_i_tnaa_out1 <- dat_i_tnaa[, c(
    mitmatmisc::calc_pctl(pr, pctl_pr, "pr_p"),
    pr_mean = mean(pr),
    mitmatmisc::calc_pctl(hn, pctl_pr, "hn_p"),
    hn_mean = mean(hn),
    mitmatmisc::calc_pctl(tasmin, pctl_tas, "tasmin_p"),
    mitmatmisc::calc_pctl(tasmax, pctl_tas, "tasmax_p")
  ), .(season)]
  
  dat_i_tnaa_out2 <- map(c("pr", "tasmin", "tasmax", "hn"), \(x){
    dat_i_tnaa[, 
               .(qval = quantile(value, pctl_ecdf),
                 pctl = pctl_ecdf,
                 variable = x),
               .(season),
               env = list(value = x)]
  }) %>% rbindlist            
  
  saveRDS(dat_i_tnaa_out1, path(path_out, "tnaa", "mean-pctl", "raw", i_rcm_name, ext = "rds"))
  saveRDS(dat_i_tnaa_out2, path(path_out, "tnaa", "ecdf", "raw", i_rcm_name, ext = "rds"))
  
  
  
  # ** elev --------------------------------------------------------------------
  
  dat_i_elev_out1 <- dat_i_elev[, c(
    mitmatmisc::calc_pctl(pr, pctl_pr, "pr_p"),
    pr_mean = mean(pr),
    mitmatmisc::calc_pctl(hn, pctl_pr, "hn_p"),
    hn_mean = mean(hn),
    mitmatmisc::calc_pctl(tasmin, pctl_tas, "tasmin_p"),
    mitmatmisc::calc_pctl(tasmax, pctl_tas, "tasmax_p")
  ), .(season, elev_fct)]
  
  dat_i_elev_out2 <- map(c("pr", "tasmin", "tasmax", "hn"), \(x){
    dat_i_elev[, 
               .(qval = quantile(value, pctl_ecdf),
                 pctl = pctl_ecdf,
                 variable = x),
               .(season, elev_fct),
               env = list(value = x)]
  }) %>% rbindlist            
  
  
  saveRDS(dat_i_elev_out1, path(path_out, "elev", "mean-pctl", "raw", i_rcm_name, ext = "rds"))
  saveRDS(dat_i_elev_out2, path(path_out, "elev", "ecdf", "raw", i_rcm_name, ext = "rds"))
  
  
  # ** icell ----------------------------------------------------------------
  
  
  dat_i_icell_out1 <- dat_i[, c(
    mitmatmisc::calc_pctl(pr, pctl_pr, "pr_p"),
    pr_mean = mean(pr),
    mitmatmisc::calc_pctl(hn, pctl_pr, "hn_p"),
    hn_mean = mean(hn),
    mitmatmisc::calc_pctl(tasmin, pctl_tas, "tasmin_p"),
    mitmatmisc::calc_pctl(tasmax, pctl_tas, "tasmax_p")
  ), .(season, icell)]
  
  
  dat_i_icell_out4 <- map(
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
  
  saveRDS(dat_i_icell_out1, path(path_out, "icell", "mean-pctl", "raw", i_rcm_name, ext = "rds"))
  saveRDS(dat_i_icell_out4, path(path_out, "icell", "etccdi", "raw", i_rcm_name, ext = "rds"))
  
  l_raw[[i_rcm_name]] <- dat_i
  l_raw_tnaa[[i_rcm_name]] <- dat_i_tnaa
  l_raw_elev[[i_rcm_name]] <- dat_i_elev
  
  return(NULL) 
}



# loop ba -----------------------------------------------------------------



for(i_ba in ba_variants){
  
  files_ba <- dir_ls(path(path_in, str_c("ba-", i_ba)))

  
  dir_create(path(path_out, "tnaa", c("mean-pctl", "ecdf", "dist-stat"),str_c("ba-", i_ba)))
  dir_create(path(path_out, "elev", c("mean-pctl", "ecdf", "dist-stat"), str_c("ba-", i_ba)))
  dir_create(path(path_out, "icell", c("mean-pctl", "dist-stat", "etccdi"), str_c("ba-", i_ba)))
    
  for(i in 1:nrow(dat_inv_loop_mod)){
    
    i_rcm_name <- dat_inv_loop_mod[i, institute_rcm]
    files_read <- str_subset(files_ba, fixed(i_rcm_name))
    
    file_rcm_orog <- dat_inv[variable == "orog" & institute_rcm == i_rcm_name, list_files[[1]]]
    dat_rcm_orog <- nc_grid_to_dt(file_rcm_orog, add_xy = T)
    dat_rcm_orog <- dat_rcm_orog[icell %in% icell_common]
    dat_rcm_orog[, elev_fct := cut(orog, breaks = elev_breaks, dig.lab = 5)]
                                 
    dat_pr <- nc_grid_to_dt(str_subset(files_read, "pr"))
    setnames(dat_pr, 3, "pr")
    dat_pr <- dat_pr[icell %in% icell_common]
    
    dat_tasmin <- nc_grid_to_dt(str_subset(files_read, "tasmin"))
    setnames(dat_tasmin, 3, "tasmin")
    dat_tasmin <- dat_tasmin[icell %in% icell_common]
    
    dat_tasmax <- nc_grid_to_dt(str_subset(files_read, "tasmax"))
    setnames(dat_tasmax, 3, "tasmax")
    dat_tasmax <- dat_tasmax[icell %in% icell_common]
    
    dat_i <- cbind(dat_pr, tasmax = dat_tasmax$tasmax, tasmin = dat_tasmin$tasmin)
    
    dat_i[, hn := snowfall(pr, tasmax, tasmin)]
    
    dat_i[, season := mitmatmisc::season_fct(month(date))]
    dat_i <- dat_i %>% merge(dat_rcm_orog[, .(icell, elev_fct)])
    
    
    dat_i_tnaa <- dat_i[icell %in% icell_common,
                        lapply(.SD, mean),
                        .(date, season),
                        .SDcols = c("pr", "hn", "tasmax", "tasmin")]
    
    dat_i_elev <- dat_i[icell %in% icell_common,
                        lapply(.SD, mean),
                        .(date, season, elev_fct),
                        .SDcols = c("pr", "hn", "tasmax", "tasmin")]
    
    

    dat_i <- merge(dat_i,  
                   l_raw[[i_rcm_name]] %>% 
                     dplyr::rename_with(~str_c(.x, "_raw"), c(pr, tasmin, tasmax, hn)),
                   by = c("icell", "date", "season", "elev_fct"))
    dat_i_tnaa <- merge(dat_i_tnaa,
                        l_raw_tnaa[[i_rcm_name]] %>% 
                          dplyr::rename_with(~str_c(.x, "_raw"), c(pr, tasmin, tasmax, hn)))
    dat_i_elev <- merge(dat_i_elev, 
                        l_raw_elev[[i_rcm_name]] %>% 
                          dplyr::rename_with(~str_c(.x, "_raw"), c(pr, tasmin, tasmax, hn)),)
    
    # ** tnaa --------------------------------------------------------------------
    
    dat_i_tnaa_out1 <- dat_i_tnaa[, c(
      mitmatmisc::calc_pctl(pr, pctl_pr, "pr_p"),
      pr_mean = mean(pr),
      mitmatmisc::calc_pctl(hn, pctl_pr, "hn_p"),
      hn_mean = mean(hn),
      mitmatmisc::calc_pctl(tasmin, pctl_tas, "tasmin_p"),
      mitmatmisc::calc_pctl(tasmax, pctl_tas, "tasmax_p")
    ), .(season)]
    
    dat_i_tnaa_out2 <- map(c("pr", "tasmin", "tasmax", "hn"), \(x){
      dat_i_tnaa[, 
                 .(qval = quantile(value, pctl_ecdf),
                   pctl = pctl_ecdf,
                   variable = x),
                 .(season),
                 env = list(value = x)]
    }) %>% rbindlist            
    
    
    dat_i_tnaa_out3 <-  map(c("pr", "tasmin", "tasmax", "hn"), \(x){
      
      map(c("ad_test", "cvm_test", "ks_test", "wass_test"), \(y){
        dat_i_tnaa[, 
                   as.list(f_test(value, value_raw, nboots = dist_stat_nboots, keep.boots = F)) %>% 
                     setNames(c("dist_stat", "pval")) %>% 
                     c(dist_test = y, variable = x),
                   .(season),
                   env = list(f_test = y, value = x, value_raw = str_c(x, "_raw"))]
      }) %>% rbindlist
      
    }) %>% rbindlist      
    
    
    
    saveRDS(dat_i_tnaa_out1, path(path_out, "tnaa", "mean-pctl", str_c("ba-", i_ba), i_rcm_name, ext = "rds"))
    saveRDS(dat_i_tnaa_out2, path(path_out, "tnaa", "ecdf", str_c("ba-", i_ba), i_rcm_name, ext = "rds"))
    saveRDS(dat_i_tnaa_out3, path(path_out, "tnaa", "dist-stat", str_c("ba-", i_ba), i_rcm_name, ext = "rds"))
    
    
    
    # ** elev --------------------------------------------------------------------
    
    dat_i_elev_out1 <- dat_i_elev[, c(
      mitmatmisc::calc_pctl(pr, pctl_pr, "pr_p"),
      pr_mean = mean(pr),
      mitmatmisc::calc_pctl(hn, pctl_pr, "hn_p"),
      hn_mean = mean(hn),
      mitmatmisc::calc_pctl(tasmin, pctl_tas, "tasmin_p"),
      mitmatmisc::calc_pctl(tasmax, pctl_tas, "tasmax_p")
    ), .(season, elev_fct)]
    
    dat_i_elev_out2 <- map(c("pr", "tasmin", "tasmax", "hn"), \(x){
      dat_i_elev[, 
                 .(qval = quantile(value, pctl_ecdf),
                   pctl = pctl_ecdf,
                   variable = x),
                 .(season, elev_fct),
                 env = list(value = x)]
    }) %>% rbindlist            
    
    
    dat_i_elev_out3 <-  map(c("pr", "tasmin", "tasmax", "hn"), \(x){
      
      map(c("ad_test", "cvm_test", "ks_test", "wass_test"), \(y){
        dat_i_elev[, 
                   as.list(f_test(value, value_raw, nboots = dist_stat_nboots, keep.boots = F)) %>% 
                     setNames(c("dist_stat", "pval")) %>% 
                     c(dist_test = y, variable = x),
                   .(season, elev_fct),
                   env = list(f_test = y, value = x, value_raw = str_c(x, "_raw"))]
      }) %>% rbindlist
      
    }) %>% rbindlist      
    
    
    saveRDS(dat_i_elev_out1, path(path_out, "elev", "mean-pctl", str_c("ba-", i_ba), i_rcm_name, ext = "rds"))
    saveRDS(dat_i_elev_out2, path(path_out, "elev", "ecdf", str_c("ba-", i_ba), i_rcm_name, ext = "rds"))
    saveRDS(dat_i_elev_out3, path(path_out, "elev", "dist-stat", str_c("ba-", i_ba), i_rcm_name, ext = "rds"))
    
    
    # ** icell ----------------------------------------------------------------
    
    
    dat_i_icell_out1 <- dat_i[, c(
      mitmatmisc::calc_pctl(pr, pctl_pr, "pr_p"),
      pr_mean = mean(pr),
      mitmatmisc::calc_pctl(hn, pctl_pr, "hn_p"),
      hn_mean = mean(hn),
      mitmatmisc::calc_pctl(tasmin, pctl_tas, "tasmin_p"),
      mitmatmisc::calc_pctl(tasmax, pctl_tas, "tasmax_p")
    ), .(season, icell)]
    
    
    
    dat_i_icell_out3 <- map(c("pr", "tasmin", "tasmax", "hn"), \(x){
      
      map(c("ad_test", "cvm_test", "ks_test", "wass_test"), \(y){
        dat_i[,
              as.list(f_test(value, value_raw, nboots = dist_stat_nboots, keep.boots = F)) %>%
                setNames(c("dist_stat", "pval")) %>%
                c(dist_test = y, variable = x),
              .(season, icell),
              env = list(f_test = y, value = x, value_raw = str_c(x, "_raw"))]
      }) %>% rbindlist
      
    }) %>% rbindlist
    
    dat_i_icell_out4 <- map(
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
    
    saveRDS(dat_i_icell_out1, path(path_out, "icell", "mean-pctl", str_c("ba-", i_ba), i_rcm_name, ext = "rds"))
    saveRDS(dat_i_icell_out3, path(path_out, "icell", "dist-stat", str_c("ba-", i_ba), i_rcm_name, ext = "rds"))
    saveRDS(dat_i_icell_out4, path(path_out, "icell", "etccdi", str_c("ba-", i_ba), i_rcm_name, ext = "rds"))
    
    
  }
  
}

