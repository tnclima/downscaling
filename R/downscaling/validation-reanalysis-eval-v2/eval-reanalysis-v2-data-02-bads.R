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
# 5: spatcor
# 6: metrics

# settings ----------------------------------------------------------------




path_in <- "/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v2/"
path_in_v1 <- "/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v1/"

path_bads <- dir_ls(path_in) %>%
  str_subset("ba-qdm$", negate = T) %>%
  str_subset("ba-mbcn$", negate = T) %>% 
  c(dir_ls(path_in_v1) %>% str_subset("bads"))

# only redo subset
# path_bads <- dir_ls(path_in) %>%
#   str_subset("ds-lr$", negate = F)

path_out <- "/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/"
dir_create(path(path_out, "tnaa", c("mean-pctl", "ecdf", "dist-stat", "metrics")))
dir_create(path(path_out, "elev", c("mean-pctl", "ecdf", "dist-stat", "metrics")))
dir_create(path(path_out, "icell", c("mean-pctl", "dist-stat", "etccdi", "spatcor", "metrics")))


date_sub <- as.Date(c("1989-01-02", "2008-12-31"))  

pctl_pr <- c(0.95, 0.99, 1)
pctl_tas <- c(0, 0.01, 0.05, 0.5, 0.95, 0.99, 1)
pctl_ecdf <- seq(0, 1, by=0.01)

elev_breaks <- seq(0, 3500, by = 500)

dist_stat_nboots <- 1000 # number of bootstrap iterations for AD, KS, ... test (default pkg twosamples: 2000)
# n_cores <- 16 # number of cores for dist_stat icell

# mitmatmisc::init_parallel_ubuntu_fork(n_cores)

# inv ---------------------------------------------------------------------

dat_inv <- inv_sub_reanalysis()
dat_inv_loop <- dat_inv[variable %in% c("tasmin", "tasmax", "pr")]

dat_inv_loop_mod <- dat_inv_loop[, .(gcm, institute_rcm, experiment, 
                                     ensemble, downscale_realisation)] %>% unique()



# elev --------------------------------------------------------------------

# file_orog <- "/home/climatedata/downscaling/obs4rcm_lonlat_tnaa/orog_eudem_1km.nc"
file_orog <- "/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc"
# file_orog <- "/home/climatedata/obs/orography/crespi_lonlat_1km_precipitation.nc"

dat_orog <- nc_grid_to_dt(file_orog, add_xy = T)
dat_orog <- dat_orog[!is.na(orog)]

# dat_orog$orog %>% hist(50)
# dat_orog$orog %>% summary

dat_orog[, elev_fct := cut(orog, breaks = elev_breaks, dig.lab = 5)]
# dat_orog %>% ggplot(aes(longitude, latitude, fill = orog))+geom_raster()
# summary(dat_orog$elev_fct)



# common icell upscaled crespi --------------------------------------------

dat1 <- nc_grid_to_dt("/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v2/ba-qdm-ds-qdm/pr_CLMcom-CCLM4-8-17_ECMWF-ERAINT_evaluation.nc",
                      date_range = c("2000-01-01", "2000-01-01"))
dat2 <- nc_grid_to_dt("/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v1/bads-qdm/pr_CLMcom-CCLM4-8-17_ECMWF-ERAINT_evaluation.nc",
                      date_range = c("2000-01-01", "2000-01-01"))
icell_common <- intersect(dat1[!is.na(pr), icell], dat2[!is.na(pr), icell])



# spatcor fun -------------------------------------------------------------


# template raster for autocor
rr_template <- rast(file_orog)

f_autocor <- function(dat, cl){
  # if(all(dat[[cl]] == 0, na.rm = T)) return(data.table())
  rr0 <- rast(rr_template)
  rr0[dat$icell] <- dat[[cl]]
  rr0[!(1:ncell(rr0) %in% icell_common)] <- NA
  data.table(morani = autocor(rr0, method = "moran"),
             gearyc = autocor(rr0, method = "geary"))
}

# crespi data -------------------------------------------------------------



l_file_obs <- list(
  tasmax = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MaxTemp.nc",
  tasmin = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MinTemp.nc",
  pr = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_Prec.nc"
)

dat_pr <- nc_grid_to_dt(l_file_obs[["pr"]], date_range = date_sub)
setnames(dat_pr, 3, "pr_crespi")
dat_pr <- dat_pr[!is.na(pr_crespi)]

dat_tasmin <- nc_grid_to_dt(l_file_obs[["tasmin"]], date_range = date_sub)
setnames(dat_tasmin, 3, "tasmin_crespi")
dat_tasmin <- dat_tasmin[!is.na(tasmin_crespi)]

dat_tasmax <- nc_grid_to_dt(l_file_obs[["tasmax"]], date_range = date_sub)
setnames(dat_tasmax, 3, "tasmax_crespi")
dat_tasmax <- dat_tasmax[!is.na(tasmax_crespi)]

dat_crespi <- cbind(dat_pr, 
                    tasmax_crespi = dat_tasmax$tasmax_crespi, 
                    tasmin_crespi = dat_tasmin$tasmin_crespi)

dat_crespi[, hn_crespi := snowfall(pr_crespi, tasmax_crespi, tasmin_crespi)]

rm(dat_pr, dat_tasmax, dat_tasmin); gc();

dat_crespi[pr_crespi < 0, pr_crespi := 0]
dat_crespi[hn_crespi < 0, hn_crespi := 0]
dat_crespi[, season := mitmatmisc::season_fct(month(date))]
dat_crespi <- dat_crespi %>% merge(dat_orog[, .(icell, elev_fct)])

dat_crespi_tnaa <- dat_crespi[icell %in% icell_common,
                              lapply(.SD, mean),
                              .(date, season),
                              .SDcols = str_c(c("pr", "hn", "tasmax", "tasmin"), "_crespi")]

dat_crespi_elev <- dat_crespi[icell %in% icell_common,
                              lapply(.SD, mean),
                              .(date, season, elev_fct),
                              .SDcols = str_c(c("pr", "hn", "tasmax", "tasmin"), "_crespi")]

# out data
dat_crespi_tnaa_out1 <- dat_crespi_tnaa[, c(
  mitmatmisc::calc_pctl(pr_crespi, pctl_pr, "pr_crespi_p"),
  pr_crespi_mean = mean(pr_crespi),
  mitmatmisc::calc_pctl(hn_crespi, pctl_pr, "hn_crespi_p"),
  hn_crespi_mean = mean(hn_crespi),
  mitmatmisc::calc_pctl(tasmin_crespi, pctl_tas, "tasmin_crespi_p"),
  mitmatmisc::calc_pctl(tasmax_crespi, pctl_tas, "tasmax_crespi_p")
), .(season)]

dat_crespi_tnaa_out2 <- map(str_c(c("pr", "tasmin", "tasmax", "hn"), "_crespi"), \(x){
  dat_crespi_tnaa[, 
                  .(qval = quantile(value, pctl_ecdf),
                    pctl = pctl_ecdf,
                    variable = x),
                  .(season),
                  env = list(value = x)]
}) %>% rbindlist

dat_crespi_elev_out1 <- dat_crespi_elev[, c(
  mitmatmisc::calc_pctl(pr_crespi, pctl_pr, "pr_crespi_p"),
  pr_crespi_mean = mean(pr_crespi),
  mitmatmisc::calc_pctl(hn_crespi, pctl_pr, "hn_crespi_p"),
  hn_crespi_mean = mean(hn_crespi),
  mitmatmisc::calc_pctl(tasmin_crespi, pctl_tas, "tasmin_crespi_p"),
  mitmatmisc::calc_pctl(tasmax_crespi, pctl_tas, "tasmax_crespi_p")
), .(season, elev_fct)]

dat_crespi_elev_out2 <- map(str_c(c("pr", "tasmin", "tasmax", "hn"), "_crespi"), \(x){
  dat_crespi_elev[, 
                  .(qval = quantile(value, pctl_ecdf),
                    pctl = pctl_ecdf,
                    variable = x),
                  .(season, elev_fct),
                  env = list(value = x)]
}) %>% rbindlist

dat_crespi_icell_out1 <-  dat_crespi[, c(
  mitmatmisc::calc_pctl(pr_crespi, pctl_pr, "pr_crespi_p"),
  pr_crespi_mean = mean(pr_crespi),
  mitmatmisc::calc_pctl(hn_crespi, pctl_pr, "hn_crespi_p"),
  hn_crespi_mean = mean(hn_crespi),
  mitmatmisc::calc_pctl(tasmin_crespi, pctl_tas, "tasmin_crespi_p"),
  mitmatmisc::calc_pctl(tasmax_crespi, pctl_tas, "tasmax_crespi_p")
), .(season, icell)]

dat_crespi_icell_out5 <- map(str_c(c("pr", "tasmin", "tasmax", "hn"), "_crespi"), \(x){
  dat_crespi[, 
             c(f_autocor(.SD, x), variable = x), 
             .(season, date)]
}) %>% rbindlist



# save
saveRDS(dat_crespi_tnaa_out1, path(path_out, "tnaa", "mean-pctl", "crespi.rds"))
saveRDS(dat_crespi_tnaa_out2, path(path_out, "tnaa", "ecdf", "crespi.rds"))
saveRDS(dat_crespi_elev_out1, path(path_out, "elev", "mean-pctl", "crespi.rds"))
saveRDS(dat_crespi_elev_out2, path(path_out, "elev", "ecdf", "crespi.rds"))
saveRDS(dat_crespi_icell_out1, path(path_out, "icell", "mean-pctl", "crespi.rds"))
saveRDS(dat_crespi_icell_out5, path(path_out, "icell", "spatcor", "crespi.rds"))

# main loop ---------------------------------------------------------------


foreach(i_path = path_bads) %do% {
  
  files_bads <- dir_ls(i_path)
  i_bads <- path_file(i_path)
  
  dir_create(path(path_out, "tnaa", c("mean-pctl", "ecdf", "dist-stat", "metrics"), i_bads))
  dir_create(path(path_out, "elev", c("mean-pctl", "ecdf", "dist-stat", "metrics"), i_bads))
  dir_create(path(path_out, "icell", c("mean-pctl", "dist-stat", "etccdi", "spatcor", "metrics"), i_bads))
  
  foreach(i = 1:nrow(dat_inv_loop_mod)) %do% {
    
    i_rcm_name <- dat_inv_loop_mod[i, institute_rcm]
    files_read <- str_subset(files_bads, fixed(i_rcm_name))
    
    # files_out <- path(paths_out,
    #                   path_file(i_path),
    #                   i_rcm_name,
    #                   ext = "rds")
    # names(files_out) <- names(paths_out)
    # dir_create(path_dir(files_out))
    # 
    # if(all(file_exists(files_out))) return(NULL)
    
    if(any(str_detect(files_read, fixed("pr")))){
      
      

# *pr ---------------------------------------------------------------------

      
      
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
      rm(dat_pr, dat_tasmin, dat_tasmax);gc();
      
      dat_i[, hn := snowfall(pr, tasmax, tasmin)]

      dat_i[, season := mitmatmisc::season_fct(month(date))]
      dat_i <- dat_i %>% merge(dat_orog[, .(icell, elev_fct)])
      
      
      dat_i_tnaa <- dat_i[icell %in% icell_common,
                          lapply(.SD, mean),
                          .(date, season),
                          .SDcols = c("pr", "hn", "tasmax", "tasmin")]
      
      dat_i_elev <- dat_i[icell %in% icell_common,
                          lapply(.SD, mean),
                          .(date, season, elev_fct),
                          .SDcols = c("pr", "hn", "tasmax", "tasmin")]
      
      dat_i <- merge(dat_i, dat_crespi, by = c("icell", "date", "season", "elev_fct"))
      dat_i_tnaa <- merge(dat_i_tnaa, dat_crespi_tnaa)
      dat_i_elev <- merge(dat_i_elev, dat_crespi_elev)

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
                     as.list(f_test(value, value_crespi, nboots = dist_stat_nboots, keep.boots = F)) %>% 
                       setNames(c("dist_stat", "pval")) %>% 
                       c(dist_test = y, variable = x),
                     .(season),
                     env = list(f_test = y, value = x, value_crespi = str_c(x, "_crespi"))]
        }) %>% rbindlist
        
      }) %>% rbindlist      
      
      dat_i_tnaa_out6 <- map(c("pr", "tasmin", "tasmax", "hn"), \(x){
        dat_i_tnaa[, 
                   .(mae = mean(abs(value - value_crespi)),
                     bias = mean(value - value_crespi),
                     corr = cor(value, value_crespi),
                     variable = x),
                   .(season),
                   env = list(value = x, value_crespi = str_c(x, "_crespi"))]
      }) %>% rbindlist   
      
      
      
      saveRDS(dat_i_tnaa_out1, path(path_out, "tnaa", "mean-pctl", i_bads, i_rcm_name, ext = "rds"))
      saveRDS(dat_i_tnaa_out2, path(path_out, "tnaa", "ecdf", i_bads, i_rcm_name, ext = "rds"))
      saveRDS(dat_i_tnaa_out3, path(path_out, "tnaa", "dist-stat", i_bads, i_rcm_name, ext = "rds"))
      saveRDS(dat_i_tnaa_out6, path(path_out, "tnaa", "metrics", i_bads, i_rcm_name, ext = "rds"))
    


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
                     as.list(f_test(value, value_crespi, nboots = dist_stat_nboots, keep.boots = F)) %>% 
                       setNames(c("dist_stat", "pval")) %>% 
                       c(dist_test = y, variable = x),
                     .(season, elev_fct),
                     env = list(f_test = y, value = x, value_crespi = str_c(x, "_crespi"))]
        }) %>% rbindlist
        
      }) %>% rbindlist      
      
      dat_i_elev_out6 <- map(c("pr", "tasmin", "tasmax", "hn"), \(x){
        dat_i_elev[, 
                   .(mae = mean(abs(value - value_crespi)),
                     bias = mean(value - value_crespi),
                     corr = cor(value, value_crespi),
                     variable = x),
                   .(season, elev_fct),
                   env = list(value = x, value_crespi = str_c(x, "_crespi"))]
      }) %>% rbindlist   
      
      
      saveRDS(dat_i_elev_out1, path(path_out, "elev", "mean-pctl", i_bads, i_rcm_name, ext = "rds"))
      saveRDS(dat_i_elev_out2, path(path_out, "elev", "ecdf", i_bads, i_rcm_name, ext = "rds"))
      saveRDS(dat_i_elev_out3, path(path_out, "elev", "dist-stat", i_bads, i_rcm_name, ext = "rds"))
      saveRDS(dat_i_elev_out6, path(path_out, "elev", "metrics", i_bads, i_rcm_name, ext = "rds"))
      

# ** icell ----------------------------------------------------------------

      
      dat_i_icell_out1 <- dat_i[, c(
        mitmatmisc::calc_pctl(pr, pctl_pr, "pr_p"),
        pr_mean = mean(pr),
        mitmatmisc::calc_pctl(hn, pctl_pr, "hn_p"),
        hn_mean = mean(hn),
        mitmatmisc::calc_pctl(tasmin, pctl_tas, "tasmin_p"),
        mitmatmisc::calc_pctl(tasmax, pctl_tas, "tasmax_p")
      ), .(season, icell)]
      
      
      # length(icell_common)
      # length(icell_common)/n_cores
      
      # l_icell_split <- split(icell_common, ceiling(seq_along(icell_common)/10))
      # 
      # dat_i_icell_out3 <- foreach(
      #   # i_split = seq_along(l_icell_split),
      #   i_split = 1:32,
      #   .inorder = F,
      #   .final = rbindlist
      # ) %dopar% {
      #   
      #   dat_i_split <- merge(
      #     dat_i[icell %in% l_icell_split[[i_split]]],
      #     dat_crespi[icell %in% l_icell_split[[i_split]]],
      #     by = c("icell", "date", "season", "elev_fct")
      #   )
      #   
      #   zz <- map(c("pr", "tasmin", "tasmax", "hn"), \(x){
      #     
      #     map(c("ad_test", "cvm_test", "ks_test", "wass_test"), \(y){
      #       dat_i_split[, 
      #                   as.list(f_test(value, value_crespi, nboots = dist_stat_nboots, keep.boots = F)) %>% 
      #                     setNames(c("dist_stat", "pval")) %>% 
      #                     c(dist_test = y, variable = x),
      #                   .(season, icell),
      #                   env = list(f_test = y, value = x, value_crespi = str_c(x, "_crespi"))]
      #     }) %>% rbindlist
      #     
      #   }) %>% rbindlist      
      #   
      #   return(zz)
      #   
      # }
      # 
      
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
      
      dat_i_icell_out5 <- map(c("pr", "tasmin", "tasmax", "hn"), \(x){
        dat_i[, 
              c(f_autocor(.SD, x), variable = x), 
              .(season, date)]
      }) %>% rbindlist
      
      dat_i_icell_out6 <- map(c("pr", "tasmin", "tasmax", "hn"), \(x){
        dat_i[, 
              .(mae = mean(abs(value - value_crespi)),
                bias = mean(value - value_crespi),
                corr = cor(value, value_crespi),
                variable = x),
              .(season, icell),
              env = list(value = x, value_crespi = str_c(x, "_crespi"))]
      }) %>% rbindlist  
      
      saveRDS(dat_i_icell_out1, path(path_out, "icell", "mean-pctl", i_bads, i_rcm_name, ext = "rds"))
      # saveRDS(dat_i_icell_out3, path(path_out, "icell", "dist-stat", i_bads, i_rcm_name, ext = "rds"))
      saveRDS(dat_i_icell_out4, path(path_out, "icell", "etccdi", i_bads, i_rcm_name, ext = "rds"))
      saveRDS(dat_i_icell_out5, path(path_out, "icell", "spatcor", i_bads, i_rcm_name, ext = "rds"))
      saveRDS(dat_i_icell_out6, path(path_out, "icell", "metrics", i_bads, i_rcm_name, ext = "rds"))


      
    } else {
      

# *no pr ------------------------------------------------------------------

      
      
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
      rm(dat_tasmin, dat_tasmax);gc();
      
      # dat_i[, hn := snowfall(pr, tasmax, tasmin)]
      
      dat_i[, season := mitmatmisc::season_fct(month(date))]
      dat_i <- dat_i %>% merge(dat_orog[, .(icell, elev_fct)])
      
      
      dat_i_tnaa <- dat_i[icell %in% icell_common,
                          lapply(.SD, mean),
                          .(date, season),
                          .SDcols = c("tasmax", "tasmin")]
      
      dat_i_elev <- dat_i[icell %in% icell_common,
                          lapply(.SD, mean),
                          .(date, season, elev_fct),
                          .SDcols = c("tasmax", "tasmin")]
      
      dat_i <- merge(dat_i, dat_crespi, by = c("icell", "date", "season", "elev_fct"))
      dat_i_tnaa <- merge(dat_i_tnaa, dat_crespi_tnaa)
      dat_i_elev <- merge(dat_i_elev, dat_crespi_elev)
      
      
      # ** tnaa --------------------------------------------------------------------
      
      dat_i_tnaa_out1 <- dat_i_tnaa[, c(
        # mitmatmisc::calc_pctl(pr, pctl_pr, "pr_p"),
        # pr_mean = mean(pr),
        # mitmatmisc::calc_pctl(hn, pctl_pr, "hn_p"),
        # hn_mean = mean(hn),
        mitmatmisc::calc_pctl(tasmin, pctl_tas, "tasmin_p"),
        mitmatmisc::calc_pctl(tasmax, pctl_tas, "tasmax_p")
      ), .(season)]
      
      dat_i_tnaa_out2 <- map(c("tasmin", "tasmax"), \(x){
        dat_i_tnaa[, 
                   .(qval = quantile(value, pctl_ecdf),
                     pctl = pctl_ecdf,
                     variable = x),
                   .(season),
                   env = list(value = x)]
      }) %>% rbindlist            
      
      
      dat_i_tnaa_out3 <-  map(c("tasmin", "tasmax"), \(x){
        
        map(c("ad_test", "cvm_test", "ks_test", "wass_test"), \(y){
          dat_i_tnaa[, 
                     as.list(f_test(value, value_crespi, nboots = dist_stat_nboots, keep.boots = F)) %>% 
                       setNames(c("dist_stat", "pval")) %>% 
                       c(dist_test = y, variable = x),
                     .(season),
                     env = list(f_test = y, value = x, value_crespi = str_c(x, "_crespi"))]
        }) %>% rbindlist
        
      }) %>% rbindlist      
      
      
      dat_i_tnaa_out6 <- map(c("tasmin", "tasmax"), \(x){
        dat_i_tnaa[, 
                   .(mae = mean(abs(value - value_crespi)),
                     bias = mean(value - value_crespi),
                     corr = cor(value, value_crespi),
                     variable = x),
                   .(season),
                   env = list(value = x, value_crespi = str_c(x, "_crespi"))]
      }) %>% rbindlist   
      
      
      saveRDS(dat_i_tnaa_out1, path(path_out, "tnaa", "mean-pctl", i_bads, i_rcm_name, ext = "rds"))
      saveRDS(dat_i_tnaa_out2, path(path_out, "tnaa", "ecdf", i_bads, i_rcm_name, ext = "rds"))
      saveRDS(dat_i_tnaa_out3, path(path_out, "tnaa", "dist-stat", i_bads, i_rcm_name, ext = "rds"))
      saveRDS(dat_i_tnaa_out6, path(path_out, "tnaa", "metrics", i_bads, i_rcm_name, ext = "rds"))
      
      
      # ** elev --------------------------------------------------------------------
      
      dat_i_elev_out1 <- dat_i_elev[, c(
        # mitmatmisc::calc_pctl(pr, pctl_pr, "pr_p"),
        # pr_mean = mean(pr),
        # mitmatmisc::calc_pctl(hn, pctl_pr, "hn_p"),
        # hn_mean = mean(hn),
        mitmatmisc::calc_pctl(tasmin, pctl_tas, "tasmin_p"),
        mitmatmisc::calc_pctl(tasmax, pctl_tas, "tasmax_p")
      ), .(season, elev_fct)]
      
      dat_i_elev_out2 <- map(c("tasmin", "tasmax"), \(x){
        dat_i_elev[, 
                   .(qval = quantile(value, pctl_ecdf),
                     pctl = pctl_ecdf,
                     variable = x),
                   .(season, elev_fct),
                   env = list(value = x)]
      }) %>% rbindlist            
      
      
      dat_i_elev_out3 <-  map(c("tasmin", "tasmax"), \(x){
        
        map(c("ad_test", "cvm_test", "ks_test", "wass_test"), \(y){
          dat_i_elev[, 
                     as.list(f_test(value, value_crespi, nboots = dist_stat_nboots, keep.boots = F)) %>% 
                       setNames(c("dist_stat", "pval")) %>% 
                       c(dist_test = y, variable = x),
                     .(season, elev_fct),
                     env = list(f_test = y, value = x, value_crespi = str_c(x, "_crespi"))]
        }) %>% rbindlist
        
      }) %>% rbindlist      
      
      
      dat_i_elev_out6 <- map(c("tasmin", "tasmax"), \(x){
        dat_i_elev[, 
                   .(mae = mean(abs(value - value_crespi)),
                     bias = mean(value - value_crespi),
                     corr = cor(value, value_crespi),
                     variable = x),
                   .(season, elev_fct),
                   env = list(value = x, value_crespi = str_c(x, "_crespi"))]
      }) %>% rbindlist   
      
      saveRDS(dat_i_elev_out1, path(path_out, "elev", "mean-pctl", i_bads, i_rcm_name, ext = "rds"))
      saveRDS(dat_i_elev_out2, path(path_out, "elev", "ecdf", i_bads, i_rcm_name, ext = "rds"))
      saveRDS(dat_i_elev_out3, path(path_out, "elev", "dist-stat", i_bads, i_rcm_name, ext = "rds"))
      saveRDS(dat_i_elev_out6, path(path_out, "elev", "metrics", i_bads, i_rcm_name, ext = "rds"))
      
      
      # ** icell ----------------------------------------------------------------
      
      
      dat_i_icell_out1 <- dat_i[, c(
        # mitmatmisc::calc_pctl(pr, pctl_pr, "pr_p"),
        # pr_mean = mean(pr),
        # mitmatmisc::calc_pctl(hn, pctl_pr, "hn_p"),
        # hn_mean = mean(hn),
        mitmatmisc::calc_pctl(tasmin, pctl_tas, "tasmin_p"),
        mitmatmisc::calc_pctl(tasmax, pctl_tas, "tasmax_p")
      ), .(season, icell)]
      
      
      # length(icell_common)
      # length(icell_common)/n_cores
      
      # l_icell_split <- split(icell_common, ceiling(seq_along(icell_common)/10))
      # 
      # dat_i_icell_out3 <- foreach(
      #   # i_split = seq_along(l_icell_split),
      #   i_split = 1:32,
      #   .inorder = F,
      #   .final = rbindlist
      # ) %dopar% {
      #   
      #   dat_i_split <- merge(
      #     dat_i[icell %in% l_icell_split[[i_split]]],
      #     dat_crespi[icell %in% l_icell_split[[i_split]]],
      #     by = c("icell", "date", "season", "elev_fct")
      #   )
      #   
      #   zz <- map(c("pr", "tasmin", "tasmax", "hn"), \(x){
      #     
      #     map(c("ad_test", "cvm_test", "ks_test", "wass_test"), \(y){
      #       dat_i_split[, 
      #                   as.list(f_test(value, value_crespi, nboots = dist_stat_nboots, keep.boots = F)) %>% 
      #                     setNames(c("dist_stat", "pval")) %>% 
      #                     c(dist_test = y, variable = x),
      #                   .(season, icell),
      #                   env = list(f_test = y, value = x, value_crespi = str_c(x, "_crespi"))]
      #     }) %>% rbindlist
      #     
      #   }) %>% rbindlist      
      #   
      #   return(zz)
      #   
      # }
      # 
      
      dat_i_icell_out4 <- map(
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
      
      
      dat_i_icell_out5 <- map(c("tasmin", "tasmax"), \(x){
        dat_i[, 
              c(f_autocor(.SD, x), variable = x), 
              .(season, date)]
      }) %>% rbindlist
      
      dat_i_icell_out6 <- map(c("tasmin", "tasmax"), \(x){
        dat_i[, 
              .(mae = mean(abs(value - value_crespi)),
                bias = mean(value - value_crespi),
                corr = cor(value, value_crespi),
                variable = x),
              .(season, icell),
              env = list(value = x, value_crespi = str_c(x, "_crespi"))]
      }) %>% rbindlist  
      
      
      
      
      saveRDS(dat_i_icell_out1, path(path_out, "icell", "mean-pctl", i_bads, i_rcm_name, ext = "rds"))
      # saveRDS(dat_i_icell_out3, path(path_out, "icell", "dist-stat", i_bads, i_rcm_name, ext = "rds"))
      saveRDS(dat_i_icell_out4, path(path_out, "icell", "etccdi", i_bads, i_rcm_name, ext = "rds"))
      saveRDS(dat_i_icell_out5, path(path_out, "icell", "spatcor", i_bads, i_rcm_name, ext = "rds"))
      saveRDS(dat_i_icell_out6, path(path_out, "icell", "metrics", i_bads, i_rcm_name, ext = "rds"))
      
      
      
    }
    
    
    
  }
  
  
}




