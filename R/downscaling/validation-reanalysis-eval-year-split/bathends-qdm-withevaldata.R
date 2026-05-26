# 

library(terra)
library(magrittr)
library(lubridate)
library(data.table)
setDTthreads(12)
library(fs)
library(foreach)
library(MBC)
library(stringr)
library(purrr)
library(eurocordexr)

source("R/functions/create_empty_netcdf.R")
source("R/functions/get_nc_1d.R")
source("R/functions/inv_sub_reanalysis.R")
source("R/functions/snowfall.R")

# extended_l_years_train_period <- readRDS("data/random-years-reanalysis2-extended.rds")
extended_l_years_train_period <- readRDS("data/random-years-reanalysis-extended.rds")

path_tmp_nc <- "/home/climatedata/downscaling/validation-cv-reanalysis/zz-tmp-qdm/"
# if(dir_exists(path_tmp_nc)) stop("tmp directory exists!")
dir_create(path_tmp_nc)

path_in_baqdm <- "/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v6/ba-qdm/"


# settings ba ds ----------------------------------------------------

date_rcm_sub <- as.Date(c("1989-01-02", "2008-12-31"))

# l_wet_day <- list(tasmax = F, tasmin = F, pr = 0.05) # QM: 0.05 for consistency with QDM()
l_ratio <- list(tasmax = F, tasmin = F, pr = T) # QDM: ratio in QDM()

temp_mv <- F # temporal moving window +-1 month? 
n_nc_sync <- 200 # intermediate save to nc_out file every n cells
n_cores <- 1 # parallel computation; bottleneck maybe disk access; (reads RCM memory, crespi cell-by-cell)

upscaled_crespi <- T # needed info, because different NA cells at boundaries

l_file_obs <- list(
  tasmax = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MaxTemp.nc",
  tasmin = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MinTemp.nc",
  pr = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_Prec.nc"
)

l_file_obs_orog <- list(
  tasmax = "/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc",
  tasmin = "/home/climatedata/obs/orography/crespi_lonlat_1km_temperature.nc",
  pr = "/home/climatedata/obs/orography/crespi_lonlat_1km_precipitation.nc"
)

# inventory ---------------------------------------------------------------

dat_inv <- inv_sub_reanalysis()
dat_inv_loop <- dat_inv[variable %in% c("tasmin", "tasmax", "pr")]
dat_inv_loop_mod <- dat_inv_loop[, .(gcm, institute_rcm, experiment, 
                                     ensemble, downscale_realisation)] %>% unique()

# ** settings eval ** -----------------------------------------------------


# crespi data for eval ----------------------------------------------------



date_sub <- as.Date(c("1989-01-02", "2008-12-31"))  

pctl_pr <- c(0.95, 0.99, 1)
pctl_tas <- c(0, 0.01, 0.05, 0.5, 0.95, 0.99, 1)
pctl_ecdf <- seq(0, 1, by=0.01)

elev_breaks <- seq(0, 3500, by = 500)


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





# common icell upscaled crespi

dat1 <- nc_grid_to_dt("/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v2/ba-qdm-ds-qdm/pr_CLMcom-CCLM4-8-17_ECMWF-ERAINT_evaluation.nc",
                      date_range = c("2000-01-01", "2000-01-01"))
dat2 <- nc_grid_to_dt("/home/climatedata/downscaling/validation-cv-reanalysis/data-daily-v1/bads-qdm/pr_CLMcom-CCLM4-8-17_ECMWF-ERAINT_evaluation.nc",
                      date_range = c("2000-01-01", "2000-01-01"))
icell_common <- intersect(dat1[!is.na(pr), icell], dat2[!is.na(pr), icell])


# crespi data 

# defined above
# l_file_obs <- list(
#   tasmax = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MaxTemp.nc",
#   tasmin = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MinTemp.nc",
#   pr = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_Prec.nc"
# )

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



# ** start loop extended years ** ---------------------------------------------------------------

mitmatmisc::init_parallel_ubuntu(n_cores)



for(i_ext in 1:length(extended_l_years_train_period)){
  
  l_years_train_period <- extended_l_years_train_period[[i_ext]]
  
  
  files_ba <- dir_ls(path(path_in_baqdm, i_ext))
  
  zz <- foreach(
    i_file_ba = files_ba,
    .inorder = F
  ) %dopar% {
    
    i_file_ba_split <- i_file_ba %>% 
      path_file() %>% 
      str_split_1("_")
    
    i_rcm_name <- i_file_ba_split[2]
    i_var <- i_file_ba_split[1]
    file_rcm <- i_file_ba
    
    file_rcm_orog <- dat_inv[variable == "orog" & institute_rcm == i_rcm_name, list_files[[1]]]
    file_obs_orog <- l_file_obs_orog[[i_var]]
    
    file_out <- path(path_tmp_nc,
                     i_ext,
                     path_file(i_file_ba))
    
    
    if(file_exists(file_out)) return(NULL)
    
    
    
    
    
    # data --------------------------------------------------------------------
    
    rs_rcm_orog <- rast(file_rcm_orog)
    rs_obs_orog <- rast(file_obs_orog)
    
    if(!upscaled_crespi){
      cells_obs <- which(!is.na(rs_obs_orog[]))
      cells_obs_na <- which(is.na(rs_obs_orog[]))
    }
    
    rs_rcm <- rast(file_rcm)
    mat_rcm <- values(rs_rcm, mat = T)
    rs_obs <- rast(l_file_obs[[i_var]])
    
    dt_rcm_orog <- as.data.table(rs_rcm_orog, xy = T, na.rm = F)
    dt_obs_orog <- as.data.table(rs_obs_orog, xy = T, na.rm = F)
    
    # time stuff --------------------------------------------------------------
    
    # non-standard cal workaround
    nc_rcm <- nc_open(file_rcm)
    raw_times <- ncdf4.helpers::nc.get.time.series(nc_rcm)
    mapped_times <- eurocordexr::map_non_standard_calendar(raw_times)
    dates_rcm <- mapped_times$dates_full
    nc_close(nc_rcm)
    months_rcm <- month(dates_rcm)
    years_rcm <- year(dates_rcm)
    
    dates_obs <- time(rs_obs)
    months_obs <- month(dates_obs)
    years_obs <- year(dates_obs)
    
    # create xy lookup rasters crespi-rcm ---------------------------------------------------
    
    rs_rcm_cells <- rs_rcm_orog
    rs_rcm_cells[] <- 1:ncell(rs_rcm_cells)
    
    rs_cells_rcm_obs <- project(rs_rcm_cells, rs_obs_orog, method = "near")
    # rs_cells_rcm_obs[is.na(rs_obs_orog)] <- NA # mask outside TNAA?
    names(rs_cells_rcm_obs) <- "rcm_cell"
    
    
    # update cells na (upscaled crespi) ---------------------------------------------------
    
    if(upscaled_crespi){
      
      # rs_zz <- unwrap(wrap())
      icell_na_upscale <- which(is.na(rs_rcm[[1]][]))
      # which(rs_cells_rcm_obs[] %in% icell_na_upscale)
      
      cells_obs <- intersect(which(!rs_cells_rcm_obs[] %in% icell_na_upscale),
                             which(!is.na(rs_obs_orog[])))
      cells_obs_na <- setdiff(1:ncell(rs_obs_orog), cells_obs)
    }
    
    
    # outfile ---------------------------------------------------------
    
    dir_create(path_dir(file_out))
    create_emtpy_netcdf(file_template = file_obs_orog,
                        file_out = file_out,
                        l_varinfo = l_nc_info[[i_var]],
                        date_period = date_rcm_sub,
                        overwrite = F)
    
    
    # main proc ---------------------------------------------------------------
    
    nc_out <- nc_open(file_out, write = T)
    i_nc_sync <- 0
    
    for(i_cell in cells_obs){
      
      i_nc_sync <- i_nc_sync + 1
      
      i_row <- rowFromCell(rs_obs, i_cell)
      i_col <- colFromCell(rs_obs, i_cell)
      
      vals_obs <- get_nc_1d(l_file_obs[[i_var]], i_row, i_col)
      elev_obs <- as.vector(rs_obs_orog)[i_cell]
      
      dat_obs <- data.table(date = dates_obs,
                            year = years_obs,
                            # year0 = years_obs - min(years_obs),
                            month = months_obs,
                            month_fct = mitmatmisc::month_fct(months_obs),
                            value = vals_obs)
      dat_obs <- dat_obs[date >= date_rcm_sub[1] & date <= date_rcm_sub[2]]
      
      i_cell_rcm <- as.vector(rs_cells_rcm_obs)[i_cell]
      vals_rcm <- mat_rcm[i_cell_rcm, ]
      
      
      # if(i_var == "pr"){
      #   vals_rcm <- vals_rcm*24*3600
      # } else {
      #   vals_rcm <- vals_rcm-273.15
      # }
      
      # non-standard cal adjustment
      vals_rcm <- vals_rcm[mapped_times$idx_pcict]
      
      dat_rcm <- data.table(date = dates_rcm,
                            year = years_rcm,
                            decade = ceiling(years_rcm/10),
                            # year0 = years_rcm - min(years_rcm),
                            month = months_rcm,
                            # month_fct = mitmatmisc::month_fct(months_rcm),
                            value = vals_rcm,
                            value_ba = NA_real_) 
      dat_rcm <- dat_rcm[date >= date_rcm_sub[1] & date <= date_rcm_sub[2]]
      
      for(years_train_period in l_years_train_period){
        
        dat_obs_hist <- dat_obs[year %in% years_train_period]
        dat_rcm_hist <- dat_rcm[year %in% years_train_period] 
        
        # moving window correction
        for(i_month in 1:12){
          if(temp_mv){
            i_month_window <- c(12,1:12,1)[1 + i_month+c(-1:1)]
          } else {
            i_month_window <- i_month 
          }
          
          vals_train_obs <- dat_obs_hist[month %in% i_month_window][["value"]]
          vals_train_rcm <- dat_rcm_hist[month %in% i_month_window][["value"]]
          
          # for(i_decade in all_decades){
          # i_decade_window <- i_decade + c(-1:1)
          dat_rcm_fut_window <- dat_rcm[month %in% i_month_window & 
                                          # decade %in% i_decade_window &
                                          ! year %in% years_train_period] 
          
          vals_future_rcm <- dat_rcm_fut_window[["value"]]
          l_qdm <- QDM(vals_train_obs, vals_train_rcm, vals_future_rcm,
                       ratio = l_ratio[[i_var]])
          dat_rcm_fut_window[, value_ba := l_qdm$mhat.p]
          
          # update only month and decade in the middle (not moving)
          dat_rcm[month == i_month & ! year %in% years_train_period, 
                  value_ba := dat_rcm_fut_window[month == i_month, value_ba]]
          
          # }
          
        }
      }
      
      
      vals_out <- dat_rcm$value_ba
      ncvar_put(nc_out, varid = i_var, vals = vals_out, 
                start = c(i_col, i_row, 1), count = c(1, 1, -1))
      
      if(i_nc_sync %% n_nc_sync == 0) nc_sync(nc_out)
      
      # cat(sprintf("%s - cell#(x,y): %i (%i,%i)", date(), i_cell, i_col, i_row), "\n")
      
    }
    
    nc_sync(nc_out)
    
    # fill with NA rest
    i_nc_sync <- 0
    for(i_cell in cells_obs_na){
      i_nc_sync <- i_nc_sync + 1
      
      i_row <- rowFromCell(rs_obs, i_cell)
      i_col <- colFromCell(rs_obs, i_cell)
      
      ncvar_put(nc_out, varid = i_var, vals = rep(NA_real_, nc_out$dim$time$len), 
                start = c(i_col, i_row, 1), count = c(1, 1, -1))
      
      if(i_nc_sync %% n_nc_sync == 0) nc_sync(nc_out)
      
      # cat(sprintf("%s - NA cell#(x,y): %i (%i,%i)", date(), i_cell, i_col, i_row), "\n")
    }
    
    
    
    nc_close(nc_out)
    
  }
  
  
  
  
  
  
  
  
  # ** start eval ** ----------------------------------------------------------------
  
  i_path <- path(path_tmp_nc, i_ext)
  i_bads <- "ba-qdm-ds-qdm"
  
  path_out <- "/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v6/"
  dir_create(path(path_out, "tnaa", c("mean-pctl", "ecdf", "dist-stat", "metrics", "spatcor")))
  dir_create(path(path_out, "elev", c("mean-pctl", "ecdf", "dist-stat", "metrics")))
  dir_create(path(path_out, "icell", c("mean-pctl", "dist-stat", "etccdi", "metrics")))
  
  
  
  
  
  # main loop ---------------------------------------------------------------
  
  files_bads <- dir_ls(i_path)
  
  dir_create(path(path_out, "tnaa", c("mean-pctl", "ecdf", "dist-stat", "metrics", "spatcor"), i_bads, i_ext))
  dir_create(path(path_out, "elev", c("mean-pctl", "ecdf", "dist-stat", "metrics"), i_bads, i_ext))
  dir_create(path(path_out, "icell", c("mean-pctl", "dist-stat", "etccdi", "metrics"), i_bads, i_ext))
  
  n_loop <- nrow(dat_inv_loop_mod)
  n_shift <- -1
  
  foreach(i = 1:n_loop) %do% {
    
    i_rcm_name <-  dat_inv_loop_mod[i, institute_rcm]
    files_read <- str_subset(files_bads, fixed(i_rcm_name))
    
    # if last file exists: skip rest of loop (save reading in a lot of data)
    if(file_exists(path(path_out, "icell", "metrics", i_bads, i_ext, i_rcm_name, ext = "rds"))) return(NULL)
    
    
    lgl_pr <- any(str_detect(files_read, fixed("pr")))
    
    if(lgl_pr){
      dat_pr <- nc_grid_to_dt(str_subset(files_read, "pr"))
      setnames(dat_pr, 3, "pr")
      dat_pr <- dat_pr[!is.na(pr)]
    }      
    
    dat_tasmin <- nc_grid_to_dt(str_subset(files_read, "tasmin"))
    setnames(dat_tasmin, 3, "tasmin")
    dat_tasmin <- dat_tasmin[!is.na(tasmin)]
    
    dat_tasmax <- nc_grid_to_dt(str_subset(files_read, "tasmax"))
    setnames(dat_tasmax, 3, "tasmax")
    dat_tasmax <- dat_tasmax[!is.na(tasmax)]
    
    if(lgl_pr){
      dat_i <- cbind(dat_pr, tasmax = dat_tasmax$tasmax, tasmin = dat_tasmin$tasmin)
      rm(dat_pr, dat_tasmin, dat_tasmax);gc();
      
      dat_i[, hn := snowfall(pr, tasmax, tasmin)]
      
      map_vars <- c("pr", "hn", "tasmax", "tasmin")
    } else {
      dat_i <- cbind(dat_tasmax, tasmin = dat_tasmin$tasmin)
      rm(dat_tasmin, dat_tasmax);gc();
      
      map_vars <- c("tasmax", "tasmin")
    }
    
    dat_i[, season := mitmatmisc::season_fct(month(date))]
    dat_i <- dat_i %>% merge(dat_orog[, .(icell, elev_fct)])
    
    
    dat_i_tnaa <- dat_i[icell %in% icell_common,
                        lapply(.SD, mean),
                        .(date, season),
                        .SDcols = map_vars]
    
    dat_i_elev <- dat_i[icell %in% icell_common,
                        lapply(.SD, mean),
                        .(date, season, elev_fct),
                        .SDcols = map_vars]
    
    dat_i <- merge(dat_i, dat_crespi, by = c("icell", "date", "season", "elev_fct"))
    dat_i_tnaa <- merge(dat_i_tnaa, dat_crespi_tnaa)
    dat_i_elev <- merge(dat_i_elev, dat_crespi_elev)
    
    if(lgl_pr){
      dat_i_tnaa[, pr_crespi := data.table::shift(pr_crespi, n_shift)]
      dat_i_tnaa[, hn_crespi := data.table::shift(hn_crespi, n_shift)]
      dat_i_tnaa <- dat_i_tnaa[!is.na(pr_crespi)]
      
      dat_i_elev[, pr_crespi := data.table::shift(pr_crespi, n_shift), .(elev_fct)]
      dat_i_elev[, hn_crespi := data.table::shift(hn_crespi, n_shift), .(elev_fct)]
      dat_i_elev <- dat_i_elev[!is.na(pr_crespi)]
      
      dat_i[, pr_crespi := data.table::shift(pr_crespi, n_shift), .(icell)]
      dat_i[, hn_crespi := data.table::shift(hn_crespi, n_shift), .(icell)]
      dat_i <- dat_i[!is.na(pr_crespi)]
      
    }      
    
    # ** tnaa --------------------------------------------------------------------
    
    if(!file_exists(path(path_out, "tnaa", "mean-pctl", i_bads, i_ext, i_rcm_name, ext = "rds"))){
      
      if(lgl_pr){
        dat_i_tnaa_out1 <- dat_i_tnaa[, c(
          mitmatmisc::calc_pctl(pr, pctl_pr, "pr_p"),
          pr_mean = mean(pr),
          mitmatmisc::calc_pctl(hn, pctl_pr, "hn_p"),
          hn_mean = mean(hn),
          mitmatmisc::calc_pctl(tasmin, pctl_tas, "tasmin_p"),
          mitmatmisc::calc_pctl(tasmax, pctl_tas, "tasmax_p")
        ), .(season)]
      } else {
        dat_i_tnaa_out1 <- dat_i_tnaa[, c(
          # mitmatmisc::calc_pctl(pr, pctl_pr, "pr_p"),
          # pr_mean = mean(pr),
          # mitmatmisc::calc_pctl(hn, pctl_pr, "hn_p"),
          # hn_mean = mean(hn),
          mitmatmisc::calc_pctl(tasmin, pctl_tas, "tasmin_p"),
          mitmatmisc::calc_pctl(tasmax, pctl_tas, "tasmax_p")
        ), .(season)]
      }
      
      saveRDS(dat_i_tnaa_out1, path(path_out, "tnaa", "mean-pctl", i_bads, i_ext, i_rcm_name, ext = "rds"))
    }
    
    
    if(!file_exists(path(path_out, "tnaa", "ecdf", i_bads, i_ext, i_rcm_name, ext = "rds"))){
      
      dat_i_tnaa_out2 <- map(map_vars, \(x){
        dat_i_tnaa[, 
                   .(qval = quantile(value, pctl_ecdf),
                     pctl = pctl_ecdf,
                     variable = x),
                   .(season),
                   env = list(value = x)]
      }) %>% rbindlist  
      
      saveRDS(dat_i_tnaa_out2, path(path_out, "tnaa", "ecdf", i_bads, i_ext, i_rcm_name, ext = "rds"))
    }          
    # 
    # if(!file_exists(path(path_out, "tnaa", "dist-stat", i_bads, i_ext, i_rcm_name, ext = "rds"))){
    #   dat_i_tnaa_out3 <-  map(map_vars, \(x){
    #     map(c("ad_test", "cvm_test", "ks_test", "wass_test"), \(y){
    #       dat_i_tnaa[, 
    #                  as.list(f_test(value, value_crespi, nboots = dist_stat_nboots, keep.boots = F)) %>% 
    #                    setNames(c("dist_stat", "pval")) %>% 
    #                    c(dist_test = y, variable = x),
    #                  .(season),
    #                  env = list(f_test = y, value = x, value_crespi = str_c(x, "_crespi"))]
    #     }) %>% rbindlist
    #   }) %>% rbindlist   
    #   saveRDS(dat_i_tnaa_out3, path(path_out, "tnaa", "dist-stat", i_bads, i_rcm_name, ext = "rds"))
    # }
    
    
    if(!file_exists(path(path_out, "tnaa", "metrics", i_bads, i_ext, i_rcm_name, ext = "rds"))){
      
      dat_i_tnaa_out6 <- map(map_vars, \(x){
        dat_i_tnaa[, 
                   .(mae = mean(abs(value - value_crespi)),
                     bias = mean(value - value_crespi),
                     bias_rel = mean(value - value_crespi)/mean(value_crespi),
                     corr = cor(value, value_crespi),
                     variable = x),
                   .(season),
                   env = list(value = x, value_crespi = str_c(x, "_crespi"))]
      }) %>% rbindlist  
      
      saveRDS(dat_i_tnaa_out6, path(path_out, "tnaa", "metrics", i_bads, i_ext, i_rcm_name, ext = "rds"))
    } 
    
    
    if(!file_exists(path(path_out, "tnaa", "spatcor", i_bads, i_ext, i_rcm_name, ext = "rds"))){
      
      dat_i_tnaa_out7 <- map(map_vars, \(x){
        dat_i[, 
              .(spatcor = suppressWarnings(cor(value, value_crespi)),
                spatcor_nonzero = suppressWarnings(
                  cor(value[value > 0 & value_crespi > 0], 
                      value_crespi[value > 0 & value_crespi > 0])
                ),
                variable = x),
              .(season, date),
              env = list(value = x, value_crespi = str_c(x, "_crespi"))] %>% 
          .[,
            .(spatcor = mean(spatcor, na.rm = T),
              spatcor_nonzero = mean(spatcor_nonzero, na.rm = T)),
            .(season, variable)]
      }) %>% rbindlist   
      
      saveRDS(dat_i_tnaa_out7, path(path_out, "tnaa", "spatcor", i_bads, i_ext, i_rcm_name, ext = "rds"))
    } 
    
    
    # ** elev --------------------------------------------------------------------
    
    if(!file_exists(path(path_out, "elev", "mean-pctl", i_bads, i_ext, i_rcm_name, ext = "rds"))){
      
      if(lgl_pr){
        dat_i_elev_out1 <- dat_i_elev[, c(
          mitmatmisc::calc_pctl(pr, pctl_pr, "pr_p"),
          pr_mean = mean(pr),
          mitmatmisc::calc_pctl(hn, pctl_pr, "hn_p"),
          hn_mean = mean(hn),
          mitmatmisc::calc_pctl(tasmin, pctl_tas, "tasmin_p"),
          mitmatmisc::calc_pctl(tasmax, pctl_tas, "tasmax_p")
        ), .(season, elev_fct)]
      } else {
        dat_i_elev_out1 <- dat_i_elev[, c(
          # mitmatmisc::calc_pctl(pr, pctl_pr, "pr_p"),
          # pr_mean = mean(pr),
          # mitmatmisc::calc_pctl(hn, pctl_pr, "hn_p"),
          # hn_mean = mean(hn),
          mitmatmisc::calc_pctl(tasmin, pctl_tas, "tasmin_p"),
          mitmatmisc::calc_pctl(tasmax, pctl_tas, "tasmax_p")
        ), .(season, elev_fct)]
      }
      
      saveRDS(dat_i_elev_out1, path(path_out, "elev", "mean-pctl", i_bads, i_ext, i_rcm_name, ext = "rds"))
    }
    
    if(!file_exists(path(path_out, "elev", "ecdf", i_bads, i_ext, i_rcm_name, ext = "rds"))){
      dat_i_elev_out2 <- map(map_vars, \(x){
        dat_i_elev[, 
                   .(qval = quantile(value, pctl_ecdf),
                     pctl = pctl_ecdf,
                     variable = x),
                   .(season, elev_fct),
                   env = list(value = x)]
      }) %>% rbindlist    
      
      saveRDS(dat_i_elev_out2, path(path_out, "elev", "ecdf", i_bads, i_ext, i_rcm_name, ext = "rds"))
    }
    # 
    # if(!file_exists(path(path_out, "elev", "dist-stat", i_bads, i_rcm_name, ext = "rds"))){
    #   dat_i_elev_out3 <-  map(map_vars, \(x){
    #     map(c("ad_test", "cvm_test", "ks_test", "wass_test"), \(y){
    #       dat_i_elev[, 
    #                  as.list(f_test(value, value_crespi, nboots = dist_stat_nboots, keep.boots = F)) %>% 
    #                    setNames(c("dist_stat", "pval")) %>% 
    #                    c(dist_test = y, variable = x),
    #                  .(season, elev_fct),
    #                  env = list(f_test = y, value = x, value_crespi = str_c(x, "_crespi"))]
    #     }) %>% rbindlist
    #   }) %>% rbindlist  
    #   
    #   saveRDS(dat_i_elev_out3, path(path_out, "elev", "dist-stat", i_bads, i_rcm_name, ext = "rds"))
    # }
    # 
    if(!file_exists(path(path_out, "elev", "metrics", i_bads, i_ext, i_rcm_name, ext = "rds"))){
      
      dat_i_elev_out6 <- map(map_vars, \(x){
        dat_i_elev[, 
                   .(mae = mean(abs(value - value_crespi)),
                     bias = mean(value - value_crespi),
                     bias_rel = mean(value - value_crespi)/mean(value_crespi),
                     corr = cor(value, value_crespi),
                     variable = x),
                   .(season, elev_fct),
                   env = list(value = x, value_crespi = str_c(x, "_crespi"))]
      }) %>% rbindlist  
      
      saveRDS(dat_i_elev_out6, path(path_out, "elev", "metrics", i_bads, i_ext, i_rcm_name, ext = "rds"))
    }
    
    
    
    # ** icell ----------------------------------------------------------------
    
    if(!file_exists(path(path_out, "icell", "mean-pctl", i_bads, i_ext, i_rcm_name, ext = "rds"))){
      
      if(lgl_pr){
        dat_i_icell_out1 <- dat_i[, c(
          mitmatmisc::calc_pctl(pr, pctl_pr, "pr_p"),
          pr_mean = mean(pr),
          mitmatmisc::calc_pctl(hn, pctl_pr, "hn_p"),
          hn_mean = mean(hn),
          mitmatmisc::calc_pctl(tasmin, pctl_tas, "tasmin_p"),
          mitmatmisc::calc_pctl(tasmax, pctl_tas, "tasmax_p")
        ), .(season, icell)]
      } else {
        dat_i_icell_out1 <- dat_i[, c(
          # mitmatmisc::calc_pctl(pr, pctl_pr, "pr_p"),
          # pr_mean = mean(pr),
          # mitmatmisc::calc_pctl(hn, pctl_pr, "hn_p"),
          # hn_mean = mean(hn),
          mitmatmisc::calc_pctl(tasmin, pctl_tas, "tasmin_p"),
          mitmatmisc::calc_pctl(tasmax, pctl_tas, "tasmax_p")
        ), .(season, icell)]
      }
      
      saveRDS(dat_i_icell_out1, path(path_out, "icell", "mean-pctl", i_bads, i_ext, i_rcm_name, ext = "rds"))
    }
    
    
    if(!file_exists(path(path_out, "icell", "metrics", i_bads, i_ext, i_rcm_name, ext = "rds"))){
      
      dat_i_icell_out6 <- map(map_vars, \(x){
        dat_i[, 
              .(mae = mean(abs(value - value_crespi)),
                bias = mean(value - value_crespi),
                bias_rel = mean(value - value_crespi)/mean(value_crespi),
                corr = cor(value, value_crespi),
                variable = x),
              .(season, icell),
              env = list(value = x, value_crespi = str_c(x, "_crespi"))]
      }) %>% rbindlist  
      
      saveRDS(dat_i_icell_out6, path(path_out, "icell", "metrics", i_bads, i_ext, i_rcm_name, ext = "rds"))
    }
    
    
  }
  
  
  
  
  # end i_ext ---------------------------------------------------------------
  
  
  cat(format(Sys.time()), "- done ", i_ext, "\n")
  # dir_delete(path_tmp_nc)
  
  
  
  
  
  
}
