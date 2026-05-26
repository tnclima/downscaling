# calc PCAs Crespi

library(eurocordexr)
library(data.table)
setDTthreads(4)
library(magrittr)
library(ggplot2)
library(fs)
library(foreach)

summary_prcomp <- function(xx_pca, k = 10){
  sdev <- xx_pca$sdev^2
  sdev <- sdev / sum(sdev)
  data.table(pc = paste0("PC", 1:k),
             prop_sd = sdev[1:k],
             cumsum_prop_sd = cumsum(sdev[1:k]))
}

# settings ----------------------------------------------------------------

path_out <- "/home/climatedata/downscaling/pca-ked/crespi-pca/testing-wet-dry/"
path_out_varexp <- "/home/climatedata/downscaling/pca-ked/crespi-pca-varexp/testing-wet-dry/"

dir_create(path_out)
dir_create(path_out_varexp)


n_pc <- 20
# n_sub_dates <- 300 # 0 for no subsetting # not needed, since subset to wet days
center_pca <- T
scale_pca <- F

seasons <- list("DJF" = c(12,1,2),
                "MAM" = 3:5,
                "JJA" = 6:8,
                "SON" = 9:11)

l_file_obs <- list(
  # tasmax = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MaxTemp.nc",
  # tasmin = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_MinTemp.nc",
  pr = "/home/climatedata/obs/CRESPI/daily_1km_lonlat/DailySeries_1980_2020_Prec.nc"
)




i_var <- "pr"  

i_varname <- get_varnames(l_file_obs[[i_var]])

dat_obs <- nc_grid_to_dt(l_file_obs[[i_var]])
setnames(dat_obs, i_varname, "value")
dat_obs <- dat_obs[!is.na(value)]
dat_obs[, month := month(date)]



# no dry days --------------------------------------------------------------------

l_out <- list()
l_out_varexp <- list()

for(i_seas in names(seasons)){
  
  dat_obs2 <- dat_obs[month %in% seasons[[i_seas]]]
  
  mat_obs <- dat_obs2 %>% 
    dcast(icell ~ date, value.var = "value") %>% 
    as.matrix
  
  vec_icell <- mat_obs[, 1]
  mat_obs <- mat_obs[, -1]
  
  area_sum <- apply(mat_obs, 2, sum)
  area_sum2 <- area_sum/length(vec_icell)
  
  sub_dates <- which(area_sum2 > 1)
  mat_obs <- mat_obs[, sub_dates]
  
  if(i_var == "pr"){
    mat_obs[mat_obs < 0] <- 0
    
    # remove all 0 columns
    zero_col <- apply(mat_obs, 2, \(x) all(x == 0))
    mat_obs <- mat_obs[, !zero_col]
    
    # normalize
    mat_obs <- sqrt(mat_obs)
  }
  
  pca2 <- prcomp(mat_obs, center = center_pca, scale = scale_pca, rank. = 20)
  
  dat_pca <- data.table(variable = i_var, season = i_seas, icell = vec_icell, pca2$x)
  l_out[[i_seas]] <- dat_pca
  
  dat_pca_varexp <- summary_prcomp(pca2, n_pc)
  l_out_varexp[[i_seas]] <- dat_pca_varexp
  
  
}

dat_out <- rbindlist(l_out)
file_out <- path(path_out, 
                 paste0("nodry-", i_var, 
                        "-center", center_pca, 
                        "-scale", scale_pca), 
                 ext = "rds")
saveRDS(dat_out, file_out)

dat_out_varexp <- rbindlist(l_out_varexp, idcol = "season")
file_out_varexp <- path(path_out_varexp, 
                        paste0("nodry-", i_var, 
                               "-center", center_pca, 
                               "-scale", scale_pca), 
                        ext = "rds")
saveRDS(dat_out_varexp, file_out_varexp)








# all pixels wet?  --------------------------------------------------------------------

l_out <- list()
l_out_varexp <- list()

for(i_seas in names(seasons)){
  
  dat_obs2 <- dat_obs[month %in% seasons[[i_seas]]]
  
  mat_obs <- dat_obs2 %>% 
    dcast(icell ~ date, value.var = "value") %>% 
    as.matrix
  
  vec_icell <- mat_obs[, 1]
  mat_obs <- mat_obs[, -1]
  
  area_wet <- apply(mat_obs, 2, \(x) all(x > 0.1))
  
  sub_dates <- which(area_wet)
  mat_obs <- mat_obs[, sub_dates]
  
  if(i_var == "pr"){
    mat_obs[mat_obs < 0] <- 0
    
    # remove all 0 columns
    zero_col <- apply(mat_obs, 2, \(x) all(x == 0))
    mat_obs <- mat_obs[, !zero_col]
    
    # normalize
    mat_obs <- sqrt(mat_obs)
  }
  
  pca2 <- prcomp(mat_obs, center = center_pca, scale = scale_pca, rank. = 20)
  
  dat_pca <- data.table(variable = i_var, season = i_seas, icell = vec_icell, pca2$x)
  l_out[[i_seas]] <- dat_pca
  
  dat_pca_varexp <- summary_prcomp(pca2, n_pc)
  l_out_varexp[[i_seas]] <- dat_pca_varexp
  
  
}

dat_out <- rbindlist(l_out)
file_out <- path(path_out, 
                 paste0("allwet-", i_var, 
                        "-center", center_pca, 
                        "-scale", scale_pca), 
                 ext = "rds")
saveRDS(dat_out, file_out)

dat_out_varexp <- rbindlist(l_out_varexp, idcol = "season")
file_out_varexp <- path(path_out_varexp, 
                        paste0("allwet-", i_var, 
                               "-center", center_pca, 
                               "-scale", scale_pca), 
                        ext = "rds")
saveRDS(dat_out_varexp, file_out_varexp)


