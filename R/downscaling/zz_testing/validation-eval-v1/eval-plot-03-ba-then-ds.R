#

# library(eurocordexr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(magrittr)
library(ggplot2)
library(fs)
library(stringr)
library(purrr)
library(foreach)
library(patchwork)


path_in <- "/home/climatedata/downscaling/validation-cv/rdata-summary/zz_eval-ba-then-ds/"


dat_clim_crespi <- readRDS("/home/climatedata/downscaling/validation-cv/rdata-summary/eval-04-trends-crespi.rds")
dat_clim_rcm <- readRDS("/home/climatedata/downscaling/validation-cv/rdata-summary/eval-05-trends-rcm.rds")
dat_clim_eobs <- readRDS("/home/climatedata/downscaling/validation-cv/rdata-summary/eval-06-trends-eobs.rds")

dat_icell <- readRDS("/home/climatedata/downscaling/validation-cv/rdata-summary/eval-07-aux-icell-1km-011deg.rds")
dat_aux <- dat_icell[!is.na(orog), .(icell = icell_1km, x, y, orog)]
# dat_aux <- dat_aux[!is.na(orog), .(icell, lon = longitude, lat = latitude, orog)]
# ggplot(dat_aux, aes(x, y, fill = orog))+geom_raster()

# ds files inventory ----------------------------------------------------------------

dat_files <- data.table(filename = dir_ls(path_in))
dat_files[, c("period20", "ba", "ds", "centers") := tstrsplit(path_file(filename) %>% path_ext_remove(), "_")]
dat_files


# dat 1 mod -------------------------------------------------------------------

dat_files_1mod <- dat_files[centers == 1]

dat_1mod <- foreach(
  i = 1:nrow(dat_files_1mod),
  .final = rbindlist
) %do% {
  dat_i <- readRDS(dat_files_1mod[i, filename])
  dat_i[, .(icell, month, variable, mean_value, mean_value_ref, wassersteindist, 
            centers, ba, ds, period20)]
}



# trends ------------------------------------------------------------------

dat1 <- dat_1mod[month == 7 & variable == "tasmax" & ds == "lrfix" & ba == "qm" & period20 == "2001-2020"]


dat1 %>% 
  merge(dat_aux) %>% 
  ggplot(aes(x, y, fill = mean_value - mean_value_ref))+
  geom_raster()+
  scale_fill_gradient2()

