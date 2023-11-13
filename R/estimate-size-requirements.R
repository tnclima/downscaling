# size requirements 
library(fs)
library(magrittr)


fn_tn_1km <- "/home/climatedata/downscaling/size_requirements/crespi_TN_1km_1year_maxtemp.nc"
fn_tnaa_1km <- "/home/climatedata/downscaling/size_requirements/crespi_TNAA_1km_1year_maxtemp.nc"

# TN vs TNAA
frac_tn_tnaa <- as.numeric(file_size(fn_tn_1km)) / as.numeric(file_size(fn_tnaa_1km))

# 1 year crespi 1km dataset TN
file_size(fn_tn_1km)
130*file_size(fn_tn_1km)

# 130 years test qm TNAA
file_size("/home/climatedata/downscaling/zz_temp/test-qm-tasmax.nc")
frac_tn_tnaa*file_size("/home/climatedata/downscaling/zz_temp/test-qm-tasmax.nc")


# compression factors
# temp
as.numeric(file_size("/home/climatedata/downscaling/size_requirements/crespi_TN_1km_1year_maxtemp_compressed.nc")) /
  as.numeric(file_size("/home/climatedata/downscaling/size_requirements/crespi_TN_1km_1year_maxtemp.nc"))
# precip
as.numeric(file_size("/home/climatedata/downscaling/size_requirements/crespi_TN_1km_1year_prec_compressed.nc")) / 
  as.numeric(file_size("/home/climatedata/downscaling/size_requirements/crespi_TN_1km_1year_prec.nc"))


# 130 years
130*file_size(fn_tn_1km)
# 130 years * 4 variables
4*130*file_size(fn_tn_1km)
# 130 years * 4 variables * 17 models
17*4*130*file_size(fn_tn_1km)
# 130 years * 4 variables * 90 models
90*4*130*file_size(fn_tn_1km)


# raw 0.11
dat_inv <- eurocordexr::get_inventory("/home/climatedata/downscaling/rcm_lonlat_tnaa/")
size_full <- dat_inv$list_files %>% unlist %>% file_size() %>% sum
dat_inv[!{gcm == "MPI-M-MPI-ESM-LR" & ensemble != "r1i1p1"} & 
          !{gcm == "ICHEC-EC-EARTH" & ensemble != "r12i1p1"}] %>% 
  .[variable != "orog", .(gcm, institute_rcm, experiment, ensemble)] %>% 
  unique %>% 
  .[, .N, experiment]
size_full*frac_tn_tnaa
size_full*frac_tn_tnaa/95