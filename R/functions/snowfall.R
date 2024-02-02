# snowfall separation based on pr, tasmax, tasmin

# th: temperature threshold (degC)

snowfall <- function(pr, tasmax, tasmin, th = 2){
  out <- pr
  out[tasmin > th] <- 0
  # out[tasmax <= th]
  lgl_mix <- tasmax > th & tasmin < th
  if(any(lgl_mix)){
    out[lgl_mix] <- pr[lgl_mix] * (th - tasmin[lgl_mix])/(tasmax[lgl_mix] - tasmin[lgl_mix])
  }
  out
}