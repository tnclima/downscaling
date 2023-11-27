library(mgcv)

#' Interpolate a variable in 3d with a GAM x,y,orog
#'
#' @param dt_rcm_orog data to train (cols: x,y,orog)
#' @param dt_obs_orog data to predict (same cols as to train)
#' @param vals_rcm values to train (same length as nrow(dt_rcm_orog))
#' @param kk basis dimension for smooth term (default: 5)
#'
#' @return interpolated values (same length as nrow(dt_obs_orog))
#'
lapse_rate_varying <- function(dt_rcm_orog, 
                               dt_obs_orog, 
                               vals_rcm, 
                               kk = 5){
  
  dt_gam <- cbind(dt_rcm_orog, vals_rcm)
  
  gm_fit <- gam(vals_rcm ~ s(x, k = kk) + s(y, k = kk) + s(orog, k = kk), data = dt_gam)
  vals_out <- predict(gm_fit, dt_obs_orog)
  vals_out
  
}