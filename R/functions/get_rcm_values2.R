library(terra)
#' retrieve rcm values based on xy, xy-buffer, elev, elev-buffer
#' 
#' use matrix of low-res values
#'
#' @param i_cell cell-index high-res rast
#' @param rs_cells_rcm_obs cell-indices of low-res rast in high-res dimensions
#' @param mat_rcm matrix to extract from (low-res, 0.11), dim: ncell*date
#' @param rs_rcm_orog rast with elevation (low-res, 0.11), needed if type="elev"
#'   or if type="xy" and n_cells > 1
#' @param type "xy" or "elev"
#' @param n_cells number of cells to return (xy: width of square around i_cell
#'   in 0.11 pixels must be odd; elev: number of cells with highest weights)
#' @param elev_obs value of high-res elevation of i_cell, needed if type="elev"
#' @param weight_by halving weights for horizontal and vertical distances if
#'   type="elev" (units: m)
#'
#' @return matrix of values, first column is main vector, other columns only if
#'   buffer
#' @export
#'
#' @examples
get_rcm_values2 <- function(i_cell, 
                            rs_cells_rcm_obs, 
                            mat_rcm,
                            rs_rcm_orog,
                            type,
                            n_cells = 1,
                            elev_obs,
                            weight_by = list(tau_h = 50000, tau_v = 200)){
  
  stopifnot(type %in% c("xy", "elev"))
  
  i_cell_rcm <- as.vector(rs_cells_rcm_obs)[i_cell]
  
  if(type == "xy"){

    if(n_cells == 1){
      vals_mat <- mat_rcm[i_cell_rcm, , drop = F]
      i_coi <- 1
    } else {
      stopifnot(n_cells %% 2 == 1)
      neigh_cells <- adjacent(rs_rcm_orog, i_cell_rcm, 
                              directions = matrix(T, n_cells, n_cells)) %>% as.vector()
      neigh_cells <- neigh_cells[!is.na(neigh_cells)]
      vals_mat <- mat_rcm[neigh_cells, ]
      i_coi <- which(neigh_cells == i_cell_rcm)
    }
    
    vals_rcm <- unname(t(vals_mat))
    # put cell of interest first
    vals_rcm <- cbind(vals_rcm[, i_coi, drop = F], vals_rcm[, -i_coi, drop = F])
    
  } else if(type == "elev"){
    
    rs_dist_h <- rast(rs_rcm_orog, vals = NA_real_)
    rs_dist_h[i_cell_rcm] <- 1
    rs_weight_h <- exp( -(distance(rs_dist_h)^2) / (weight_by$tau_h^2/log(2)) ) 
    rs_weight_v <- exp( -((rs_rcm_orog - elev_obs)^2) / (weight_by$tau_v^2/log(2)) ) 
    rs_weight <- rs_weight_h*rs_weight_v
    
    if(n_cells == 1){
      i_cell_rcm_selected <- which.max(as.vector(rs_weight))
    } else {
      i_cell_rcm_selected <- sort(as.vector(rs_weight), 
                                  decreasing = T, 
                                  index.return = T)$ix[1:n_cells]
    }
    vals_mat <- mat_rcm[i_cell_rcm_selected, ]
    vals_rcm <- unname(t(vals_mat))
    
  }
  
  
  vals_rcm
  
}
