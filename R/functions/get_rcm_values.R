library(terra)
#' retrieve rcm values based on xy, xy-buffer, elev, elev-buffer
#' 
#' use pointer to rast() object of low-res values
#'
#' @param i_cell cell-index high-res rast
#' @param rs_cells_rcm_obs cell-indices of low-res rast in high-res dimensions
#' @param rs_rcm rast to extract from (low-res, 0.11)
#' @param rs_rcm_orog rast with elevation (low-res, 0.11), needed if type="elev"
#' @param type "xy" or "elev"
#' @param n_cells number of cells to return (xy: width of square around i_cell in
#'   0.11 pixels must be odd; elev: number of cells with highest weights)
#' @param elev_obs value of high-res elevation of i_cell, needed if type="elev"
#' @param weight_by halving weights for horizontal and vertical distances if
#'   type="elev" (units: m)
#'
#' @return matrix of values, first column is main vector, other columns only if
#'   buffer
#' @export
#'
#' @examples
get_rcm_values <- function(i_cell, 
                           rs_cells_rcm_obs, 
                           rs_rcm,
                           rs_rcm_orog,
                           type,
                           n_cells = 1L,
                           elev_obs,
                           weight_by = list(tau_h = 50000, tau_v = 200)){
  
  stopifnot(type %in% c("xy", "elev"))
  
  i_cell_rcm <- as.vector(rs_cells_rcm_obs)[i_cell]
  
  if(type == "xy"){
    i_row <- rowFromCell(rs_rcm, i_cell_rcm)
    i_col <- colFromCell(rs_rcm, i_cell_rcm)
    
    
    if(n_cells == 1L){
      i_nrows <- 1
      i_ncols <- 1
    } else {
      stopifnot(n_cells %% 2 == 1)
      neigh_cells <- adjacent(rs_rcm, i_cell_rcm, 
                              directions = matrix(T, n_cells, n_cells)) %>% as.vector()
      neigh_cells <- neigh_cells[!is.na(neigh_cells)]
      rc <- rowColFromCell(rs_rcm, neigh_cells)
      rc_min <- apply(rc, 2, min)
      rc_max <- apply(rc, 2, max)
      i_row <- rc_min[1]
      i_col <- rc_min[2]
      i_nrows <- rc_max[1] - rc_min[1] + 1 
      i_ncols <- rc_max[2] - rc_min[2] + 1 
    }
      
    vals_mat <- values(rs_rcm, mat = T, 
                       nrows = i_nrows, ncols = i_ncols,
                       row = i_row, 
                       col = i_row)
    
    vals_rcm <- unname(t(vals_mat))
    
    # put cell of interest first
    i_coi <- which(neigh_cells == i_cell_rcm)
    vals_rcm <- cbind(vals_rcm[, i_coi, drop = F], vals_rcm[, -i_coi, drop = F])
    
    
    
  } else if(type == "elev"){
    
    rs_dist_h <- rast(rs_rcm_orog, vals = NA_real_)
    rs_dist_h[i_cell_rcm] <- 1
    rs_weight_h <- exp( -(distance(rs_dist_h)^2) / (weight_by$tau_h^2/log(2)) ) 
    rs_weight_v <- exp( -((rs_rcm_orog - elev_obs)^2) / (weight_by$tau_v^2/log(2)) ) 
    rs_weight <- rs_weight_h*rs_weight_v
    
    if(n_cells == 1L){
      i_cell_rcm_selected <- which.max(as.vector(rs_weight))
    } else {
      i_cell_rcm_selected <- sort(as.vector(rs_weight), 
                                  decreasing = T, 
                                  index.return = T)$ix[1:n_cells]
    }
    
    vals_rcm <- vapply(i_cell_rcm_selected, 
                       FUN.VALUE = double(dim(rs_rcm)[3]), 
                       function(x){
                         values(rs_rcm, mat = F, nrows = 1, ncols = 1,
                                row = rowFromCell(rs_rcm, x), 
                                col = colFromCell(rs_rcm, x))
                       })
    
  }
  
  
  vals_rcm
  
}
