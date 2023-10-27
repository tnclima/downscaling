# optimal chunk shape 3D

# from:
# https://www.unidata.ucar.edu/blogs/developer/en/entry/chunking_data_choosing_shapes
# https://www.unidata.ucar.edu/blog_content/data/2013/chunk_shape_3D.py



# Return a 'good shape' for a 3D variable, assuming balanced 1D/(n-1)D access
# 
# varShape  -- length 3 list of variable dimension sizes
# chunkSize -- maximum chunksize desired, in bytes (default 4096)
# valSize   -- size of each data value, in bytes (default 4)
# 
# Returns integer chunk lengths of a chunk shape that provides
# balanced access of 1D subsets and 2D subsets of a netCDF or HDF5
# variable var with shape (T, X, Y), where the 1D subsets are of the
# form var[:,x,y] and the 2D slices are of the form var[t,:,:],
# typically 1D time series and 2D spatial slices.  'Good shape' for
# chunks means that the number of chunks accessed to read either
# kind of 1D or 2D subset is approximately equal, and the size of
# each chunk (uncompressed) is no more than chunkSize, which is
# often a disk block size.


chunk_shape_3D <- function(varShape, valSize = 4, chunkSize = 4096){
  
  rank <- 3
  chunkVals <- chunkSize / valSize # ideal number of values in a chunk
  numChunks  <- prod(varShape) / chunkVals # ideal number of chunks
  axisChunks <- numChunks^0.25 # ideal number of chunks along each 2D axis
  
  # will be first estimate of good chunk shape
  if(varShape[1] / axisChunks^2 < 1){
    chunkDim <- 1
    axisChunks <- axisChunks / sqrt(varShape[1]/axisChunks^2)
  } else {
    chunkDim <- floor(varShape[1] / axisChunks^2)
  }
  
  cFloor <- chunkDim
  prod <- 1  # factor to increase other dims if some must be increased to 1.0
  for(i in 2:rank){
    if(varShape[i] / axisChunks < 1.0) prod <- prod * axisChunks / varShape[i]
  }
    
  for(i in 2:rank){
    if(varShape[i] / axisChunks < 1.0){
      chunkDim <- 1.0
    } else {
      chunkDim = floor((prod*varShape[i]) / axisChunks)
      cFloor <- c(cFloor, chunkDim)
    }
      
  }
 
  
  # cFloor is typically too small, (prod(cFloor) < chunkSize)
  # Adding 1 to each shape dim results in chunks that are too large,
  # (prod(cFloor + 1) > chunkSize).  Want to just add 1 to some of the
  # axes to get as close as possible to chunkSize without exceeding
  # it.  Here we use brute force, compute prod(cCand) for all
  # 2^rank candidates and return the one closest to chunkSize
  # without exceeding it.
  comb01 <- unname(as.matrix(expand.grid(0:1, 0:1, 0:1)))
  cCand <- sweep(comb01, 2, cFloor, "+")
  thisChunkSize <- valSize * apply(cCand, 1, prod)
  iBest <- max(which(thisChunkSize <= chunkSize))
  
  cCand[iBest, ]

}