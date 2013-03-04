moveC <- function(map, time){
  #passes these arguments from the BML object "map" down to C
  # map is the matrix, unwound as a long vector (by columns)
  # t is the number of timesteps
  stopifnot(class(map) == c('BML', 'matrix'))
  stopifnot(class(time) == 'numeric' & time > 0)
  nrow <- attributes(map)$dim[1]
  ncol <- attributes(map)$dim[2]
  CC <- .C("move", result = as.integer(map), as.integer(time), as.integer(nrow), as.integer(ncol))$result
  moved <- matrix(CC, nrow = nrow, ncol = ncol)
  attributes(moved) <- list(time = time, dim = c(nrow,ncol),
                           prop.red = length(moved[moved == 2])/length(moved), 
                           prop.blue = length(moved[moved == 1])/length(moved),
                           prop.cars = length(moved[moved == 1 | moved == 2])/length(moved))
  class(moved) <- c('BML', 'matrix')
  return(moved) 
}
