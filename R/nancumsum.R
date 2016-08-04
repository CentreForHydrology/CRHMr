#' Finds cumulative sum of non-missing values in an array
#'
#' @param x Required. An array or vector of numeric values
#'
#' @return Returns an array or vector with cumulative sums of each column.
#' @author Kevin Shook. The code is copied from a MATLAB program nancumsum.m written by Alan Barr.
#' @export
#' @seealso  \code{\link{PcpFiltPosTh}}
#' @examples
#' a <- nancumsum(BadLake7376[,2])
nancumsum <- function(x){
  # check to see if an array or vector
  if (is.vector(x)){
    vectorlength <- length(x)
    cs <- rep.int(NA_real_, vectorlength) 
    iYaN <- which(!is.na(x))
    cs[iYaN] <- cumsum(x[iYaN])
  }
  else{
    nt <- dim(x)[1]
    nc <- dim(x)[2]
    cs <- matrix(NA_real_, nt, nc) ()
    
    for (ic in 1:nc){
      iYaN <- which(!is.na(x[,ic]))
      cs[iYaN,ic] <- cumsum(x[iYaN,ic])
    }
    
  }

  return(cs)
}