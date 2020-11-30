#' Reads maxtrix of specified parameter from .prj file
#'
#' @description This function reads numeric parameter values which are in a square matrix where the number of
#' rows and columns are equal to the number of HRUs.
#' @param prjFile Required. Name of .prj file.
#' @param paramName Required. Name of parameter to be read.
#' @param logfile logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If successful, returns the parameter values as a matrix, with column and row names of the HRU names. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @seealso  \code{\link{readPrjParameters}} \code{\link{setPrjMatrix}}
#' @export
#'
#' @examples \dontrun{
#' # read in as a matrix
#' infile <- "Cluster1_PTH_100km2_WFDEI-GEM_new_watershed10_50%_IN.prj"
#' variables <- readPrjMatrix(infile, "distrib_Route") }

readPrjMatrix <- function(prjFile = "", paramName = "", logfile = "") {
  # check parameters
  if (prjFile == "") {
    cat('Missing CRHM input .prj file name\n')
    return(FALSE)
  }

  if (paramName == "") {
    cat('Missing parameter name\n')
    return(FALSE)
  }
  result <- readPrjParameters(prjFile, paramName, logfile)

  if (length(result) <= 1) {
    return(result)
  }
  else{
    size <- length(result)
    nrow <- sqrt(size)
    ncol <- nrow
    hru_names <- readPrjHRUnames(prjFile)
    parameter_matrix <- matrix(result, nrow = nrow, ncol = ncol, byrow = TRUE, dimnames =list(hru_names, hru_names))
    # log action
    comment <- paste('readPrjMatrix prjFile: ', prjFile, sep = "")
    result <- logAction(comment, logfile)
    if (!result) {
      return(result)
    }
    else{
      return(parameter_matrix)
    }
  }
}
