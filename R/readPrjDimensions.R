#' Reades the dimensions from a .prj file
#'
#' @param prjFile Required. Name of .prj file.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If successful, returns the dimension values as a data frame with the columns \code{dimension} and \code{value}.
#' The dimensions are \code{nhru} (the number of HRUs), \code{nlay} (the number of layers) and \code{nobs} (the
#' number of obs files). If unsuccessful, the value \code{FALSE} is returned.
#' @author Kevin Shook
#' @seealso  \code{\link{readPrjParameters}}
#' @export
#'
#' @examples \dontrun{model_dims <- readPrjDimensions("Bad74_Frozen.prj")}
#'
readPrjDimensions <- function(prjFile = "", logfile = "") {
  # check parameters
  if (prjFile == "") {
    cat('Missing CRHM input .prj file name\n')
    return(FALSE)
  }


  # read in .prj file
  prj <- readPrj(prjFile)

  # find start and end of parameters
  start_line <- grep("Dimensions", prj, fixed = TRUE)
  if (length(start_line) == 0) {
    cat('Error: could not find the model dimensions!\n')
    return(FALSE)
  }

 # shorten prj to speed up processing and avoid problems
  prj <- prj[start_line:(start_line + 5)]
  nhru_line <- grep("nhru", prj, fixed = TRUE)
  nhru <- as.numeric(parseText(prj[nhru_line])[2])
  nlay_line <- grep("nlay", prj, fixed = TRUE)
  nlay <- as.numeric(parseText(prj[nlay_line])[2])
  nobs_line <- grep("nobs", prj, fixed = TRUE)
  nobs <- as.numeric(parseText(prj[nobs_line])[2])

 # assemble and output

  value <- c(nhru, nlay, nobs)
  dimension <-  c("nhru", "nlay", "nobs")
  dimensions <- data.frame(dimension, value)

  # log action
  comment <- paste('readPrjDimensions prjFile: ', prjFile, sep = "")
  result <- logAction(comment, logfile)
  if (!result) {
    return(result)
  }
  else{
    return(dimensions)
  }

}
