#' Reads names of HRUs from .prj file
#'
#' @description This function reads names of HRUs from a .prj file. The function \code{readPrjParameters}
#' will not work, as it only returns numeric values.
#' @param prjFile Required. Name of .prj file.
#' @param logfile logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If successful, returns the parameter values as a vector. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @seealso  \code{\link{setPrjHRUnames}}
#' @export
#'
#' @examples \dontrun{
#' # read in as a vector
#' HRUnames <- readPrjHRUnames("Bad_Lake_1974-1975.prj")}

readPrjHRUnames <- function(prjFile = "", logfile = ""){
  # check parameters
  if (prjFile == "") {
    cat('Missing CRHM input .prj file name\n')
    return(FALSE)
  }

  # get number of HRUs
  dims <- readPrjDimensions(prjFile)
  HRU_count <- dims$value[1]

  prj <- readPrj(prjFile)

  HRU_names <- readPrjTextVals(prj,"hru_names", HRU_count)

  # log action
  comment <- paste('readPrjHRUnames: ', prjFile, sep = "")
  result <- logAction(comment, logfile)
  if (!result) {
    return(result)
  }
  else{
    return(HRU_names)
  }

}
