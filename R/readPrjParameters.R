#' Reads values of specified parameter from .prj file
#'
#' @param prjFile Required. Name of .prj file.
#' @param paramName Required. Name of parameter to be read.
#' @param logfile logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If successful, returns the parameter values. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @seealso  \code{\link{setPrjOutputVariables}}
#' @export
#'
#' @examples \dontrun{
#' # read in as a vector
#' variables <- readPrjParameters("Bad_Lake_1974-1975.prj", "Soil soil_moist_init")}

readPrjParameters <- function(prjFile = "", paramName = "", logfile = ""){

  # check parameters
  if (prjFile == "") {
    cat('Missing CRHM input .prj file name\n')
    return(FALSE)
  }

  if (paramName == "") {
    cat('Missing parameter name\n')
    return(FALSE)
  }

  # read in .prj file
  prj <- readPrj(prjFile)
  num_lines <- length(prj)

  # find start and end of parameters
  start_line <- grep(paramName, prj, fixed = TRUE)
  if (length(start_line) == 0) {
    cat('Error: could not find the parameter!\n')
    return(FALSE)
  }

 # find starting point
  line_num <- start_line + 1
  done <- FALSE
  all_vals <- c(0)

  while (!done) {
    # check if done
    current <- prj[line_num]
    if (stringr::str_detect(current, "#") |
        stringr::str_detect(current, "<") |
        line_num >= num_lines) {
      done <- TRUE
    }
    else {
      # check to see if numeric or not
      current_vals <- parseNums(current)
      all_vals <- c(all_vals, current_vals)
      line_num <- line_num + 1
    }
  }
  # log action
  comment <- paste('readPrjParameters prjFile: ', prjFile, sep = "")
  result <- logAction(comment, logfile)
  if (result) {
    return(all)
  }
  else{
    return(all_vals)
  }

}
