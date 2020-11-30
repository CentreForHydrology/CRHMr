#' Sets the matrix of parameter values in a CRHM model \code{.prj} file
#' @description Replaces the existing values of a single parameter, for all HRUs, in a \code{.prj} file.
#' @param inputPrjFile Required. Name of the \code{.prj} file.
#' @param paramName Required. Name of parameter to set.
#' @param paramMatrix Required. Square matrix containing new parameter values. There must be at as many rows and
#' columns in this vector as HRUs in the orginal \code{.prj} file.
#' @param outputPrjFile Optional. If omitted, the input \code{.prj} file will be overwritten.
#' @param decimals Optional. If 0, number of decimal places in output is not set. If >0, number of decimal places
#' in output is fixed.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function
#' in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working
#' interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If successful, returns \code{TRUE}. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @note Currently, this function has only been tested on simple \code{.prj}
#'  files, i.e. without REW Grp parameters. It \emph{might} work for complex files if the \option{REW Grp} specification is included in the parameter name.
#' @seealso  \code{\link{readPrjMatrix}}
#' @export
#'
#' @examples
#' \dontrun{
#' prj_file <- "Cluster1_PTH_100km2_WFDEI-GEM_new_watershed10_50%_IN.prj"
#' distrib_route_vals <- readPrjMatrix(prj_file, "distrib_Route")
#' distrib_route_vals[, 10] <- 0
#' result <- setPrjMatrix(prj_file, "distrib_Route",distrib_route_vals))}
setPrjMatrix <- function(inputPrjFile='', paramName=NULL,
                             paramMatrix=NULL, outputPrjFile='',
                             decimals = 0,
                             quiet=TRUE, logfile='') {
  eol_val <- win.eol()

  # check parameters
  if (inputPrjFile == ''){
    cat('Missing CRHM input .prj file name\n')
    return(FALSE)
  }

  if (is.null(paramName)){
    cat('Missing parameter name\n')
    return(FALSE)
  }

  if (is.null(paramMatrix)){
    cat('Missing parameter matrix\n')
    return(FALSE)
  }

  # read in .prj file
  prj <- readPrj(inputPrjFile)

  # find start and end of parameters
  start_line <- grep(paramName, prj, fixed=TRUE)
  if(length(start_line) == 0){
    cat('Could not find the specified parameter\n')
    return(FALSE)
  }

  if(!quiet)
    cat('Found parameters: ', prj[start_line],'\n', sep='')


  line_num <- start_line + 1
  done <- FALSE
  total_vals <- 0

  top <- prj[1:start_line]
  all_lines <- top
  # find end of data
  rest <- prj[-(1:line_num)]
  end_line <- grep("[:alpha:]", rest)[1]
  bottom <- rest[end_line:(length(rest))]
  all_lines <- top
  # insert lines
  new_lines <- nrow(paramMatrix)
  for (i in 1:new_lines){
    new_vals <- paramMatrix[i,]
    if(decimals > 0)
      new_vals <- formatC(new_vals, digits = decimals)
    replace_vals <- stringr::str_c(new_vals, collapse=' ')
    all_lines <- c(all_lines, replace_vals)
  }

  all_lines <- c(all_lines, bottom)


  # write to file
  if (outputPrjFile == '')
    outputPrjFile <- inputPrjFile

  writePrj(all_lines, outputPrjFile)

  # log action
  comment <- paste('setPrjMarix input file: ', inputPrjFile,
                   ' output file:', outputPrjFile,
                   ' paramName:',paramName)

  result <- logAction(comment, logfile)
  return(result)
}
