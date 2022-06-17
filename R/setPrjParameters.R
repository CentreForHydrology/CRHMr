#' Sets the parameter values in a CRHM model \code{.prj} file
#' @description Replaces the existing values of a single parameter, for all HRUs, in a \code{.prj} file.
#' @param inputPrjFile Required. Name of the \code{.prj} file.
#' @param paramName Required. Name of parameter to set.
#' @param paramVals Required. Vector containing new parameter values. There must be at least as many values in this vector as parameter values in the orginal \code{.prj} file.
#' @param outputPrjFile Optional. If omitted, the input \code{.prj} file will be overwritten.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling
#' this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default).
#' If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If successful, returns \code{TRUE}. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @note Currently, this function has only been tested on simple \code{.prj} files, i.e. without REW Grp parameters. It \emph{might} work for complex files if the \option{REW Grp} specification is included in the parameter name.
#' @seealso  \code{\link{runCRHM}} \code{\link{setPrjDates}}
#' @export
#'
#' @examples
#' \dontrun{
#' result <- setPrjParameters('c:/CRHM/Bad74_Frozen.prj',
#' 'fetch', c(1500, 1500, 1500))}
setPrjParameters <- function(inputPrjFile='', paramName='',
                             paramVals='', outputPrjFile='',
                             quiet=TRUE, logfile='') {
  eol_val <- win.eol()
  param_count <- length(paramVals)

  # check parameters
  if (inputPrjFile == '') {
    cat('Missing CRHM input .prj file name\n')
    return(FALSE)
  }

  if (paramName == '') {
    cat('Missing parameter name\n')
    return(FALSE)
  }

  if (paramName == '') {
    cat('Missing parameter values\n')
    return(FALSE)
  }

  # read in .prj file
  prj <- readPrj(inputPrjFile)

  # find start and end of parameters
  start_line <- grep(paramName, prj, fixed = TRUE)
  if (length(start_line) == 0) {
    cat('Could not find the specified parameter\n')
    return(FALSE)
  }

  if (!quiet)
    cat('Found parameters: ', prj[start_line],'\n', sep = '')


  line_num <- start_line + 1
  done <- FALSE
  total_vals <- 0
  while (!done) {
    # check to see if numeric or not
    current <- prj[line_num]
    current_vals <- parseText(current)

    if (is.numeric(type.convert(current_vals, as.is = TRUE))) {
      # found values
      num_vals <- length(current_vals)
      total_vals <- total_vals + num_vals

      if (total_vals > param_count) {
        cat('More parameters in file than specified\n')
        return(FALSE)
      }

      # assemble replacement values
      replace_start <- (total_vals - num_vals) + 1
      replace_vals <- stringr::str_c(paramVals[replace_start:total_vals],
                                     collapse = ' ')

      prj[line_num] <- replace_vals

      if (total_vals == param_count)
        done <- TRUE
    }
    else
      done <- TRUE

    if (line_num > start_line + param_count) {
      done <- TRUE
      cat('Could not find the all the parameter values\n')
      return(FALSE)
    }
    line_num <-  line_num + 1
  }

  # write to file
  if (outputPrjFile == '')
    outputPrjFile <- inputPrjFile

  writePrj(prj, outputPrjFile)

  # log action
  comment <- paste('setPrjPrameters input file: ', inputPrjFile,
                   ' output file:', outputPrjFile,
                   ' paramName:',paramName)

  result <- logAction(comment, logfile)
  return(result)
}
