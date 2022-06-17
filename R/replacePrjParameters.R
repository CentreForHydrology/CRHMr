#' Replaces the parameter values in a CRHM model \code{.prj} file
#'
#' @description Replaces the existing values of a single parameter, for all HRUs,
#' in a \code{.prj} file. This differs from \code{setPrjParameters} in that this function can
#' change the number of values (i.e. the number of HRUs) for a parameter. It
#' is intended for use in \code{deletePrjHRUs}. Note
#' that this function is very destructive, so it's a good idea to have a backup copy
#' of your \code{.prj} file \emph{before} running it!
#' @param inputPrjFile Required. Name of the \code{.prj} file.
#' @param paramName Required. Name of parameter to set.
#' @param paramVals Required. Vector containing new parameter values.
#' @param outputPrjFile Optional. If omitted, the input \code{.prj} file will be overwritten.
#' @param oldParamCount Optional. If > 0, then this parameter sets the number of
#' HRUs in the original \code{.prj} file. Otherwise the number of HRUs is read
#' from the file. The parameter is needed when the number of HRUs in the \code{.prj}
#' file \code{Dimensions} section does not match the actual number of HRU values
#' for the parameter.
#' @param quiet Optional. Suppresses display of messages, except for errors. I
#' f you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action.
#' Normally not used.
#' @return If successful, returns \code{TRUE}. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @note Currently, this function has only been tested on simple \code{.prj} files,
#' i.e. without REW Grp parameters. It \emph{might} work for complex files if
#' the \option{REW Grp} specification is included in the parameter name.
#' @seealso  \code{\link{deletePrjHRUs}} \code{\link{setPrjParameters}}
#' @export
#'
#' @examples
#' \dontrun{
#' result <- replacePrjParameters("CRHM/Bad74_Frozen.prj",
#' "fetch", c(1500, 100))}
replacePrjParameters <- function(inputPrjFile='', paramName='',
                             paramVals='', outputPrjFile='',
                             oldParamCount = 0,
                             quiet=TRUE, logfile='') {

  if (oldParamCount < 1) {
    model_dims <- readPrjDimensions(inputPrjFile)
    oldParamCount <- model_dims[1]
  }


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

  # if more than one match, use only the first one
  if (length(start_line) > 1) {
    start_line <- start_line[1]
  }

  if (!quiet)
    cat('Found parameters: ', prj[start_line],'\n', sep = '')


  line_num <- start_line + 1
  done <- FALSE
  total_vals <- 0
  # find the beginning and end of the values

  while (!done) {
    # check to see if numeric or not
    current <- prj[line_num]
    current_vals <- parseText(current)

    if (is.numeric(type.convert(current_vals, as.is = TRUE))) {
      # found values
      num_vals <- length(current_vals)
      total_vals <- total_vals + num_vals
    }
    if (total_vals >= oldParamCount)
      done <- TRUE
    else
     line_num <-  line_num + 1
  }

  # replace parameter values
  replace_vals <- paste(stringr::str_c(paramVals, collapse = " "),  sep = '')
  prj[start_line + 1] <- replace_vals

  # delete un-needed parameter values
  found_lines <- line_num - start_line

  if (found_lines > 1) {
     prj <- prj[-((start_line + 2):line_num)]
  }


  # write to file
  if (outputPrjFile == '')
    outputPrjFile <- inputPrjFile

  writePrj(prj, outputPrjFile)

  # log action
  comment <- paste('replacePrjPrameters input file: ', inputPrjFile,
                   ' output file:', outputPrjFile,
                   ' paramName:',paramName)

  result <- logAction(comment, logfile)
  return(result)
}
