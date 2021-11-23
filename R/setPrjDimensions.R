#' Sets model dimensions in .prj file
#'
#' @param inputPrjFile Required. Name of the \code{.prj} file to read.
#' @param dimensionVals Required. Vector containing new dimension values. There must be 3 values in the
#' correct order: \code{nhru} (the number of HRUs), \code{nlay} (the number of layers) and \code{nobs} (the
#' number of obs files).
#' @param outputPrjFile Optional. If omitted, the input \code{.prj} file will be overwritten.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If successful, returns \code{TRUE}. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @seealso \code{\link{readPrjDimensions}}
#' @export
#'
#' @examples \dontrun{
#' new_dimensions <- c(47, 2, 1)
#' check <- setPrjDimensions("Bad74_Frozen.prj", new_dimensions)}
setPrjDimensions <- function(inputPrjFile = NULL, dimensionVals = NULL,
                             outputPrjFile = NULL, logfile = "") {
  # check parameters
  if (is.null(inputPrjFile))
    stop("Missing CRHM input .prj file name")

  if (is.null(dimensionVals))
    stop("Missing model dimensions")

  # read in .prj file
  prj <- readPrj(inputPrjFile)

  # find start and end of parameters
  start_line <- grep("Dimensions", prj, fixed = TRUE)
  if (length(start_line) == 0)
    stop("Could not find the model dimensions")

  # get existing values and over-write
  old_dimensions <- readPrjDimensions(prjFile = inputPrjFile, logfile = logfile)
  new_dimensions <- old_dimensions
  new_dimensions[, 2] <- dimensionVals

  # replace
  prj[(start_line + 2):(start_line + 4)] <- paste(new_dimensions[, 1], new_dimensions[, 2])

  # write to file
  if (is.null(outputPrjFile))
    outputPrjFile <- inputPrjFile

  writePrj(prj, outputPrjFile)

  # log action
  comment <- paste('setPrjDimensions input file: ', inputPrjFile,
                   ' output file:', outputPrjFile)

  result <- logAction(comment, logfile)
  return(result)

}
