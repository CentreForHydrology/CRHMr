#' Sets the Run ID in a CRHM model \code{.prj} file
#'
#' @param inputPrjFile Required. Name of the \code{.prj} file.
#' @param RunID Required. New RunID for basin. Must be an integer.
#' @param outputPrjFile Optional. If omitted, the input \code{.prj} file will be overwritten.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If successful, returns \code{TRUE}. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @seealso  \code{\link{runCRHM}} \code{\link{setPrjHRUnames}} \code{\link{setPrjDates}} \code{\link{setPrjParameters}}
#' @export
#'
#' @examples
#' \dontrun{
#' result <- setPrjRunID('c:/CRHM/Bad Lake 1974-1975.prj', '5')}
setPrjRunID <- function(inputPrjFile = "", RunID = 0, outputPrjFile = "", logfile = "") {
  # check parameters
  if (inputPrjFile == "") {
    cat("Missing CRHM input .prj file name\n")
    return(FALSE)
  }

  # read in .prj file
  prj <- readPrj(inputPrjFile)

  # find basin name in .prj
  linenum <- grep("RUN_ID", prj, fixed = TRUE)

  if (is.null(linenum)) {
    cat("Missing existing Run ID parameter\n")
    return(FALSE)
  }
  prj[linenum + 1] <- RunID

  # write to file
  if (outputPrjFile == "") {
    outputPrjFile <- inputPrjFile
  }

  writePrj(prj, outputPrjFile)

  # log action
  comment <- paste("setPrjDates input file: ", inputPrjFile,
    " output file:", outputPrjFile, " Run_ID:", RunID,
    sep = ""
  )

  result <- logAction(comment, logfile)
  return(result)
}
