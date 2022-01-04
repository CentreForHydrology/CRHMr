#' Sets the names of the obs files in a CRHM model \code{.prj} file
#'
#' @param inputPrjFile Required. Name of the \code{.prj} file.
#' @param obsFiles Optional. A string or vector of strings containing the names of the obs file(s). If none are supplied, then the existing obs files will be deleted.
#' @param outputPrjFile Optional. If omitted, the input \code{.prj} file will be overwritten.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If successful, returns \code{TRUE}. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @seealso \code{\link{runCRHM}} \code{\link{setPrjDates}} \code{\link{setPrjParameters}}
#' @export
#'
#' @examples
#' \dontrun{
#' obsFiles <- c("BadLakeHourly.obs", "BadLakeppt.obs")
#' result <- setPrjObs("c:/CRHM/Bad Lake 1974-1975.prj", obsFiles}
setPrjObs <- function(inputPrjFile = "", obsFiles = "", outputPrjFile = "",
                      logfile = "") {
  # check parameters
  if (inputPrjFile == "") {
    cat("Missing CRHM input .prj file name\n")
    return(FALSE)
  }

  # read in .prj file
  prj <- readPrj(inputPrjFile)

  # find start and end of obs
  linenum <- grep("Observations:", prj, fixed = TRUE)
  first_piece <- prj[1:(linenum + 1)]

  rest <- prj[-(1:(linenum + 1))]
  numLines <- length(rest)
  hashloc <- grep("#", rest, fixed = TRUE)
  hashloc <- hashloc[1]
  rest <- rest[hashloc:numLines]

  # combine all lines together
  if (length(obsFiles) > 1) {
    allLines <- c(first_piece, obsFiles, rest)
  } else {
    if (obsFiles != "") {
      allLines <- c(first_piece, obsFiles, rest)
    }
    else {
      allLines <- c(first_piece, rest)
    }
  }


  # write to file
  if (outputPrjFile == "") {
    outputPrjFile <- inputPrjFile
  }

  writePrj(allLines, outputPrjFile)

  # log action
  comment <- paste(
    "setObs input file: ", inputPrjFile,
    " output file:", outputPrjFile
  )

  result <- logAction(comment, logfile)
  return(result)
}
