#' Sets the basin name in a CRHM model \code{.prj} file
#'
#' @param inputPrjFile Required. Name of the \code{.prj} file.
#' @param basinName Required. New name for basin.
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
#' result <- setPrjBasinName('c:/CRHM/Bad Lake 1974-1975.prj', 'NewBadLakemodel')}
setPrjBasinName <- function(inputPrjFile='', basinName='', outputPrjFile='', logfile='') {

  # check parameters
  if (inputPrjFile == ''){
    cat('Missing CRHM input .prj file name\n')
    return(FALSE)
  }

  if (basinName == ''){
    cat('Missing name for basin\n')
    return(FALSE)
  }


  # read in .prj file
  prj <- readPrj(inputPrjFile)

  # find basin name in .prj
  linenum <- grep('basin_name', prj, fixed=TRUE)
  prj[linenum+1] <- paste("'",basinName,"'", sep='')


  # write to file
  if (outputPrjFile == '')
    outputPrjFile <- inputPrjFile

  writePrj(prj, outputPrjFile)

  # log action
  comment <- paste('setPrjDates input file: ', inputPrjFile,
' output file:', outputPrjFile, ' basinName:' , basinName, sep='')

  result <- logAction(comment, logfile)
  return(result)
}
