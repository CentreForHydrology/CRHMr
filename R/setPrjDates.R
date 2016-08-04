#' Sets the model run start and end dates in a CRHM model \code{.prj} file
#'
#' @param inputPrjFile Required. Name of the \code{.prj} file.
#' @param startDate Required. Model run starting date in format \option{YYYY MM DD}.
#' @param endDate Required. Model run ending date in format \option{YYYY MM DD}.
#' @param outputPrjFile Optional. If omitted, the input \code{.prj} file will be overwritten.
#' @param initialStateFile Optional. If specified, the specified file will be set as the initial state file, i.e. the state variables to be read in at the beginning of the model run.
#' @param finalStateFile Optional. If specified, the specified file will be set as the final state file, i.e. the state variables to be exported at the end of the model run.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If successful, returns \code{TRUE}. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @note If you are using Windows, the paths for the state files must use \option{\\\\} instead of \option{\\} for the directory delimiter. The reason is that R, like many programming languages, interprets \option{\\} as an escape code. This is different from the file path set in \option{inputPrjFile}, which must be an \R path using the \option{/} symbol. See the example below.
#' @seealso  \code{\link{runCRHM}}
#' @export
#'
#' @examples
#' \dontrun{
#' result <- setPrjDates('c:/CRHM/Bad Lake 1974-1975.prj', 
#' '1974 10 1', '1975 10 1', initialStateFile='c:\\CRHM\\Model_Initial_State.int',
#'  finalStateFile='c:\\CRHM\\Model_Final_State.int')}
setPrjDates <- function(inputPrjFile='', startDate='', endDate='', outputPrjFile='', 
                        initialStateFile='', finalStateFile='', 
                        logfile='') {
  eol_val <- win.eol()
  
  # check parameters
  if (inputPrjFile == ''){
    cat('Missing CRHM input .prj file name\n')
    return(FALSE)  
  }

  if (startDate == ''){
    cat('Missing model start date\n')
    return(FALSE)  
  }
  
  if (endDate == ''){
    cat('Missing model end date\n')
    return(FALSE)  
  }
  
  # read in .prj file
  prj <- readPrj(inputPrjFile)
 
  # find start and end dates in .prj
  linenum <- grep('Dates:', prj, fixed=TRUE)
  prj[linenum+2] <- startDate
  prj[linenum+3] <- endDate  
  
  # check to see if state files need to be set, and if they are already set
  
  if (initialStateFile != ''){
    # see if lines indicating the initial state file are present
    currentStateFileLoc <- grep('Initial_State', prj, fixed=TRUE)
    
    if (length(currentStateFileLoc) < 1)
      # did not find setting so append
      prj <- c(prj, c('Initial_State', '######', initialStateFile, '######')) 
    
    else{
      # the line 'Initial_State' exists, so see if it contains a value
      nextLine2 <- prj[currentStateFileLoc + 2]
      hashPresent <- stringr::str_detect(nextLine2, '#')
      if (hashPresent){
        # missing initial state filename
        prj1 <- prj[1:(currentStateFileLoc+1)]
        prj2 <- prj[(currentStateFileLoc+2):length(prj)]
        prj <- c(prj1, initialStateFile, prj2)
      }
      else{
        prj[currentStateFileLoc + 2] <- initialStateFile        
      }

    }
  }
  
  if (finalStateFile != ''){
    # see if lines indicating the initial state file are present
    currentStateFileLoc <- grep('Final_State', prj, fixed=TRUE)
    
    if (length(currentStateFileLoc) < 1)
      # did not find setting so append
      prj <- c(prj, c('Final_State', '######', finalStateFile, '######')) 
    
    else{
      # the line 'Initial_State' exists, so see if it contains a value
      nextLine2 <- prj[currentStateFileLoc + 2]
      hashPresent <- stringr::str_detect(nextLine2, '#')
      if (hashPresent){
        # missing initial state filename
        prj1 <- prj[1:(currentStateFileLoc+1)]
        prj2 <- prj[(currentStateFileLoc+2):length(prj)]
        prj <- c(prj1, finalStateFile, prj2)
      }
      else{
        prj[currentStateFileLoc + 2] <- finalStateFile        
      }
    }
  }
  
  # write to file
  if (outputPrjFile == '')
    outputPrjFile <- inputPrjFile
  
  writePrj(prj, outputPrjFile)
  
  # log action
  comment <- paste('setPrjDates input file: ', inputPrjFile, 
'output file: outputPrjFile startDate:',startDate, ' endDate:', endDate, sep='')
  
  result <- logAction(comment, logfile)
  return(result)
}