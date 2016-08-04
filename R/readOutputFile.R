#' Reads CRHM model output into a data frame
#'
#' @description Reads a file containing output from a CRHM model into a \pkg{CRHMr} data frame. Note that because of the way that \R reads in files, the variable names will have appended periods, e.g. \code{t.1}. This version reads in both old CRHM output (without units) and new CRHM output which contains the variable units in the second line.
#' @param outputFile Required. The name of the CRHM output file to be read in.
#' @param timezone Required. The name of the timezone of the data as a character string. This should be the timezone of your data, but omitting daylight savings time. Note that the timezone code is specific to your OS. To avoid problems, you should use a timezone without daylight savings time. Under Linux, you can use \option{CST} and \option{MST} for Central Standard or Mountain Standard time, respectively. Under Windows or OSX, you can use \option{etc/GMT+6} or \option{etc/GMT+7} for Central Standard and Mountain Standard time. DO NOT use \option{America/Regina} as the time zone, as it includes historical changes between standard and daylight savings time.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If successful, returns a \pkg{CRHMr} data frame. If unsuccessful, returns the value \code{FALSE}.
#' @author Kevin Shook
#' @seealso \code{\link{readExportFile}} \code{\link{readObsFile}} \code{\link{readOutputUnits}}
#' @examples
#' \dontrun{
#' stoon <- readOutputFile('CRHM_output_1.txt', 'etc/GMT+6')}
#' @export

readOutputFile <- function(outputFile, timezone='', quiet=TRUE, logfile=''){

  # check parameters
  if (outputFile == ''){
    cat('Error: must specify a file name\n')
    return(FALSE)
  }  
  if (timezone == ''){
    cat('Error: must specify a timezone\n')
    return(FALSE)
  }  
  # check for '#' symbols in header
  con <- file(outputFile, "r", blocking = FALSE)
  input <- readLines(con)
  close(con)
  
  line1 <- input[1]
  
  # remove parentheses
  variables <- stringr::str_replace_all(line1, stringr::fixed('('),'.')
  variables <- stringr::str_replace_all(variables, stringr::fixed(')'),'')
  
  # replace tabs with spaces to allow for parsing
  variables <- stringr::str_replace_all(variables, '#','.calc') 
  variables <- stringr::str_split(variables, '\t')
  
  # check for units
  line2 <- input[2]
  units_present <- stringr::str_detect(line2, 'units')
  
  if(units_present)
    skiplines <- 2
  else
    skiplines <- 1

  output <- read.table(outputFile, header=FALSE, skip=skiplines, stringsAsFactors=FALSE)
  names(output) <- variables[[1]]
  
  # convert Excel time to timeDate
  output$time <- as.POSIXct(as.numeric(output[,1])*24*3600, origin="1899-12-30",tz='UTC')
  output$time <- lubridate::force_tz(output$time, tzone=timezone)
  names(output)[1] <- 'datetime'
  num.cols <- ncol(output)
  
  # clean up dates
  output <- makeRegular(output, timezone=timezone)
  
  # output info to screen and write to log file
  file.info <- CRHM_summary(output)
  if (!quiet)
    print(file.info)
  
  comment <- paste('readOutputFile outputFile:', outputFile, ' Timezone:', timezone, sep='')
  result <- logAction(comment, logfile)
  
  if(result)
    return(output)
  else
    return(result)
}
