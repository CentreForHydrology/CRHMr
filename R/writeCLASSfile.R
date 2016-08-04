#' Writes forcing data to a CLASS file
#' @description Writes a standard \pkg{CRHMr} data frame to a CLASS forcing data file.
#' @param CLASS Required. A \pkg{CRHMr} data frame containing the fields  \code{SWin}, \code{LWin}, \code{Prec}, \code{T}, \code{qa}, \code{U}, and \code{P}, in addition to the \code{datetime}.
#' @param CLASSfile Required. A the file to hold the CLASS data. 
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If successful, returns the value \code{TRUE}. If unsuccessful, returns the value \code{FALSE}.
#' @export
#'
#' @examples \dontrun{
#' result <- writeCLASSfile(class, 'driv.met')}
writeCLASSfile <- function(CLASS, CLASSfile='',  logfile=''){
  eol.val <- win.eol()
  if (is.null(CLASS)){
    cat('Missing CLASS data\n')
    return(FALSE)  
  }
  
  if (CLASSfile == ''){
    cat('Missing CLASS file name\n')
    return(FALSE)  
  }

  CLASSname <- deparse(substitute(obs))
  
  # get date variables
  year <- as.numeric(format(CLASS$datetime, format='%Y'))
  daynum <- as.numeric(format(CLASS$datetime, format='%j'))
  hour <- as.numeric(format(CLASS$datetime, format='%H'))
  mins <- as.numeric(format(CLASS$datetime, format='%M'))
  
  # format variables
  hour <- sprintf('%6g', hour)
  mins <- sprintf('%8g', mins) 
  daynum <- sprintf('%8g', daynum) 
  year <- sprintf('%8g',year)
  
  Qsi <- sprintf('%12.6f',CLASS[,2])
  Qli <- sprintf('%12.6f', CLASS[,3])
  p <- sprintf('%12.6f', CLASS[,4])
  t <- sprintf('%12.6f', CLASS[,5])
  ea <- sprintf('%12.6f', CLASS[,6])
  u <- sprintf('%12.6f', CLASS[,7])
  ap <- sprintf('%12.2f', CLASS[,8])
  
  output <- data.frame(hour, mins, daynum, year, Qsi, Qli, p, t, ea, u, ap, sep='')
  
  write.table(output, file=CLASSfile, quote=FALSE, sep='', eol=eol.val, 
              row.names=FALSE, col.names=FALSE)
  
  comment <- paste('writeCLASSfile dataframe:', CLASSname,
                   ' CLASS file: ', CLASSfile, sep='')  
  result <- logAction(comment, logfile)
  
  return(TRUE) 
}