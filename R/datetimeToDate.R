#' Converts an \R datetime to a date
#'
#' @description This is used when a \pkg{CRHMr} date is stored as a datetime (POSIXct) variable.
#' @param dataframe Required. A data frame. The first name of the column must be a POSIXct variable, which can be converted to a standard \R date.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If successful, returns a data frame where the first column is a standard \pkg{CRHMr} date. If unsuccessful, returns the value \code{FALSE}.
#' @author Kevin Shook
#' @seealso \code{\link{dateToDatetime}} 
#' @export
#'
#' @examples
#' Badlake.radiation.daily <- aggDataframe(BadLake7376, period='daily', 
#' columns=c(6,7,8), funs=c('mean'))
#' BadLake.datetime <- dateToDatetime(Badlake.radiation.daily, timezone='CST')
#' BadLake.date <- datetimeToDate(BadLake.datetime)
datetimeToDate <- function(dataframe, logfile=''){
  # check parameters
  if(is.null(dataframe)){
    cat('Error: missing datafame\n')
    return(FALSE)
  }
  if (!lubridate::is.POSIXt(dataframe[,1])){
    cat('Error: first column is not POSIX\n')
    return(FALSE)
  }

  dataframeName <- deparse(substitute(dataframe))
  
  # convert data
  datetime <- dataframe[,1]
  data_vals <- dataframe[,-1]
  data_names <- names(dataframe[,-1])
  datevals <- paste(format(datetime, format='%Y-%m-%d'))
  datevals <- as.Date(datevals, format='%Y-%m-%d')
  dataframe <- data.frame(datevals,data_vals)
  names(dataframe)<- c('date', data_names)
  
  comment <- paste('datetimeToDate dataframe:', dataframeName, sep='')
  result <- logAction(comment, logfile)
  if (result)
    return (dataframe)
  else
    return(result)
}