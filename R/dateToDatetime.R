#' Converts an \R date to a datetime
#'
#' @description The \code{aggDataframe} function produces non-standard data frames, which have dates rather than datetimes as their first column. This function will convert a data frame with dates in the first column to have datetimes with the time being set to a constant value.
#' @param dataframe Required. A data frame. The first name of the column must be \option{date}, and it must be standard \R date.
#' @param hour Optional. The hour to be used for the datetime values. Default is \code{0}.
#' @param timezone Required. The name of the timezone of the data as a character string. This should be the timezone of your data, but omitting daylight savings time. Note that the timezone code is specific to your OS. To avoid problems, you should use a timezone without daylight savings time. Under Linux, you can use \option{CST} and \option{MST} for Central Standard or Mountain Standard time, respectively. Under Windows or OSX, you can use \option{etc/GMT+6} or \option{etc/GMT+7} for Central Standard and Mountain Standard time. DO NOT use \option{America/Regina} as the time zone, as it includes historical changes between standard and daylight savings time.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If successful, returns a data frame where the first column is a standard \pkg{CRHMr} datetime. If unsuccessful, returns the value \code{FALSE}.
#' @author Kevin Shook
#' @seealso \code{\link{aggDataframe}} \code{\link{datetimeToDate}}
#' @export
#'
#' @examples
#' Badlake.radiation.daily <- aggDataframe(BadLake7376, period='daily',
#' columns=c(6,7,8), funs=c('mean'))
#' BadLake.datetime <- dateToDatetime(Badlake.radiation.daily, timezone='CST')
#'
dateToDatetime <- function(dataframe, hour=0, timezone='', logfile=''){
  # check parameters
  if (names(dataframe)[1] != 'date'){
    cat("Error: first column must be called 'date'\n")
    return(FALSE)
  }

  if (timezone == ''){
    cat('Error: must specify a timezone\n')
    return(FALSE)
  }

  if ((hour < 0) | (hour > 23)){
    cat('Invalid hour value\n')
    return(FALSE)
  }

  dataframeName <- deparse(substitute(dataframe))

  # convert data
  hourstring <- formatC(hour, width=2, format="d",flag="0")
  hourstring <- paste(' ', hourstring, ':00', sep='')
  dataframe$date <- paste(format(dataframe$date, format='%Y-%m-%d'), hourstring, sep='')
  dataframe$date <- as.POSIXct(dataframe$date, format='%Y-%m-%d %H:%M', tz=timezone)
  names(dataframe)[1] <- 'datetime'

  comment <- paste('dateToDatetime dataframe:', dataframeName, sep='')
  result <- logAction(comment, logfile)
  if (result)
    return (dataframe)
  else
    return(result)
}
