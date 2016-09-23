#' Creates an empty data fame for CRHM obs
#'
#' @description Creates a data fame to hold observations. All values are initially set to \code{NA_real_}. Once the data fame has been created, it may be filled from other data fames by imputation.
#' @param start.date Required. Starting date, in the format \option{Y-m-d}, \option{d/m/Y}, \option{d-m-Y}, \option{d B Y}, \option{d b Y}, \option{d-B-Y}, \option{d-b-Y}, \option{B d, Y}, or \option{b d, Y}, where \option{Y} = year, \option{m} = month number, \option{B} = capitalized month name, \option{b} = lower case month name, \option{d} = day number.
#' @param end.date Required. Ending date,, in the format \option{d/m/Y}, \option{d-m-Y}, \option{d B Y}, \option{d b Y}, \option{d-B-Y}, \option{d-b-Y}, \option{B d, Y}, or \option{b d, Y}, where \option{Y} = year, \option{m} = month number, \option{B} = capitalized month name, \option{b} = lower case month name, \option{d} = day number.
#' @param timestep Optional. Time interval between values in hours. The default value is 1 hour. The interval can be less than 1 hour.
#' @param variables Optional. vector containing the names of variables to be created. The default values are \option{t}, \option{rh}, \option{u}, and \option{p}.
#' @param reps Optional. The number of repetitions for each value. The default is 1.
#' @param timezone Required. The name of the timezone of the data as a character string. This should be the timezone of your data, but omitting daylight savings time. Note that the timezone code is specific to your OS. To avoid problems, you should use a timezone without daylight savings time. Under Linux, you can use \option{CST} and \option{MST} for Central Standard or Mountain Standard time, respectively. Under Windows or OSX, you can use \option{etc/GMT+6} or \option{etc/GMT+7} for Central Standard and Mountain Standard time. DO NOT use \option{America/Regina} as the time zone, as it includes historical changes between standard and daylight savings time.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @details The names for the variables in the data fame are derived from the specified variables, and the number of reps. For example if the default variable names are used, with 2 reps, then the variables created will be \code{t.1}, \code{t.2}, \code{rh.1}, \code{rh.2}, \code{u.1}, \code{u.2}, \code{p.1}, and \code{p.2}.
#' @return If successful, the function returns a CRHM data fame with the specified number of each variable, with values set to be \code{NA}. If unsuccessful, returns the value \code{FALSE}.
#' @author Kevin Shook
#' @seealso  \code{\link{impute}}
#' @examples
#'vars <- c('t','rh','u')
#'stoon <- createObsDataframe('1/1/1960', '31/12/1960', variables=vars, reps=2, timezone='etc/GMT+6')
#' @export


createObsDataframe <-
function(start.date, end.date, timestep=1,
                               variables=c('t','rh','u','p'),
                               reps=1, timezone='', logfile=''){
  # creates an obs data fame
  # all values are initially set to NA

  if (timezone == ''){
    cat('Error: must specify a timezone\n')
    return(FALSE)
  }


  date.formats <- c('Y-m-d', 'd/m/Y','d-m-Y','d B Y','d b Y','d-B-Y','d-b-Y' ,
                    'B d, Y', 'b d, Y')
  start.date.format <- lubridate::guess_formats(start.date, date.formats)[1]
  end.date.format <- lubridate::guess_formats(end.date, date.formats)[1]

  # check timestep
  if ((timestep==24) | (timestep=='daily') | (timestep=='daily')){
    start.date <- as.Date(start.date, format=start.date.format)
    end.date <- as.Date(end.date, format=end.date.format)
    time.seq <- seq(from=start.date, to=end.date, by=1)
  }
  else{
    # create date/fime sequence
    start.time.format <- paste(start.date.format, ' %H:%M', sep='')
    end.time.format <- paste(end.date.format, ' %H:%M', sep='')

    start.time.string <- paste(start.date, ' 01:00', sep='')
    end.time.string <- paste(end.date, ' 23:00', sep='')

    start.time <- as.POSIXct(start.time.string, format=start.time.format, tz=timezone)
    end.time <- as.POSIXct(end.time.string, format=end.time.format, tz=timezone)
    time.seq <- seq(from=start.time, to=end.time, by=(3600*timestep))
  }

  obs.dataframe <- as.data.frame(time.seq)
  names(obs.dataframe) <- 'datetime'
  rows <- nrow(obs.dataframe)

  # create variable names
  variables.names.reps <- rep(variables, each=reps)
  variables.count <- length(variables)
  rep.seq <- seq(1:reps)
  variables.nums.reps <- rep(rep.seq, variables.count)
  variables.names <- paste(variables.names.reps, variables.nums.reps, sep='.')
  cols <- length(variables.names)

  # create values
  values <- matrix(data=NA_real_, nrow=rows, ncol=cols)
  values.dataframe <- as.data.frame(values)
  names(values.dataframe) <- variables.names
  obs.dataframe <- cbind(obs.dataframe, values.dataframe)
  variables.names <- stringr::str_c(variables.names, collapse=',')

  comment <- paste('createObsDataframe variables:', variables.names, sep='')
  result <- logAction(comment, logfile)
  if (result)
    return(obs.dataframe)
  else
    return(result)
}
