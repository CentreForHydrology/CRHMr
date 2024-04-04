# Miscellaneous utilities


#' @title Gets the Windows end of line characters
#'
#' @description Finds the end of line (eol) characters required for writing Windows files, such as CRHM obs files. No parameters are required. This is an internal \pkg{CRHMr} function and should \emph{never} need to be called directly.
#' @return Returns the Windows end of line characters (cr and lf).
#' @importFrom stringr str_detect fixed
#' @export
#' @keywords internal
#' @author Kevin Shook
#' @note This function is used to make the creation of Windows-specific files work on all platforms. CRHM requires its obs and project files to use the Windows end of line characters, which are expressed differently on UNIX-based operating systems such as Linux and OSX.
#' @seealso  \code{\link{automatePrj}} \code{\link{setPrjDates}} \code{\link{writeObsFile}} \code{\link{runCRHM}}
#' @examples
#' windowsEndOfLine <- win.eol()
#'
win.eol <- function(){
  # set line end characters for all OS
  if (str_detect(.Platform$OS.type, fixed('win',ignore_case = TRUE)))
    eol <- '\n'
  else
    eol <- '\r\n'

  return(eol)
}


#' @title Converts air temperature and vapour pressure to relative humidity
#'
#' @description This is an internal \pkg{CRHMr} function and should \emph{never} need to be called directly, as it does \emph{NO} data checking. Use the function \code{changeEatoRH} instead.
#' @param airtemp Required. Air temperature in \eqn{^\circ}{ }C.
#' @param vapourPressure Required. Vapour pressure in kPa.
#'
#' @return Returns RH in percent.
#' @author Kevin Shook
#' @seealso  \code{\link{changeEatoRH}}
#' @keywords internal
#' @export
#'
#' @examples
#' vp2rh(-5, 0.2007355)
vp2rh <- function(airtemp, vapourPressure){
  # converts air temp (C) and vapour pressure (kPa) to rh (percent)
  estar <- rh2vp(airtemp, 100)
  rh <- 100 * (vapourPressure/estar)

  # set limits
  rh <- pmax(rh, 0)
  rh <- pmin(rh, 100)
  return(rh)
}

#' @title Converts air temperature and relative humidity to vapour pressure
#'
#' @description This is an internal \pkg{CRHMr} function and should \emph{never} need to be called directly, as it does \emph{NO} data checking. Use the function \code{changeRHtoEa} instead.
#' @param airtemp Required. Air temperature in \eqn{^\circ}{ }C.
#' @param rh Required. Relative humidity in percent.
#'
#' @return Returns vapour pressure in kPa. If the air temperatures are scalar, returns a scalar value. If the air temperatures are a vector, returns a vector.
#' @author Kevin Shook
#' @seealso  \code{\link{changeRHtoEa}}
#' @keywords internal
#' @export
#'
#' @examples
#' rh2vp(-5, 50)
rh2vp <- function(airtemp, rh){
  if (length(airtemp) <= 1) {
    # do scalar calculation
    estar <- saturatedVP(airtemp)
    vp <- (estar * rh) / 100
    vp <- vp[1]
  }
  else {
    # do values for t > 0
    estar <- saturatedVP(airtemp)
    vp <- (estar * rh) / 100
  }
  # set limits
  vp <- pmax(vp, 0)
  return(vp)
}

#' @title Water vapour density (ASHRAE,1993)[gm-3]
#' @param mw Unknown
#' @param RHi Required. RH as a percent.
#' @param Tai Required. Air temp in C.
#' @param Ru Unknown
#'
#' @return Returns water vapour density
#' @keywords internal
#' @export
#'
phiv <- function(mw, RHi, Tai, Ru){
  val <- mw*(RHi / 100*0.611 * exp((17.3*Tai) / (237.3 + Tai))) / (Ru*(Tai + 273.15))/1000
  return(val)
}

#'Saturated water vapour density (ASHRAE,1993)[gm-3]
#'
#' @param mw Unknown
#' @param Tai Required. Air temperature
#' @param Ru Unknown
#'
#' @return Returns saturated water vapour density
#' @keywords internal
#' @export
#'
phivt <- function(mw,Tai,Ru){
  val <- mw*(0.611*exp((17.3*Tai)/(237.3 + Tai)))/(Ru*(Tai + 273.15))/1000
  return(val)
}


#' Converts a list of vectors to a data frame.
#'
#' @description This function will convert a list of vectors to a data frame. This function
#' will handle three different types of lists of vectors. First, if all the elements
#' in the list are named vectors, the resulting data frame will have have a number
#' of columns equal to the number of unique names across all vectors. In cases
#' where some vectors do not have names in other vectors, those values will be
#' filled with \code{NA}.
#'
#' The second case is when all the vectors are of the same length. In this case,
#' the resulting data frame is equivalent to applying \code{rbind} across all elements.
#'
#' The third case handled is when there are varying vector lengths and not all the
#' vectors are named. This condition should be avoided. However, the function will
#' attempt to convert this list to a data frame. The resulting data frame will have
#' a number of columns equal to the length of the longest vector. For vectors with
#' length less than this will fill the row with \code{NA}s. Note that this function
#' will print a warning if this condition occurs.
#'
#' @author Jason Bryer <<jason@@bryer.org>>
#' @references \url{http://stackoverflow.com/questions/4227223/r-list-to-data-frame}
#' @param x a list to convert to a data frame.
#' @param row.names a vector equal to \code{length(x)} corresponding to the row names.
#' If \code{NULL}, the row names will be set to \code{names(x)}.
#' @param optional not used.
#' @param ... other parameters passed to \code{\link{data.frame}}.
#' @return a data frame.
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' test1 <- list( c(a='a',b='b',c='c'), c(a='d',b='e',c='f'))
#' as.data.frame(test1)
#'
#' test2 <- list( c('a','b','c'), c(a='d',b='e',c='f'))
#' as.data.frame(test2)
#'
#' test3 <- list('Row1'=c(a='a',b='b',c='c'), 'Row2'=c(var1='d',var2='e',var3='f'))
#' as.data.frame(test3)
#'
#' test4 <- list('Row1'=letters[1:5], 'Row2'=letters[1:7], 'Row3'=letters[8:14])
#' as.data.frame(test4)
#'
#' test5 <- list(letters[1:10], letters[11:20])
#' as.data.frame(test5)
#'
#' test6 <- list(list(letters), letters)
#' as.data.frame(test6)}
#' @export
list2df <- function(x, row.names=NULL, optional=FALSE, ...) {

    if(!all(unlist(lapply(x, class)) %in%
            c('raw','character','complex','numeric','integer','logical'))) {
      warning('All elements of the list must be a vector.')
      NextMethod(x, row.names=row.names, optional=optional, ...)
    }
    allequal <- all(unlist(lapply(x, length)) == length(x[[1]]))
    havenames <- all(unlist(lapply(x, FUN=function(x) !is.null(names(x)))))
    if(havenames) { #All the vectors in the list have names we can use
      colnames <- unique(unlist(lapply(x, names)))
      df <- data.frame(matrix(
        unlist(lapply(x, FUN=function(x) { x[colnames] })),
        nrow=length(x), byrow=TRUE))
      names(df) <- colnames
    } else if(allequal) { #No names, but are of the same length
      df <- data.frame(matrix(unlist(x), nrow=length(x), byrow=TRUE), ...)
      hasnames <- which(unlist(lapply(x, FUN=function(x) !is.null(names(x)))))
      if(length(hasnames) > 0) { #We'll use the first element that has names
        names(df) <- names(x[[ hasnames[1] ]])
      }
    } else { #No names and different lengths, we'll make our best guess here!
      # warning(paste("The length of vectors are not the same and do not ",
      #               "are not named, the results may not be correct.", sep=''))
      #Find the largest
      lsizes <- unlist(lapply(x, length))
      start <- which(lsizes == max(lsizes))[1]
      df <- x[[start]]
      for(i in (1:length(x))[-start]) {
        y <- x[[i]]
        if(length(y) < length(x[[start]])) {
          y <- c(y, rep(NA, length(x[[start]]) - length(y)))
        }
        if(i < start) {
          df <- rbind(y, df)
        } else {
          df <- rbind(df, y)
        }
      }
      df <- as.data.frame(df, row.names=1:length(x))
      names(df) <- paste('Col', 1:ncol(df), sep='')
    }
    if(missing(row.names)) {
      row.names(df) <- names(x)
    } else {
      row.names(df) <- row.names
    }
    return(df)
  }

is.leapyear <- function(year){
  #http://en.wikipedia.org/wiki/Leap_year
  #http://quantitative-ecology.blogspot.ca/2009/10/leap-years.html
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

buck <- function(RH, Ta){
  buck_RH <- abs(100*((RH/100)*0.61121*exp((17.502*Ta)/(240.97 + Ta)))/(-0.61115*exp((22.452*Ta)/(272.55+Ta))))
  return(buck_RH)
}

#Diffusivity of water vapour in air [m2 s-1]
Dv <- function(Tai){
  Dv <- 2.06*10^-5*((Tai+273.15)/273.15)^1.75
  return(Dv)
}

f2c <- function(t.f){
  # converts air temps from F to C
  t.c <- (t.f - 32) / 1.8
  return(t.c)
}

#Newton-Raphston Iteration Function
ff <- function(Tai, Ti1, Li, mw, Ru, RHi){
  val <- -Ti1+Tai-(Li*Dv(Tai)/Ka(Tai))*(phivt(mw,Ti1,Ru) - phiv(mw,RHi,Tai,Ru))
  return(val)
}

#Newton-Raphston Iteration Function
fp <- function(Tai, Ti1, Li, mw, Ru, RHi){
  T1 <- Ti1 + 0.001*Ti1
  T2 <- Ti1 - 0.001*Ti1
  fp_val <- (ff(Tai,T1, Li, mw, Ru, RHi)-ff(Tai,T2, Li, mw, Ru, RHi))/(0.002*Ti1)
  return(fp_val)
}

#Thermal conductivity of air [J m-1 s-1 K-1]
Ka <- function(Tai){
  Ka <- 0.000063*(Tai+273.15)+0.00673
}

Ti_calc <- function(Tai, Ti1, Li, mw, Ru, RHi){
  Ti2 <- Ti1 - ff(Tai,Ti1, Li, mw, Ru, RHi) / fp(Tai,Ti1, Li, mw, Ru, RHi)
  return(Ti2)
}

#' Returns fake date using specified year
#'
#' @param dates Required. Dates to be converted
#' @param fakeYear Required. Year to be used. Default is 2000.
#'
#' @return Returns vector of fake dates.
#' @export
#' @keywords internal
#' @examples
#' fakeDate(date())
fakeDate <- function(dates, fakeYear=2000){
  fake_dates <- format(dates, format = '%m-%d')
  fake_dates <- paste(fakeYear,'-',fake_dates, sep = '')
  fake_dates <- as.Date(fake_dates, format = '%Y-%m-%d')
  return(fake_dates)
}

#' Calculates fake datetime
#' @name fakeDatetime
#' @param datetime Required. Datetime to be converted
#' @param fakeYear Required. Base year
#'
#' @return Returns vector of fake datetimes
#' @export
#' @keywords internal
#'
fakeDatetime <- function(datetime, fakeYear=2000){
  fake_datetime <- format(datetime, format='%m-%d %H:%S')
  fake_datetime <- paste(fakeYear, '-', fake_datetime, sep='')
  fake_datetime <- as.POSIXct(fake_datetime, format='%Y-%m-%d %H:%M', tzone='')
  return(fake_datetime)
}


#' Calculates fake datetime for hydrological year
#' @name fakeDatetimeHydroyear
#' @param datetime Required. Datetimes to be converted.
#' @param fakeYear Required. Base year.
#' @param startMonth Required. Start month for hydrological year.
#'
#' @return Returns vector of fake datetimes
#' @export
#' @keywords internal
#'

fakeDatetimeHydroyear <- function(datetime, fakeYear=2000, startMonth=10){
  fakeYear1 <-  fakeYear-1
  fakeYear2 <- fakeYear
  month <- as.numeric(format(datetime, format='%m'))

  hYear <- rep(fakeYear1, length(datetime))
  hYear[month < startMonth] <- fakeYear2

  fake_datetime <- format(datetime, format='%m-%d %H:%S')
  fake_datetime <- paste(hYear, '-', fake_datetime, sep='')
  fake_datetime <- as.POSIXct(fake_datetime, format='%Y-%m-%d %H:%M', tzone='')
  return(fake_datetime)
}


#' Calculates fake date for hydrological year
#'
#' @param dates Required. Dates to be converted/
#' @param fakeYear Required. Base year.
#' @param startMonth Required. Start month for hydrological year.
#'
#' @return Returns vector of fake dates
#' @keywords internal
#' @export
#'
fakeDateHydroyear <- function(dates, fakeYear=2000, startMonth=10) {
  fakeYear1 <- fakeYear - 1
  fakeYear2 <- fakeYear
  month <- as.numeric(format(dates, format = "%m"))

  hYear <- rep(fakeYear1, length(dates))
  hYear[month < startMonth] <- fakeYear2

  fake_dates <- format(dates, format = "%m-%d")
  fake_dates <- paste(hYear, "-", fake_dates, sep = "")
  fake_dates <- as.Date(fake_dates, format = "%Y-%m-%d")
  return(fake_dates)
}

#' Converts dew point temperature and ambient temperature to RH
#'
#' @description This is an internal \pkg{CRHMr} function and should normally not need to be called directly. Note that it does \emph{NO} data checking.
#' @param t Required. Air temperature in \eqn{^\circ}{ }C.
#' @param td Required. Dew point temperature in \eqn{^\circ}{ }C.
#'
#' @return Returns the RH as a percentage.
#' @author Kevin Shook
#' @references \cite{Lawrence, M. 2005. The relationship between relative humidity and
#' the dewpoint temperature in moist air. American Meteorological Society:
#' 225-233. \url{http://andrew.rsmas.miami.edu/bmcnoldy/Humidity.htm}}
#' @keywords internal
#' @export
#'
#' @examples convertDPtoRH(10, 9)
convertDPtoRH <- function(t, td){

  A <- 17.625
  B <- 243.04 #C

  RH <- 100*(exp((A*td)/(B+td))/exp((A*t)/(B+t)))

  return(RH)
}


CRHMtoZoo <-  function(CRHMdata){
    # converts CRHM data to zoo object
    # used by several functions

    cols <- ncol(CRHMdata)
    datetime <- CRHMdata[,1]
    vars <- CRHMdata[,-1]
    zoo.object <- zoo::zoo(vars, order.by = datetime)
}

#' Cumulative sums for a data frame
#' @description Finds the cumulative sum of all columns in a data frame. Note that all columns must be numeric - date and
#' datetime variables cannot be summed.
#' @param df Required. Data frame to be summed.
#'
#' @return Returns a dataframe containing the cumulative sums of all columns in the original data frame.
#' @author Kevin Shook
#' @note This function is used by other \pkg{CRHMr} functions, so it does \emph{NO} parameter testing.
#' @seealso \code{\link{cumulDailyWater}}
#' @keywords internal
#' @export
#'
#' @examples \dontrun{
#' cumul <- cumsumDataframe(modelOutput)}
cumsumDataframe <- function(df) {
  cols <- ncol(df)
  col_names <- names(df)

  for (col in 1:cols) {
    col_cumsum <- cumsum(df[, col])

    if (col == 1) {
      all_sums <- data.frame(col_cumsum)
    } else {
      all_sums <- cbind(all_sums, col_cumsum)
    }
  }
  names(all_sums) <- col_names
  return(all_sums)
}

#' Calculates atmospheric emissivity
#' @description Calculates atmospheric emissivity from specified longwave radiation and air
#' temperatures. This is an internal \pkg{CRHMr} function and should \emph{never} need
#' to be called directly, as it does \emph{NO} data checking.
#' @param longwave Required. Longwave radiation in W/m\eqn{^2}{^2}.
#' @param tK Required. Air temperatures in K.
#'
#' @return Returns the atmospheric emissivity.
#' @export
#' @author Kevin Shook
#' @seealso \code{\link{distributeQli}} \code{\link{longwave}}
#' @keywords internal
#'
#' @examples
#' emissivity(100, 293)
emissivity <- function(longwave, tK){
  stefanBoltzmann <- 5.670367e-8 #W m-2 K-4
  e <- longwave / (stefanBoltzmann * tK ^ 4)
  return(e)
}


#' Writes a comment to the \pkg{CRHMr} log file
#'
#' @description This is an internal \pkg{CRHMr} function and should normally not need to be called directly. This function is called by almost all \pkg{CRHMr} functions, and writes the comment, and the date and time to the logfile.
#' @param comment Required. Comment string to be written. This normally includes the function name, as well as the obs file being processed.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used. If not specified, then 'CRHMr.log' in the current working directory will be used.
#'
#' @return If successful, returns \code{TRUE}. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @keywords internal
#' @export
#' @examples\dontrun{
#'  comment <- paste('tMinMaxToHourly dataframe:', obsName, sep='')
#'  result <- logAction(comment)}
logAction <- function(comment = "", logfile = "") {
  eol.val <- win.eol()
  # writes comments to a log file
  date.time.formatted <- format(Sys.time(), format = "%Y-%m-%d_%H:%M:%S")

  if (logfile == "") {
    logfile <- "CRHMr.log"
  }

  if (comment == "") {
    cat("Missing comments")
    return(FALSE)
  }

  cat(date.time.formatted, comment, eol.val, sep = " ", file = logfile, append = TRUE)
  return(TRUE)
}

#' Calculates incoming longwave radiation
#' @description Calculates the incoming longwave radiation from the specified emissivity and air temperatures.
#' This is an internal \pkg{CRHMr} function and should \emph{never} need to be called directly, as it does \emph{NO} data checking.
#' @param em Required. Emissivity (0-1).
#' @param tK Required. Air temperatures in K.
#'
#' @return Returns the longwave radiation in W/m\eqn{^2}{^2}.
#' @export
#' @author Kevin Shook
#' @seealso \code{\link{distributeQli}} \code{\link{emissivity}}
#' @keywords internal
#' @examples
#' longwave(0.2, 293)
longwave <- function(em, tK){
  stefanBoltzmann <- 5.670367e-8 #W m-2 K-4
  lw <- (stefanBoltzmann * em * tK ^ 4)
  return(lw)
}

#' Finds cumulative sum of non-missing values in an array
#'
#' @param x Required. An array or vector of numeric values
#'
#' @return Returns an array or vector with cumulative sums of each column.
#' @author Kevin Shook. The code is copied from a MATLAB program nancumsum.m written by Alan Barr.
#' @export
#' @seealso  \code{\link{PcpFiltPosTh}}
#' @keywords internal
#' @examples
#' a <- nancumsum(BadLake7376[,2])
nancumsum <- function(x){
  # check to see if an array or vector
  if (is.vector(x)){
    vectorlength <- length(x)
    cs <- rep.int(NA_real_, vectorlength)
    iYaN <- which(!is.na(x))
    cs[iYaN] <- cumsum(x[iYaN])
  }
  else{
    nt <- dim(x)[1]
    nc <- dim(x)[2]
    cs <- matrix(NA_real_, nt, nc) ()

    for (ic in 1:nc) {
      iYaN <- which(!is.na(x[,ic]))
      cs[iYaN,ic] <- cumsum(x[iYaN,ic])
    }

  }
  return(cs)
}



#' Finds time interval between two datetime values in hours
#'
#' @param datetime1 Required. First datetime
#' @param datetime2 Required. Second datetime
#'
#' @return Returns a numeric value for the difference in hours
#' @export
#' @import tibble
#' @author Kevin Shook Alex Cebulski
#' @examples {
#' dt1 <- as.POSIXct("2000-01-01 12:00", format = "%Y-%m-%d %H:%M")
#' dt2 <- as.POSIXct("2000-01-01 15:00", format = "%Y-%m-%d %H:%M")
#' timestep.hours(dt1, dt2)}
#'
#'
timestep.hours <- function(datetime1, datetime2){
  if(is_tibble(datetime1) | is_tibble(datetime2)) {
    stop("This function does not work with tibbles, please convert to data frame object.")
  }
  dt  <- difftime(datetime1, datetime2, units='hours')
  dt <- abs(as.numeric(dt))
  return(dt)
}
