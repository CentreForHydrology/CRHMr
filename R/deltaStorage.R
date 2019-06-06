#' Calculates changes in storage over specified intervals
#'
#' @param CRHMdataframe Required. A standard CRHM data frame.
#' @param columns Required. The columns to be differenced, not including the
#' datetime. The default is the first column. The columns can be a single
#' number or a vector, i.e. c(1,2,3).
#' @param period Required. The period of aggregation. Must be one of \option{hourly},
#' \option{daily}, \option{monthly}, \option{yearly} (or \option{annual}) or
#' \option{hydro}. Default is \option{yearly}.
#' @param startMonth  Optional. Starting month, to be used when differencing by
#' hydrological year. Default is 10 (October).
#' @param useSecondYear Optional. Logical. Should the hydrological year be based on
#' the first or second calendar year. In other words would January 1, 2015 be the
#'  hydrological year 2014 or 2015? The default is \code{TRUE} (i.e., the
#'  hydrological year would be 2015).
#' @param logfile Optional. Name of the file to be used for logging the action.
#' Normally not used.
#' @importFrom stringr str_detect
#' @importFrom lubridate floor_date
#' @export
#' @note The period of diferencing must be greater than the time step of the
#' CRHM data. This function does NOT remove \code{NA} values before aggregation.
#' Use \command{na.omit} or one of the infilling functions (\code{\link{interpolate}} or
#' \code{\link{impute}}) if you want to remove missing values.
#' @seealso \code{\link{simpleDailyWater}}
#' @author Kevin Shook
#' @examples \dontrun{delta <- deltaStorage(crhm, c(23,24,25))}

deltaStorage <- function(CRHMdataframe = NULL, columns = 1, period = "yearly",
                         startMonth=10, useSecondYear=TRUE, logfile = '') {

  # check parameters

  if (is.null(CRHMdataframe)) {
    stop("Missing CRHM data frame")
  }

  if (period == "" | nrow(CRHMdataframe) == 0 | (length(columns) == 0)) {
    stop("Missing variables\n")
  }
  CRHMname <- deparse(substitute(CRHMdataframe))
  if (length(columns) > 1) {
    selected <- CRHMdataframe[, c(1, (columns + 1))]
    selected.names <- names(selected)
  }
  else {
    selected <- data.frame(CRHMdataframe[, c(1, (columns + 1))])
    selected.names <- names(CRHMdataframe)[c(1, (columns + 1))]
    names(selected) <- selected.names
  }

  period <- tolower(period)
  if (str_detect(period, "ann") | str_detect(period, "year")) {
    time.period <- "year"
    period.hours <- 365 * 24
  }
  else if (str_detect(period, "da")) {
    time.period <- "day"
    period.hours <- 24
  }
  else if (str_detect(period, "mo") & !str_detect(period, "year")) {
    time.period <- "month"
    period.hours <- 30 * 24
  }
  else if (str_detect(period, "ho")) {
    time.period <- "hour"
    period.hours <- 1
  }
  else if (str_detect(period, "hy")) {
    time.period <- "hydrologic-year"
    period.hours <- 365 * 24
  }
  else {
    time.period <- "year"
    period.hours <- 365 * 24
  }

  # set up time step for diff
  if (time.period == "hydrologic-year") {
    hYear <- hydroYear(CRHMdataframe, startMonth, useSecondYear)
    times <- hYear
    agg <- data.frame(unique(hYear))
    if (useSecondYear) {
      names(agg) <- "hydrological_year_second"
    } else {
      names(agg) <- "hydrological_year_first"
    }
  }
  else {
    times <- floor_date(CRHMdataframe$datetime, unit = time.period)
    agg <- data.frame(unique(times))
    names(agg) <- time.period
  }

# check period vs existing period

current.period <- timestep.hours(CRHMdataframe$datetime[1], CRHMdataframe$datetime[2])
if (current.period >= period.hours) {
  stop("Cannot aggregate to a shorter time period")
}

# get interval values
max.vals <- aggregate(selected, by = list(times), FUN = "max")
locs <- match(max.vals$datetime, selected$datetime)
interval_vals <- selected[locs, ]

# now get delta values
deltas <- apply( interval_vals[-1] , 2 , diff )
deltas <- as.data.frame(deltas)
deltas <- cbind(interval_vals$datetime[-1], deltas)
names(deltas)[1] <- "date"
deltas$date <- as.Date(deltas$date)

#add "delta" to variable names
names(deltas)[-1] <- paste(names(deltas)[-1], "_delta", sep = "")

comment <- paste("deltaStorage CRHMfile:",
                 CRHMname,
                 "period:",
                 period,
                 "startMonth:",
                 startMonth,
                 "useSecondYear:",
                 useSecondYear, sep = '')
result <- logAction(comment, logfile)
if (result)
  return(deltas)
else
  return(result)

}
