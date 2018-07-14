#' Aggregates a data frame to a shorter time period
#'
#' @description CRHM data (observations and outputs) are generally produced at hourly time steps. This function aggregates CRHM data to daily, monthly or yearly values. The data can be aggregated by their maxima, minima, means, sums and any combination of these statistics.
#' @param CRHMdataframe Required. A valid \pkg{CRHMr} data frame.
#' @param columns The columns to be aggregated, not including the datetime. The default is the first column. This can be a vector, i.e. c(1,2,3).
#' @param period The period of aggregation. Must be one of \option{hourly}, \option{daily}, \option{monthly}, \option{yearly} (or \option{annual}) or \option{hydro}. Default is \option{yearly}. Multiple-hour aggregation is not yet supported.
#' @param funs A character vector containing the function(s) for aggregation. The default is \option{mean}, but can also include \option{min}, \option{max}, \option{sum} and \option{length}. The function(s) will be applied to all of the specified columns
#' @param AggFilename Optional. File name for the aggregated data.
#' @param startMonth Optional. Starting month, to be used when aggregating by hydrological year.
#' @param useSecondYear Optional. Logical. Should the hydrological year be based on the first or second calendar year. In other words would January 1, 2015 be the hydrological year 2014 or 2015? The default is \code{TRUE} (i.e., the hydrological year would be 2015). Note that the Campbell Scientific program SPLIT uses the first calendar year (i.e., the hydrological year would be 2014). To emulate this program, set \code{useSecondYear} to be \code{FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return Returns a data frame with the aggregated values.
#' @note The period of aggregation must be smaller than the time step of the CRHM data. This function does NOT remove \code{NA} values before aggregation. Use \command{na.omit} or one of the infilling functions (\code{\link{interpolate}} or \code{\link{impute}}) if you want to remove missing values.
#' @seealso \code{\link{interpolate}} \code{\link{impute}} \code{\link{yearlyPeaks}} \code{\link{hydroYear}}
#' @author Kevin Shook
#' @examples
#' Badlake.t.monthly <- aggDataframe(BadLake7376, period='monthly',
#' columns=1, funs=c('mean'))
#' Badlake.radiation.daily <- aggDataframe(BadLake7376, period='daily',
#' columns=c(6,7,8), funs=c('mean'))
#' @export

aggDataframe <-
  function(CRHMdataframe, columns=1, period="annual", funs=c("mean"),
             AggFilename="", startMonth=10, useSecondYear=TRUE, logfile="") {
    # aggregates a dataframe of CRHM variables (obs or outputs) to longer time periods
    if (period == "" | nrow(CRHMdataframe) == 0 | (length(columns) == 0)) {
      cat("Error: missing variables\n")
      return(FALSE)
    }
    CRHMname <- deparse(substitute(CRHMdataframe))
    if (length(columns) > 1) {
      selected <- CRHMdataframe[, (columns + 1)]
      selected.names <- names(selected)
    }
    else {
      selected <- data.frame(CRHMdataframe[, (columns + 1)])
      selected.names <- names(CRHMdataframe)[columns + 1]
      names(selected) <- selected.names
    }


    if (is.numeric(period)) {
      # do sub-daily aggregation
      period.hours <- period
      if (period == 1) {
        time.period <- "hour"
        times <- lubridate::ceiling_date(CRHMdataframe$datetime, unit = time.period)
        agg <- data.frame(unique(times))
        names(agg) <- time.period
      }
      else {
        cat("Error: multiple-hour aggregation not yet supported\n")
        return(FALSE)
      }
    }
    else {
      period <- tolower(period)
      if (stringr::str_detect(period, "ann") | stringr::str_detect(period, "year")) {
        time.period <- "year"
        period.hours <- 365 * 24
      }
      else if (stringr::str_detect(period, "da")) {
        time.period <- "day"
        period.hours <- 24
      }
      else if (stringr::str_detect(period, "mo") & !stringr::str_detect(period, "year")) {
        time.period <- "month"
        period.hours <- 30 * 24
      }
      else if (stringr::str_detect(period, "ho")) {
        time.period <- "hour"
        period.hours <- 1
      }
      else if (stringr::str_detect(period, "hy")) {
        time.period <- "hydrologic-year"
        period.hours <- 365 * 24
      }
      else {
        time.period <- "year"
        period.hours <- 365 * 24
      }

      # set up time step for multi-day aggregation
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
      else if (time.period == "hour") {
        times <- lubridate::ceiling_date(CRHMdataframe$datetime, unit = time.period)
        agg <- data.frame(unique(times))
        names(agg) <- time.period
      }
      else {
        times <- lubridate::floor_date(CRHMdataframe$datetime, unit = time.period)
        agg <- data.frame(unique(times))
        names(agg) <- time.period
      }
    }

    # check period vs existing period

    current.period <- timestep.hours(CRHMdataframe$datetime[1], CRHMdataframe$datetime[2])
    if (current.period >= period.hours) {
      cat("Error: cannot aggregate to a shorter time period\n")
      return(FALSE)
    }
    # do aggregation

    if (sum(stringr::str_detect(funs, "max"))) {
      max.vals <- aggregate(selected, by = list(times), FUN = max)
      max.names <- names(max.vals)[-1]
      max.vals <- data.frame(max.vals[, -1])
      max.names <- paste(max.names, ".max", sep = "")
      names(max.vals) <- max.names
      agg <- cbind(agg, max.vals)
    }

    if (sum(stringr::str_detect(funs, "min"))) {
      min.vals <- aggregate(selected, by = list(times), FUN = min)
      min.names <- names(min.vals)[-1]
      min.vals <- data.frame(min.vals[, -1])
      min.names <- paste(min.names, ".min", sep = "")
      names(min.vals) <- min.names
      agg <- cbind(agg, min.vals)
    }

    if (sum(stringr::str_detect(funs, "mean"))) {
      mean.vals <- aggregate(selected, by = list(times), FUN = mean)
      mean.names <- names(mean.vals)[-1]
      mean.vals <- data.frame(mean.vals[, -1])
      mean.names <- paste(mean.names, ".mean", sep = "")
      names(mean.vals) <- mean.names
      agg <- cbind(agg, mean.vals)
    }

    if (sum(stringr::str_detect(funs, "sum"))) {
      sum.vals <- aggregate(selected, by = list(times), FUN = sum)
      sum.names <- names(sum.vals)[-1]
      sum.vals <- data.frame(sum.vals[, -1])
      sum.names <- paste(sum.names, ".sum", sep = "")
      names(sum.vals) <- sum.names
      agg <- cbind(agg, sum.vals)
    }

    if (sum(stringr::str_detect(funs, "length"))) {
      sum.vals <- aggregate(selected, by = list(times), FUN = length)
      sum.names <- names(sum.vals)[-1]
      sum.vals <- data.frame(sum.vals[, -1])
      sum.names <- paste(sum.names, ".length", sep = "")
      names(sum.vals) <- sum.names
      agg <- cbind(agg, sum.vals)
    }

    if (sum(stringr::str_detect(funs, "delta"))) {
      vals <- data.frame(CRHMdataframe$datetime, selected)
      names(vals)[1] <- "datetime"
      # get closest datetime to specified dates/times

      if (time.period == "year" |
         time.period == "month" |
         time.period == "day" |
         time.period == "hydrologic-year") {

        names(agg)[1] <- "date"
        if (time.period == "year") {
          agg$date <- as.Date(paste(agg$date, "-01-01", sep = ""),
          format = "%Y-%m-%d")
        } else if ( time.period == "hydrologic-year") {
          agg$date <- as.Date(paste(agg$date, "-", startMonth, "-01", sep = ""),
                              format = "%Y-%m-%d")
        }

        timezone <- lubridate::tz(CRHMdataframe$datetime)

        agg_datetime <- dateToDatetime(agg, hour = 23, timezone = timezone)
      }

      # now merge
      merged <- merge(agg_datetime, vals, by = "datetime")
      colnums <- ncol(merged) - 1
      for (i in 1:colnums) {
        delta <- diff(merged[, (i + 1)])
        merged[,(i + 1)] <- c(NA_real_, delta)
      }

      delta.names <- names(vals)[-1]
      delta.names <- paste(delta.names, ".delta", sep = "")
      names(merged)[-1] <- delta.names
      agg <- merged
      agg[,1] <- as.numeric(format(agg[,1], format = "%Y"))
      names(agg)[1] <- "year"

    }

    # remove double periods from variable names
    agg.names <- names(agg)
    agg.names <- stringr::str_replace(agg.names, stringr::fixed(".."), ".")
    names(agg) <- agg.names


    # format dates
    if (time.period == "year") {
      agg[, 1] <- as.numeric(format(agg[, 1], format = "%Y"))
    } else if (time.period == "month") {
      agg[, 1] <- format(agg[, 1], format = "%Y-%m")
    } else if (time.period == "day") {
      agg[, 1] <- as.Date(agg[, 1])
      names(agg)[1] <- "date"
    }
    else if (time.period == "hour") {
      names(agg)[1] <- "datetime"
    }


    # write to file, if name provided
    if (AggFilename != "") {
      write.csv(agg, file = AggFilename, row.names = FALSE, col.names = TRUE)
    } else {
      AggFilename <- "NA"
    }

    comment <- paste("aggDataframe dataframe:", CRHMname, " Aggfuns:",
      stringr::str_c(funs, collapse = ","),
      " period:", period, " file:", AggFilename,
      sep = ""
    )
    result <- logAction(comment, logfile)

    if (result) {
      return(agg)
    } else {
      return(result)
    }
  }
