#'  Creates a \pkg{ggplot2} object of cumulative precips by year
#' @description Calculates cumulative precipitations and creates a \pkg{ggplot2} faceted by year. Note that this function would work for any other cumulative variable, if the Y-axis label is changed.
#' @param obs Required. Either a \pkg{CRHMr} \code{obs} data frame or a list of data frames.
#' @param obsNames Optional. A vector containing names for the data frames. If there are more data frames than names, the data will be named \code{obs 1}, \code{obs 2} etc.
#' @param precipCols Optional. Column(s) containing precipitation. Can be a single value or a vector. Values will be recycled if necessary. Default is \code{1}.
#' @param commonDates Optional. If \code{TRUE} (the default) then the range of dates will be restricted to the common first and last dates of the datasets.
#'
#' @return If successful, returns a faceted \pkg{ggplot2} object of cumulative precipitation vs time. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook.
#' @seealso \code{\link{plotTempsByYear}}
#' @export
#'
#' @examples \dontrun{ l <- list(meas, WRF)
#' names <- c("Measured", "WRF")
#' p <- plotPrecipsByYear(l, names)
#' library(ggplot2)
#' p <- p + scale_colour_manual(values=c("red", "blue")}
plotPrecipsByYear <- function(obs="", obsNames="", precipCols=1,
                            commonDates = TRUE) {

  # declare variables for ggplot
  name <- NULL
  precip <- NULL
  fakedate <- NULL
  cumMeasured <- NULL

  # check parameters
  if (!is.list(obs)) {
    cat('Error: missing obs values\n')
    return(FALSE)
  }

  if (!is.data.frame(obs))
    numObs <- length(obs)
  else
    numObs <- 1

  if (numObs > length(obsNames)) {
    nameNum <- seq(1, numObs)
    obsNames <- paste("obs ", nameNum, sep = "")
  }

  numPrecipCols <- length(precipCols)
  precipCols <- rep.int(precipCols, ceiling(numObs / numPrecipCols))

  for (i in 1:numObs) {
    if (numObs > 1)
      df <- as.data.frame(obs[[i]])
    else
      df <- obs

    df <- na.omit(df)

    # see if we have to aggregate, or not
    dataInterval <- timestep.hours(df[1,1], df[2,1])
    if (dataInterval < 24) {
      # inst p, so aggregate
      daily <- CRHMr::aggDataframe(df, columns = precipCols[i],
                                 period = "daily", funs = "sum")
    } else
      daily <- df

    names(daily) <- c("date", "precip")

    daily$year <- lubridate::year(daily$date)

    # use fake date
    daily$fakedate <- as.Date(format(daily$date, format = "2000-%m-%d"),
                         format = "%Y-%m-%d")
    daily$name <- obsNames[i]
    minDate <- min(daily$date)
    maxDate <- max(daily$date)

    if (i == 1) {
      all <- daily
      maxMinDate <- minDate
      minMaxDate <- maxDate
    }
    else{
      all <- rbind(all, daily)
      maxMinDate <- max(minDate, maxMinDate)
      minMaxDate <- min(maxDate, minMaxDate)
    }
  }

  # restrict plot to common dates

  if (commonDates) {
    all <- all[(all$date >= maxMinDate) & (all$date <= minMaxDate), ]
  }

cumu <- plyr::ddply(all,c("year", "name"), transform,
                    cumMeasured = cumsum(precip))

  p <- ggplot2::ggplot(cumu, ggplot2::aes(fakedate, cumMeasured, colour = name)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~year) +
    ggplot2::xlab("") +
    ggplot2::scale_x_date(date_labels = "%b",
                date_breaks = "2 months") +
    ggplot2::ylab("Cumulative precipitation (mm)") +
    ggplot2::theme_gray(12)

    if (numObs > 1)
      p <- p + ggplot2::theme(legend.title = ggplot2::element_blank(),
                     legend.position = c(0.7, 0.12))
    else
      p <- p + ggplot2::theme(legend.position = "none")

  return(p)
}


