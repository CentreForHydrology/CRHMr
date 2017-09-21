#'  Creates a \pkg{ggplot2} object of daily mean air temps by year
#' @description Aggregates air temeratures to mean daily values and creates a \pkg{ggplot2} faceted by year. Note that this function would work for any other non-cumulative variable, if the Y-axis label is changed.
#' @param obs Required. Either a \pkg{CRHMr} \code{obs} data frame or a list of data frames.
#' @param obsNames Optional. A vector containing names for the data frames. If there are more data frames than names, the data will be named \code{obs 1}, \code{obs 2} etc.
#' @param tempCols Optional. Column(s) containing air temperatures. Can be a single value or a scalar. Values will be recycled if necessary.
#' @param commonYears Optional. If \code{TRUE} (the default) then the range of years will be restricted to the common first and last years.
#'
#' @return If successful, returns a faceted \pkg{ggplot2} object of mean daily air temperatures vs time. If unsuccessful, returns \code{FALSE}.
#' @export
#'
#' @examples \dontrun{ l <- list(meas, WRF)
#' names <- c("Measured", "WRF")
#' p <- plotTempsByYear(l, names)
#' library(ggplot2)
#' p <- p + scale_colour_manual(values=c("red", "blue")}
plotTempsByYear <- function(obs="", obsNames="", tempCols=1,
                            commonYears = TRUE) {

  # declare variables for ggplot
  temp <- NULL
  date <- NULL
  name <- NULL

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

  tempCols <- rep.int(tempCols, ceiling(numObs / tempCols))

  for (i in 1:numObs) {
    if (numObs > 1)
      df <- as.data.frame(obs[[i]])
    else
      df <- obs

    daily <- CRHMr::aggDataframe(df, columns = c(1, tempCols[i]),
                                 period = "daily", funs = "mean")
    names(daily)[2] <- "temp"
    daily$year <- lubridate::year(daily$date)

    # use fake date
    daily$date <- as.Date(format(daily$date, format = "2000-%m-%d"),
                         format = "%Y-%m-%d")
    daily$name <- obsNames[i]
    minYear <- min(daily$year)
    maxYear <- max(daily$year)

    if (i == 1) {
      all <- daily
      maxMinYear <- minYear
      minMaxYear <- maxYear
    }
    else{
      all <- rbind(all, daily)
      maxMinYear <- max(minYear, maxMinYear)
      minMaxYear <- min(maxYear, minMaxYear)
    }
  }

  # restrict plot to common years

  if (commonYears) {
    all <- all[(all$year >= maxMinYear) & (all$year <= minMaxYear), ]
  }

  p <- ggplot2::ggplot(all, ggplot2::aes(date, temp, colour = name)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~year) +
    ggplot2::xlab("") +
    ggplot2::scale_x_date(date_labels = "%b",
                date_breaks = "2 months") +
    ggplot2::ylab("Mean daily air temperature (C)") +
    ggplot2::theme_gray(12)

    if (numObs > 1)
      p <- p + ggplot2::theme(legend.title = ggplot2::element_blank(),
                     legend.position = c(0.7, 0.12))
    else
      p <- p + ggplot2::theme(legend.position = "none")

  return(p)
}


