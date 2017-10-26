#' Plots min, max and mean air temperatures for each day of the year
#'
#' @description Creates \pkg{ggplot2} ribbon plots of the daily minimum, maximum and mean values of air temperatures over a year. The shaded area indicates the range of values in the data frame for each day.
#' @param obs Required. Either a \pkg{CRHMr} \code{obs} data frame or a list of data frames.
#' @param obsNames Optional. A vector containing names for the data frames. If there are more data frames than names, the data will be named \code{obs 1}, \code{obs 2} etc.
#' @param tempCols Optional. Column(s) containing temperatures. Can be a single value or a vector. Values will be recycled if necessary. Default is \code{1}.
#' @param facet Optional. If \code{FALSE} (the default), then all variables will be on a single plot. If \code{TRUE}, then each variable will be on a separate facet.
#' @param facetColour Optional. The colour to be used for shading the ribbon plot in each facet. If not specified (the default), then each plot will be coloured separately.
#' @param facetCols Optional. The number of columns to be used for the facets, if selected.
#'
#' @return If unsuccessful, returns \code{FALSE}. If successful, returns a \pkg{ggplot2} object.
#' @author Kevin Shook
#' @seealso \code{\link{plotTempsByYear}}
#' @export
#'
#' @examples p <- ribbonPlotAirTemps(BadLake7376, tempCols = 1, facet=FALSE)
ribbonPlotAirTemps <- function(obs="", obsNames="", tempCols=1, facet = TRUE,
                               facetColour = "", facetCols = 2) {
  # declare variables for ggplot
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

  numTempCols <- length(tempCols)
  tempCols <- rep.int(tempCols, ceiling(numObs / numTempCols))

  for (i in 1:numObs) {
    if (numObs > 1)
      df <- as.data.frame(obs[[i]])
    else
      df <- obs

    df <- na.omit(df)
    df <- df[,c(1,(tempCols[i]+1))]

    # check interval
    dataInterval <- timestep.hours(df[1,1], df[2,1])
    if (dataInterval >= 24) {
      cat('Error: this function only works on sub-daily temperatures\n')
      return(FALSE)
    }


    # use fake date
    df$fakedate <- as.Date(format(df$datetime, format = "2000-%m-%d"),
                              format = "%Y-%m-%d")
    dataName <- obsNames[i]

    # inst p, so aggregate
    dailymin <- aggregate(df[,2], by = list(df$fakedate), FUN = "min")
    dailymean <- aggregate(df[,2,], by = list(df$fakedate), FUN = "mean")
    dailymax <- aggregate(df[,2], by = list(df$fakedate), FUN = "max")

    daily <- data.frame(dailymin, dailymean[,2], dailymax[,2], dataName)
    names(daily) <- c("date", "min", "mean", "max", "name")

    if (i == 1) {
      # merge into set of fake dates
      all <- daily
    }
    else{
      all <- rbind(all, daily)
    }
  }

  # now plot
  if (!facet) {
    p <-  ggplot2::ggplot(all, ggplot2::aes(x = date, y = mean,
                                                 colour = name)) +
      ggplot2::geom_line() +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = min,
                                                  ymax = max,
                                                  fill = name),
                           alpha = 0.2) +
      ggplot2::scale_x_date(date_labels = "%b %d") +
      ggplot2::theme(legend.title = ggplot2::element_blank())
  }
  else{

    if (facetColour == "") {
      p <-  ggplot2::ggplot(all, ggplot2::aes(x = date, y = mean,
                                              colour = name)) +
        ggplot2::geom_line() +
        ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = min,
                                                    ymax = max,
                                                    fill = name),
                             alpha = 0.4) +
        ggplot2::facet_wrap(~name, scales = 'fixed', ncol = facetCols) +
        ggplot2::scale_x_date(date_labels = "%b") +
        ggplot2::theme(legend.position = "none")
    } else {
      p <-  ggplot2::ggplot(all, ggplot2::aes(x = date, y = mean,
                                              group = name)) +
        ggplot2::geom_line() +
        ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = min,
                                                    ymax = max),
                             alpha = 0.4, fill = facetColour) +
        ggplot2::facet_wrap(~name, scales = 'fixed', ncol = facetCols) +
        ggplot2::scale_x_date(date_labels = "%b") +
        ggplot2::theme(legend.position = "none")
    }

  }

  ylabel <-  expression(paste("Daily air temperature  (", degree, C, ")", sep = ""))
  p <-  p + ggplot2::xlab('')  + ggplot2::ylab(ylabel)

  return(p)
}
