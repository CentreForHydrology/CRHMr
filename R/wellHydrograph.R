#' Plots hydrograph of daily well depths
#'
#' @description Creates a \pkg{ggplot2} object of hydrograph of well depths.
#' @param wellObs Required. \pkg{CRHMr} obs data frame of daily depths.
#' @param depthCol Optional. Column containing depths. Default is 1.
#' @param fakeDates Optional. If set to \code{TRUE} then all dates have their year replaced with \code{2000}, and the actual year is added as a variable in the plotted data. This allows the plot to be faceted by year, as shown in the examples.
#' @param hydroYear Optional. If \code{TRUE}, the depths are plotted against the hydrological year.
#' @param startMonth Optional. Sets the first month of the hydrological year (if required).
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @return If successful, returns a \pkg{ggplot2} object. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook

#' @export
#'
#' @examples
#' \dontrun{
#' p <- wellHydrograph(well, fakeDates = FALSE)
#' #' # re-plot with fake dates
#' p <- wellHydrograph(well, fakeDates = TRUE, hydroYear = TRUE)
#' p <- p + ggplot2::facet_wrap(~year)
#' p <- p + ggplot2::scale_x_date(date_breaks = "3 months",
#' date_labels = "%b")
#' }
wellHydrograph <- function(wellObs, depthCol = 1,
                           fakeDates = FALSE,
                           hydroYear = TRUE,
                           startMonth = 10,
                           quiet = TRUE){
  p <- NULL
  depth <- NULL
  wellObs <- wellObs[, c(1, (depthCol + 1))]


  if (nrow(wellObs) < 1) {
    if (!quiet) {
      cat("Error: no data to plot")
    }
    return(FALSE)
  }


  # create fake dates if req'd
  if (hydroYear) {
    if (fakeDates) {
      fakeyear <- 2000  # use leap year, just in case
      wellObs$year <- as.numeric(format(wellObs$date,
                                        format = "%Y"))
      wellObs$date <- fakeDateHydroyear(wellObs$date,
                                        fakeYear = fakeyear,
                                        startMonth = startMonth)


    }

  } else {
    if (fakeDates) {
      fakeyear <- 2000  # use leap year, just in case
      wellObs$year <- as.numeric(format(wellObs$date,
                                        format = "%Y"))
      wellObs$date <- fakeDate(wellObs$date, fakeyear)

    }
  }

  # create plot
  p <- ggplot2::ggplot(wellObs, ggplot2::aes(date, depth)) +
    ggplot2::geom_line() +
    ggplot2::xlab("") +
    ggplot2::ylab("Depth to water (m)") +
    ggplot2::scale_y_reverse()

  return(p)


}
