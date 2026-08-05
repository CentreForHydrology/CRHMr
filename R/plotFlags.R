#' Helper function to plot identified flags
#'
#' @description This function takes a vector of datetimes identified by other
#'   CRHMr qaqc functions and illustrates these on top of the original data
#'   using GGPLOT. It is also easy to pass this ggplot output onto plotly for
#'   interactive viewing by running plotly::ggplotly() after this function.
#'
#' @param obs Required. A \pkg{CRHMr} obs data frame.
#' @param datetime_flags Required. A vector of datetimes in POSIXct format, for
#'   example the direct output of the findSpikes() function.
#' @param colnum Optional. The number of the column to test for spikes, not
#'   including the \code{datetime}.
#' @param gg_flag_shape Optional. An integer representing the ggplot shape type
#'   for the flagged values.
#' @param gg_flag_colour Optional. A string representing the ggplot colour
#'   for the flagged values.
#' @param gg_flag_size Optional. An integer representing the ggplot shape size
#'   for the flagged values.
#'
#' @return a ggplot object.
#' @author Alex Cebulski
#' @seealso \code{\link{findSpikes}} \code{\link{findGaps}}
#'   \code{\link{findFlatlines}} \code{\link{findSpikesStdevWindow}}
#' @export
#'
#' @examples
#' plotFlags(BadLake7376, c(as.POSIXct("1974-09-11 12:00:00")))
plotFlags <-
  function(obs,
           datetime_flags,
           colnum = 1,
           gg_flag_shape = 4,
           gg_flag_colour = 'red',
           gg_flag_size = 1) {
    if (colnames(obs)[1] != 'datetime') {
      stop("First column of the obs file must be datetime.")
    }

    if (!"POSIXct" %in% class(datetime_flags)) {
      stop("datetime_flags must be of type POSIXct.")
    }

    obs_fltr <- obs[obs$datetime %in% datetime_flags, ]

    gg_ylab <- colnames(obs)[colnum + 1]

    ggplot2::ggplot(obs, aes(datetime, obs[, colnum + 1])) +
      geom_line() +
      geom_point(
        data = obs_fltr,
        aes(x = datetime, y = obs_fltr[, colnum + 1]),
        shape = gg_flag_shape,
        colour = gg_flag_colour
      ) +
      ggplot2::ylab(gg_ylab)
  }
