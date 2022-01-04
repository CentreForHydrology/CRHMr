#' Returns mean monthly precipitation totals
#'
#' @description Calculates mean total precipitation for each calendar month in the observation
#' data frame. Optionally plots the values. Can work with \code{p} and/or \code{ppt} values.
#' @param obs Required. A \pkg{CRHMr} data frame containing \code{p} and/or \code{ppt} values.
#' @param precipCols Optional. A vector containing the columns to be analyzed, not including the
#' \code{datetime} column. If not specified, all precipitation columns are used.
#' @param plot Optional. If \code{TRUE}, which is the default, a \pkg{ggplot2} object of the
#' monthly mean precipitations is returned. If \code{FALSE}, the values are returned as a data frame.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If successful, returns either a data frame or a \pkg{ggplot2} object of the monthly
#' mean precipitations. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @seealso \code{\link{aggDataframe}} \code{\link{monthlyQQplot}}
#' @import ggplot2
#' @importFrom stringr str_detect fixed str_c
#' @importFrom reshape2 melt
#' @export
#'
#' @examples
#' \dontrun{
#' marmot_monthly_plot <- monthlyPrecipTotals(marmot)
#' marmot_monthly_means <- monthlyPrecipTotals(marmot, plot = FALSE)
#' }
monthlyPrecipTotals <- function(obs, precipCols = 0, plot = TRUE, logfile = "") {
  monthnames <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
    "Aug", "Sep", "Oct", "Nov", "Dec"
  )

  # suppress checking of data frame variables used by ggplot2
  month <- NULL
  value <- NULL



  # check parameters
  if (nrow(obs) == 0) {
    stop("Missing obs data frame")
  }
  obsName <- deparse(substitute(obs))

  # get precip vars - use entire data frame unless instructed otherwise
  if (precipCols != 0) {
    precips <- obs[, c(1, (precipCols + 1))]
  }
  else {
    precips <- obs
    precipCols <- seq(2, (ncol(obs) - 1))
  }

  precips_names <- names(precips)

  # separate p and ppt vals
  ppt_cols <- which(str_detect(
    precips_names,
    fixed("ppt.", ignore_case = TRUE)
  ))
  p_cols <- which(str_detect(
    precips_names,
    fixed("p.", ignore_case = TRUE)
  ))

  ppt_cols_count <- length(ppt_cols)
  p_cols_count <- length(p_cols)

  if ((ppt_cols_count <= 0) & (p_cols_count <= 0)) {
    stop("Missing precip values in obs data frame")
  }

  # now extract ppt and p values
  if (ppt_cols_count > 0) {
    ppt_names <- names(precips)[ppt_cols]

    # first aggregate by date

    # check obs interval
    # find interval length

    dates <- as.Date(precips$datetime)
    ppt_daily <- aggregate(precips[, ppt_cols], by = list(dates), FUN = "mean")

    names(ppt_daily) <- c("date", ppt_names)
    ppt_daily_cols <- seq(2, ncol(ppt_daily))
    ppt_daily$date <- as.Date(ppt_daily$date, format = "%Y-%m-%d")
    ppt_daily$month_year <- format(ppt_daily$date, format = "%m-%Y")
    ppt_daily$month <- as.numeric(format(ppt_daily$date, format = "%m"))
    # now get monthly x year totals
    ppt_monthly <- aggregate(ppt_daily[, ppt_daily_cols], by = list(
      ppt_daily$month_year,
      ppt_daily$month
    ), FUN = "sum")
    names(ppt_monthly) <- c("month_year", "month", ppt_names)

    # get mean by month
    ppt_monthly_cols <- seq(3, ncol(ppt_monthly))
    ppt_monthly_mean <- aggregate(ppt_monthly[, ppt_monthly_cols],
      by = list(ppt_monthly$month), FUN = "mean"
    )
    names(ppt_monthly_mean) <- c("month", ppt_names)
    all_monthly_mean <- ppt_monthly_mean
  }

  if (p_cols_count > 0) {
    p_names <- names(precips)[p_cols]
    p <- precips[, c(1, p_cols)]
    p_cols <- seq(2, ncol(p))
    p$month_year <- format(p$datetime, format = "%m-%Y")
    p$month <- as.numeric(format(p$datetime, format = "%m"))

    # get monthly x year totals
    p_monthly <- aggregate(p[, p_cols], by = list(p$month_year, p$month), FUN = "sum")
    names(p_monthly) <- c("month_year", "month", p_names)
    p_monthly_cols <- seq(3, ncol(p_monthly))
    # get mean by month
    p_monthly_mean <- aggregate(p_monthly[, p_monthly_cols],
      by = list(p_monthly$month), FUN = "mean"
    )
    names(p_monthly_mean) <- c("month", p_names)
    if (ppt_cols_count > 0) {
      all_monthly_mean <- cbind(all_monthly_mean, p_monthly_mean[, -1])
    } else {
      all_monthly_mean <- p_monthly_mean
    }
  }


  all_melted <- melt(all_monthly_mean, id.vars = "month")

  precip_variable_names <- names(all_monthly_mean)[-1]

  # plot if selected
  if (plot) {
    all_melted$month <- monthnames[all_melted$month]
    all_melted$month <- factor(all_melted$month, levels = monthnames)
    p_plot <- ggplot(all_melted, aes(month, value)) +
      geom_bar(stat = "identity") +
      ylab("Mean monthly precipitation (mm)") +
      facet_wrap(~variable) + xlab("")
  }

  comment <- paste("monthlyPrecipDistribution obs:", obsName,
    " precip_variables:",
    str_c(precip_variable_names, collapse = ","),
    sep = ""
  )

  result <- logAction(comment, logfile)

  # return either data frame or graph
  if (result) {
    if (plot == TRUE) {
      return(p_plot)
    } else {
      return(all_monthly_mean)
    }
  }
  else {
    return(result)
  }
}
