#' Stacked plot of yearly water balance variables
#' @description Creates a stacked bar plot of all water balance components computed for a basin. Basin inputs are plotted as positive values, outputs are plotted as negative.
#' @param yearlyWater Required. A data frame of water balance components. The first column must be the year. Note that this function does not allow you to select columns - all columns will be plotted.
#' @param negCols Optional. Columns to be plotted as negative values. If not specified (the default) the columns will be guessed from their names.
#'
#' @return If successful returns a \pkg{ggplot2} object showing stacked bars of the water balance components for each year. If unsuccessful returns \code{FALSE}.
#' @export
#'
#' @examples \dontrun{
#' # get daily water balance from CRHM output
#' daily <- simpleDailyWater(Bol84, prjFile = 'Bologna1984_02.prj',
#' basinMean = TRUE, summarize = TRUE)
#'
#' # get yearly values
#' daily.datetime <- dateToDatetime(daily, timezone='Etc/GMT+7')
#' yearly <- aggDataframe(daily.datetime, period='hydro', funs='sum')
#'
#' # plot
#' p <- basinWaterBalancePlot(yearly)
#' }
basinWaterBalancePlot <- function(yearlyWater, negCols=''){
  # idea from http://www.r-bloggers.com/improved-net-stacked-distribution-graphs-via-ggplot2-trickery/
  # declare ggplot2 variables
  Year <- NULL
  value <- NULL
  variable <- NULL

  # get variable names and remove .sum if required
  varNames <- names(yearlyWater)
  names(yearlyWater)[1] <- 'Year'

  cleanNames <- stringr::str_replace(varNames, stringr::fixed('.sum', ignore_case = TRUE), '')
  names(yearlyWater) <- cleanNames

  # melt by year
  yearlyMelted <- reshape2::melt(yearlyWater, id.vars=1)


  # add direction
  yearlyMelted$direction <- 'positive'
  yearlyMelted$variable <- as.character(yearlyMelted$variable)

  if (negCols != ''){
    negNames <- names(yearlyWater)
    negLocs <- setequal(negNames, yearlyMelted$variable)
    yearlyMelted$direction[negLocs] <- 'negative'
  }
  else{
    negNames <- c('basinflow', 'evap', 'subl', 'loss', 'outflow')
    negNameCount <- length(negNames)
    for (i in 1:negNameCount){
      negLocs <- stringr::str_detect(yearlyMelted$variable,
                                     stringr::fixed(negNames[i], ignore_case=TRUE))
      if (i == 1)
        negPos <- negLocs
      else
        negPos <- negPos | negLocs
    }
    yearlyMelted$direction[negPos] <- 'negative'
  }

  # subset values
  positives <- yearlyMelted[yearlyMelted$direction=='positive',]
  negatives <- yearlyMelted[yearlyMelted$direction=='negative',]
  negatives$value <- negatives$value * -1
  # plot values

  p <- ggplot2::ggplot() + ggplot2::aes(Year, value, fill = variable) +
    ggplot2::geom_bar(data = negatives, stat = "identity") +
    ggplot2::geom_bar(data = positives, stat = "identity") +
    ggplot2::scale_y_continuous() +
    ggplot2::geom_hline(yintercept=0)

    return(p)
}
