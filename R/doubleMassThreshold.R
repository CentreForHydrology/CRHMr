#' Calculates effect of thresholding on double mass values
#'
#' @description Calculates cumulative sums for two variables and optionally returns a plot. Only values coinciding in time are used for the cumulative sums.
#' @param primaryObs Required. The primary \pkg{CRHMr} data frame of obs values.
#' @param primaryCol Optional. The column number to be used, not including the datetime. If not specified, defaults to the first column.
#' @param secondaryObs Required. The secondary \pkg{CRHMr} data frame of obs values.
#' @param secondaryCol Optional. The column number to be used, not including the datetime. If not specified, defaults to the first column.
#' @param maxThreshold Optional. The maximum value for the threshold to be applied to the secondary observations.
#' @param plot Optional. Should a plot be created? Default is \code{FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If \code{plot = TRUE}, then a \pkg{ggplot2} object will be returned, with the \code{primaryObs} values plotted on the x-axis and the \code{secondaryObs} values plotted on the y-axis. Otherwise, a dataframe is returned with the cumulative values ov each of the variables.
#' @author Kevin Shook
#' @export
#'
#' @examples \dontrun{
#' p <- doubleMass(primaryObs=CamrosePPT, secondaryObs=WetaskiwinPPT, plot=TRUE)}
doubleMass <- function(primaryObs, primaryCol=1, secondaryObs, secondaryCol=1,
                       plot=TRUE, logfile=''){
  # suppress checking of data frame variables used by ggplot2
  x <- NULL
  y <- NULL

  # check parameters
  if ((nrow(primaryObs) == 0) | (nrow(secondaryObs) == 0)){
    cat('Error: missing obs\n')
    return(FALSE)
  }

  if ((length(primaryCol) > 1) | (length(secondaryCol) > 1)){
    cat('Error: only a single variable can be plotted\n')
    return(FALSE)
  }

  primaryVarCount <- ncol(primaryObs) - 1
  secondaryVarCount <- ncol(secondaryObs) - 1

  if (primaryCol > primaryVarCount){
    cat('Error: primaryCol > number of variables\n')
    return(FALSE)
  }

  if (secondaryCol > secondaryVarCount){
    cat('Error: secondaryCol > number of variables\n')
    return(FALSE)
  }

  primaryCRHMname <- deparse(substitute(primaryObs))
  secondaryCRHMname <- deparse(substitute(secondaryObs))

  # merge data frames together and remove missing values
  primary <- primaryObs[,c(1,(primaryCol+1))]
  secondary <- secondaryObs[,c(1,(secondaryCol+1))]

  primary <- na.omit(primary)
  secondary <- na.omit(secondary)

  merged <- merge(primary, secondary, by='datetime', all=FALSE)

  # remove missing
  merged <- na.omit(merged)

  # make sure there's something to return
  if (nrow(merged) == 0){
    cat('Error: no common datetimes\n')
    return(FALSE)
  }


  primaryName <- paste('Cumulative ', primaryCRHMname,' ', names(primary)[2], sep='')
  secondaryName <- paste('Cumulative ', secondaryCRHMname,' ', names(primary)[2], sep='')

  # aggregate
  doubleMassVals <- data.frame(merged[,1], cumsum(merged[,2]), cumsum(merged[,3]))
  names(doubleMassVals) <- c('datetime', 'x', 'y')
  if (plot){
    p <- ggplot2::ggplot(doubleMassVals, ggplot2::aes(x, y)) +
      ggplot2::geom_line() +
      ggplot2::xlab(primaryName) +
      ggplot2::ylab(secondaryName) +
      ggplot2::geom_abline(slope=1, intercept = 0, colour='red') +
      ggplot2::coord_fixed(ratio = 1)
    output <- p
  }
  else
    output <- doubleMassVals


  comment <- paste('doubleMass primary:', primaryCRHMname,', secondary:',secondaryCRHMname,
                   ' primary_variable:', primaryName,
                   ' secondary_variables', secondaryName, sep='')

  result <- logAction(comment, logfile)

  return(output)
}
