#' Calculates quantile values for a Quantile-Quantile plot
#'
#' @description The built-in \code{qqplot} function does not work with \pkg{ggplot2}. This just calls the qqplot function to caclulate the quantiles without plotting them.
#' @param x Required. A numeric vector.
#' @param y Required. A numeric vector.
#'
#' @return Returns a dataframe with the quantiles of x and y.
#' @author Kevin Shook
#' @export
#'
#' @examples
#' quantiles <- qqplotValues(runif(20), runif(50))
qqplotValues <- function(x, y){
  qq <- data.frame(qqplot(x,  y, plot.it=FALSE))
  return(qq)
}
