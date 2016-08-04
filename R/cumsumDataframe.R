#' Cumulative sums for a data frame
#' @description Finds the cumulative sum of all columns in a data frame. Note that all columns must be numeric - date and datetime variables cannot be summed.
#' @param df Required. Data frame to be summed.
#'
#' @return Returns a dataframe containing the cumulative sums of all columns in the original data frame.
#' @author Kevin Shook
#' @note This function is used by other \pkg{CRHMr} functions, so it does \emph{NO} parameter testing.
#' @seealso \code{\link{cumulDailyWater}}
#' @export
#'
#' @examples \dontrun{
#' cumul <- cumsumDataframe(modelOutput)}
cumsumDataframe <- function(df){
  cols <- ncol(df)
  col_names <- names(df)
  
  for(col in 1:cols){
    col_cumsum <- cumsum(df[,col])
    
    if(col == 1)
      all_sums <- data.frame(col_cumsum)
    else
      all_sums <- cbind(all_sums, col_cumsum)
    
  }
  names(all_sums) <- col_names
  return(all_sums)
}