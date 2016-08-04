#' Finds duplicated datetimes in obs dataframe
#'
#' @description Finds duplicate datetime values. All of the duplicate values are written to a .csv file. Many time series, especially from Environment Canada, may contain duplicate datetimes.
#' @param obs Required. A \pkg{CRHMr} data frame containing obs values.
#' @param dupefile Optional. The name of the output file. If omitted the dupe file will be the name of the obs data frame, followed by \option{_dupedatetimes.csv}.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If there are duplicate datetimes, returns \code{TRUE}. If there are no duplicates, returns \code{FALSE}.
#' @author Kevin Shook
#' @note If quiet=\code{FALSE}, the function gives a list of the duplicate datetimes. Use this function before removing duplicates and/or interpolation and imputation. Most importantly, use this function before writing values to an obs file. Duplicate datetime values in an obs file will prevent CRHM from executing. 
#' @seealso \code{\link{findGaps}} \code{\link{interpolate}} \code{\link{impute}} \code{\link{writeObsFile}}
#' @examples
#' findDupes(BadLake7376, quiet=FALSE)
#' @export


findDupes <- function(obs, dupefile="", quiet=TRUE, logfile=""){
  # Finds duplicate dates in an obs data frame, and writes them to a file
  if (nrow(obs) == 0){
    cat('Error: missing secondary obs values\n')
    return(FALSE)
  }
  obsName <- deparse(substitute(obs))
  
  if (dupefile == ''){
    # create file name
    dupefile <- paste(obsName, '_dupedatetimes.csv', sep='')
  }
  
  # test for duplicates
  dupe.count <- sum(duplicated(obs$datetime))
  if (dupe.count > 0)
    any.dupes <- TRUE
  else
    any.dupes <- FALSE
  
  if(any.dupes){
    dupes <- duplicated(obs$datetime)
    a <- as.data.frame(obs$datetime[dupes])
    names(a) <- 'duplicate.datetimes'
    write.csv(a, file=dupefile, row.names=FALSE)
  }
  
  if(!quiet)
    cat(dupe.count, ' duplicate datetimes\n', sep='')
  
  if (any.dupes)
    output.message <- ' Dupes found'
  else
    output.message <- ' No dDupes found'
  
  comment <- paste('findDupes obs:', obsName, output.message, sep='')  
  result <- logAction(comment, logfile)
  
  # return either data frame or graph
  if(result)
    return(any.dupes)
  else
    return(result)
}