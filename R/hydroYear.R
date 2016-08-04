#' Returns a vector of the hydrological year for the input data frame
#' @description Calculates the hydrological year, based on the starting month.
#' @param CRHMdata Required. A \pkg{CRHMr} data frame.
#' @param startMonth Optional. The starting month for the hydrological year. Default is \code{10} (October).
#' @param useSecondYear Optional. Logical. Determines if the hydrological year is based on the first or second calendar year. In other words would the hydrological year for January 1, 2015 be 2014 or 2015? The default is \code{TRUE} (i.e., the hydrological year would be 2015). Note that the Campbell Scientific program SPLIT uses the first calendar year (i.e., the hydrological year would be 2014). To emulate this program, set \code{useSecondYear} to be \code{FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If successful, returns the hydrological year as a vector. If unsuccessful, returns the value \code{FALSE}.
#' @seealso \code{\link{aggDataframe}}
#' @author Kevin Shook. Inspired by a function by Chris Marsh.
#' @export
#'
#' @examples
#' # create a new data frame, then add the hydro year
#' BadLake <- BadLake7376
#' BadLake$hydroyear <- hydroYear(BadLake, startMonth=9)
hydroYear <- function(CRHMdata, startMonth=10, useSecondYear=TRUE, logfile=''){
  if(is.null(CRHMdata)){
    cat('Error: missing data frame\n')
    return(FALSE)
  }
  CRHMname <- deparse(substitute(CRHMdata))
    
  if ((startMonth < 1) | (startMonth > 12)){
    cat('Error: invalid start month\n')
    return(FALSE)
  }
  
  year <- as.numeric(format(CRHMdata$datetime, format='%Y'))
  month <- as.numeric(format(CRHMdata$datetime, format='%m'))

  if(useSecondYear){
    hYear <- year
    hYear[month >= startMonth] <- hYear[month >= startMonth] + 1
  }
  else{
    hYear <- year
    hYear[month < startMonth] <- hYear[month < startMonth] -1  
  }

  if(useSecondYear)
    hydroyearUsed <- 'Second year'
  else
    hydroyearUsed <- 'First year'
  
  comment <- paste('hydroyear dataframe:', CRHMname, 
                   ' startMonth:', startMonth, 'hydroyear:', 
                   hydroyearUsed, sep='')
  result <- logAction(comment, logfile) 
  if (result)
    return (hYear)
  else
    return(result)
}