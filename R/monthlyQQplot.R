#' Quantile-quantile plot by month
#' 
#' @description Does QQ plots of monthly values of a single variable type in two data frames.
#' @param primaryCRHM Required. The primary \pkg{CRHMr} data frame. Quantiles of this data will be plotted on the X axis.
#' @param primaryCol Optional. The column in the primary data frame, not including the datetime. If not specified, defaults to the first column.
#' @param secondaryCRHM Required. The secondary \pkg{CRHMr} data frame.  Quantiles of this data will be plotted on the Y axis.
#' @param secondaryCol Optional. The column in the secondary data frame, not including the datetime. If not specified, defaults to the first column.
#' @param samePeriod Optional. Logical. Should the same period of time be used for both variables? Default is \code{TRUE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If successful, returns a \pkg{ggplot2} object of faceted monthly QQ plots (3 rows x 4 columns) of the specified variables. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @seealso  \code{\link{monthlyPrecipTotals}}
#' @export
#'
#' @examples
#' BadLake <- BadLake7376
#' BadLake$year <- as.numeric(format(BadLake7376$datetime, format='%Y'))
#' badlake73 <- subset(BadLake, year==1973)
#' badlake75 <- subset(BadLake, year==1975)
#' p <- monthlyQQplot(badlake73, 1, badlake75, 1, samePeriod=FALSE)
#' print(p)
monthlyQQplot <- function(primaryCRHM, primaryCol=1, 
                          secondaryCRHM, secondaryCol=1,
                          samePeriod=TRUE, logfile=''){
  monthnames <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 
                  'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  
  # suppress checking of data frame variables used by ggplot2
  month <- NULL
  x <- NULL
  y <- NULL
  
  # check for required parameters
  if (nrow(primaryCRHM) == 0){
    cat('Error: missing primary obs values\n')
    return(FALSE)
  }
  
  if (nrow(secondaryCRHM) == 0){
    cat('Error: missing secondary obs values\n')
    return(FALSE)
  }
  
  primaryName <- deparse(substitute(primaryObs))
  secondaryName <- deparse(substitute(primaryObs)) 
  
  primaryCol <- primaryCol + 1
  secondaryCol <- secondaryCol + 1  
  primaryCol_with_time <- c(1, primaryCol)
  secondaryCol_with_time <- c(1, secondaryCol)  
  
  primary_columns_length <- length(primaryCol)
  secondary_columns_length <- length(secondaryCol)  
  

  if ((primary_columns_length > 1) | (secondary_columns_length > 1)){
    cat('Error: only a single columns can be specified\n')
    return(FALSE)
  }

  primary_variable_name <- names(primaryCRHM)[primaryCol]
  primary_rh_loc_num <- length(grep("rh", tolower(primary_variable_name), fixed=TRUE))
  secondary_variable_name <- names(secondaryCRHM)[secondaryCol]
  secondary_rh_loc_num <- length(grep("rh", tolower(secondary_variable_name), fixed=TRUE))
  
  if((primary_rh_loc_num > 0) |(secondary_rh_loc_num > 0)){
    cat("Error: can't do QQ plots of RH data\n")
    return(FALSE)
  }
  
  # select columns
  primaryCRHM_selected <- primaryCRHM[,primaryCol_with_time]
  secondaryCRHM_selected <- secondaryCRHM[,secondaryCol_with_time]
  
  if(samePeriod){
    # merge data frames together
    merged <- merge(primaryCRHM_selected, secondaryCRHM_selected, by='datetime', all=TRUE)
    merged<- na.omit(merged)
    
    if (nrow(merged) == 0){
      cat('Error: no common dates\n')
      return(FALSE)
    }
    # get month
    merged$month <- as.numeric(format(merged$datetime, format='%m'))
    
    for (monthnum in 1:12){
      monthly <- merged[merged$month == monthnum ,]
      qqvals <- qqplotValues(monthly[,2], monthly[,3])
      if (monthnum == 1)
        plotvals <- data.frame(monthnum, qqvals)
      else
        plotvals <- rbind(plotvals, data.frame(monthnum, qqvals))
    }
  }
  else{
    primaryCRHM_selected$month <- as.numeric(format(primaryCRHM_selected$datetime, format='%m'))
    secondaryCRHM_selected$month <- as.numeric(format(secondaryCRHM_selected$datetime, format='%m'))     
    for (monthnum in 1:12){
      primary_monthly <- primaryCRHM_selected[primaryCRHM_selected$month == monthnum ,]
      secondary_monthly <- secondaryCRHM_selected[secondaryCRHM_selected$month == monthnum,]
      qqvals <- qqplotValues(primary_monthly[,2], secondary_monthly[,2])
      if (monthnum == 1)
        plotvals <- data.frame(monthnum, qqvals)
      else
        plotvals <- rbind(plotvals, data.frame(monthnum, qqvals))
    }
  }
  plotvals$monthnum <- factor(plotvals$monthnum, labels=monthnames)
  # do plot
  xLabel <- paste('Quantiles ', primaryName, ' ', primary_variable_name)
  yLabel <- paste('Quantiles ',secondaryName, ' ', secondary_variable_name)
  p <- ggplot2::ggplot(plotvals, ggplot2::aes(x, y)) + 
    ggplot2::geom_point() + 
    ggplot2::facet_wrap(~monthnum, ncol=4) + 
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::geom_abline(intercept = 0, slope=1) + 
    ggplot2::xlab(xLabel) + 
    ggplot2::ylab(yLabel)
  
  comment <- paste('monthly QQplot primaryCRHM:', primaryName,
                   ' primary_variable:', primary_variable_name,
                   ' secondaryCRHM:', secondaryName,
                   ' secondary_variable:', secondary_variable_name,
                   sep='')  
  
  result <- logAction(comment, logfile)
  if (result)
    return(p)
  else
    return(result)
}