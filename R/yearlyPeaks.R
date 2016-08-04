#' Finds datetimes of annual max values
#'
#' @description Finds the annual maxima (and, optionally, minima) of the specfied columns in a CRHM dataframe and their corresponding datetimes.
#' @param CRHMdata Required. A \pkg{CRHMr} dataframe.
#' @param columns Optional. A vector containing the columns to be analyzed, not including the datetime column. If not specified, all data columns are used.
#' @param minvals Optional. Logical. Specifies if yearly minimum values should also be found. Default value is \code{FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return Returns a dataframe with maximum (and minimum if selected) values, and their corresponding datetimes, for each year for each selected variable.
#' @author Kevin Shook
#' @seealso \code{\link{aggDataframe}}
#' @export
#'
#' @examples
#' \dontrun{
#' p <- yearlyPeaks(BrandonObs, columns=c(1,2), minvals=TRUE)}
yearlyPeaks<- function(CRHMdata, columns=0, minvals=FALSE, logfile=''){
  output_names <- c('year')
  
  #check parameters
  if (nrow(CRHMdata) == 0){
    cat('Error: missing CRHM dataframe\n')
    return(FALSE)
  }
  CRHMname <- deparse(substitute(CRHMdata))
  
  if (length(columns) == 1){
    if (columns == 0)
     columns <- seq(2, (ncol(CRHMdata)-1))
    else{
      columns <- columns + 1
    }
  }
  else{
    columns <- columns + 1
  }
    
  variable_names <- names(CRHMdata)[columns]
  variable_count <- length(variable_names)
  
  CRHMdata$year <- as.numeric(format(CRHMdata$datetime, format='%Y'))
  yearly_max_values <- aggregate(CRHMdata[,columns], 
                                 by=list(CRHMdata$year), FUN='max')
  yearly_max_locs <- aggregate(CRHMdata[,columns], 
                               by=list(CRHMdata$year), FUN='which.max')

  if (minvals){
    yearly_min_locs <- aggregate(CRHMdata[,columns], 
                                 by=list(CRHMdata$year), FUN='which.min')
    yearly_min_values <- aggregate(CRHMdata[,columns], 
                                   by=list(CRHMdata$year), FUN='min')
  }
  yearly_lengths <- aggregate(CRHMdata[,columns], 
                              by=list(CRHMdata$year), FUN='length') 
  # sum up length of each year's data
  total_lengths <- cumsum(yearly_lengths[,2])
  year_count <- length(total_lengths)
  prev_year_length <- c(0, total_lengths[1:(year_count-1)])
  # find peaks and create dataframe for output
  output <- yearly_max_values[,1]
  for (i in 2:(variable_count+1)){
    offsets_max_var <- yearly_max_locs[,i]
    if (minvals)
      offsets_min_var <- yearly_min_locs[,i]
   
    # get location of each year's max
    offsets_max <- offsets_max_var + prev_year_length
    yearly_max_dates <- CRHMdata[offsets_max, 1]
    
    # get location of each year's min
    if (minvals){
      offsets_min <- offsets_min_var +prev_year_length
      yearly_min_dates <- CRHMdata[offsets_min, 1]
    }
    
   
    output <- data.frame(output, yearly_max_values[,i], yearly_max_dates)
    variable_name <- variable_names[i-1]
    max_name <- paste(variable_name,'.max', sep='')
    max_loc_name <- paste(variable_name,'.max.time', sep='')
    output_names <- c(output_names, c(max_name, max_loc_name))
    if(minvals){
      output <- data.frame(output, yearly_min_values[,i], yearly_min_dates)
      min_name <- paste(variable_name,'.min', sep='')
      min_loc_name <- paste(variable_name,'.min.time', sep='')
      output_names <- c(output_names, c(min_name, min_loc_name))
    }
  }
  output <- data.frame(output)
  names(output) <- output_names
  
  comment <- paste('yearlyPeaks Dataframe:', CRHMname,
                   ' variables:', stringr::str_c(variable_names, collapse=','),
                   sep='')  
  
  result <- logAction(comment, logfile)
  
  if(result)
    return(output)
  else
    return(result)
} 