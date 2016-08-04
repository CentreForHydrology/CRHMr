#' Reads in Campbell datalogger data
#'
#' @description Reads a datafile produced by Campbell Scientific dataloggers into a \pkg{CRHMr} data frame. Both old (multiple table, no header) and new (single table, with header) data files can be read in.
#' @param campbellFile Required. The name of the file containing the Campbell Scientific datalogger output. 
#' @param missingValue Optional. Default is -6999. All values less than or equal to this will be set to be \code{NA_real_} .
#' @param timezone Required. The name of the timezone of the data as a character string. This should be the timezone of your data, but omitting daylight savings time. Note that the timezone code is specific to your OS. To avoid problems, you should use a timezone without daylight savings time. Under Linux, you can use \option{CST} and \option{MST} for Central Standard or Mountain Standard time, respectively. Under Windows or OSX, you can use \option{etc/GMT+6} or \option{etc/GMT+7} for Central Standard and Mountain Standard time. DO NOT use \option{America/Regina} as the time zone, as it includes historical changes between standard and daylight savings time.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If unsuccessful, returns \code{FALSE}. If successful, returns a \pkg{CRHMr} data frame. If there are two tables in the campbellFile, then they are returned as a list. 
#' @author Kevin Shook
#' @note If the campbellFile does not contain headers, then the variable names will be assigned by \code{R}. In any case, you will probably want to rename the variables using the command \code{names()}.
#' @seealso \code{\link{readObsFile}} \code{\link{readExportFile}} \code{\link{readOutputFile}}
#' @export
#'
#' @examples
#' \dontrun{
#' # read in an old format Campbell file
#' oldformatdata <- readCampbell('OldDataLogger.dat', timezone='MST')
#' # now extract the data frames from the returned list 
#' obs1 <- oldfomatdata[[1]]
#' obs2 <- oldformatdata[[2]]
#' # read in a new format Campbell file
#' obs3 <- readCampbell('BowHut_Fifteen.dat', timezone='MST')}
readCampbell <- function(campbellFile, missingValue=-6999, timezone='', quiet=TRUE, logfile=''){
  returnDataframes <- list()
  keepCols <- c(0)
  
  # check parameters
  if (campbellFile == ''){
    cat('Error: must specify file name\n')
    return(FALSE)
  }
  
  if (timezone == ''){
    cat('Error: must specify a timezone\n')
    return(FALSE)
  }
  
  # check to see if there is a header
  campbellHeader <- read.csv(campbellFile, header=FALSE, stringsAsFactors = FALSE, nrows=1, skip=1)
  
  if(is.numeric(campbellHeader[1,1]))
    headerPresent <- FALSE
  else
    headerPresent <- TRUE
  

  if(headerPresent){
    campbellData <- read.csv(campbellFile, header=FALSE, stringsAsFactors = FALSE, skip=4)
    names(campbellData) <- campbellHeader
    
    # convert new date/time to CRHMr datetime
    names(campbellData)[1] <- 'datetime'
    datetime <- as.POSIXct(campbellData$datetime, format='%Y-%m-%d %H:%M:%S',tz=timezone)
    
    # get rid of "TIMESTAMP" and "RECORD" columns
    campbellData <- campbellData[,-1:-2]
    campbellData <- cbind(datetime, campbellData)
    
    # now set missing values to NA_real_
    # set values less than the missingValue to be NA_real_
    colNum <- ncol(campbellData)
    for (i in 2:colNum){
      colVals <- campbellData[,i]
      missingLocs <- (colVals <= missingValue)
      colVals[missingLocs] <- NA_real_
      campbellData[,i] <- colVals
    }
    
    file.info <- CRHM_summary(campbellData)
    if (!quiet)
      print(file.info)
    
    comment <- paste('readCampbellFile campbellFile:', campbellFile, 
                     ' timezone:', timezone, sep='')
    result <- logAction(comment, logfile)
    
    if(result)
      return(campbellData)
    else
      return(result)
    

  }
  else{
  # no header, old style
    campbellData <- read.csv(campbellFile, header=FALSE, stringsAsFactors = FALSE)
    
    # check for number of data tables
    tables <- unique(campbellData[,1])
    tableCount <- length(tables)

    for (tablenum in 1:tableCount){
      campbellTable <- campbellData[campbellData[,1]==tables[tablenum],]
      colCount <- ncol(campbellTable)
      # find times with only 3 digits and add leading zero
      timelengths <- stringr::str_length(campbellTable[,4])
      shortTimes <- (timelengths == 3)
      veryshortTimes <- (timelengths == 2)
      campbellTable[shortTimes,4] <- paste('0', campbellTable[shortTimes,4], sep='')
      campbellTable[veryshortTimes,4] <- paste('00', campbellTable[veryshortTimes,4], sep='')
      
      # convert date/time vars to datetime
      datetime <- paste(campbellTable[,2],'-', campbellTable[,3], ' ',campbellTable[,4], sep='')
      datetime <- as.POSIXct(datetime, format='%Y-%j %H%M', tz=timezone)
      campbellTable <-  campbellTable[,5:colCount]
      campbellTable <- cbind(datetime, campbellTable)
      names(campbellTable)[1] <- 'datetime'
      
      # remove columns which only have NA values
      colNum <- ncol(campbellTable)
      colNumCount <- 1
      for (i in 1:colNum){
        colmax <- max(campbellTable[,i])
        if(!is.na(colmax)){
          keepCols[colNumCount] <- i
          colNumCount <- colNumCount + 1          
        }
      } 

      campbellTable <- campbellTable[, keepCols]
      
      # set values less than the missingValue to be NA_real_
      colNum <- ncol(campbellTable)
      for (i in 2:colNum){
        colVals <- campbellTable[,i]
        missingLocs <- (colVals <= missingValue)
        colVals[missingLocs] <- NA_real_
        campbellTable[,i] <- colVals
      }
      
      # assign variable names if specified
      # if (length(tables) == 1){
      #   if (varNames !='')
      #     names(campbellTable)[-1] <- varNames
      #   return(campbellTable)  
      # }

      if (!quiet){
        cat('Data table: ', tablenum, '\n', sep='')
        file.info <- CRHM_summary(campbellTable)
        print(file.info)
      }

      
      # assemble list of data frames
      returnDataframes[[tablenum]] = campbellTable
    }
  }
  
  comment <- paste('readCampbellFile campbellFile:', campbellFile, 
                   ' timezone:', timezone, sep='')
  result <- logAction(comment, logfile)
  
  if(result)
    return(returnDataframes)
  else
    return(result)
}
