#' Reads exported CRHM output into a data frame
#'
#' @description Reads output from a CRHM model, that has been manually exported, into a \pkg{CRHMr} data frame. CRHM model output can be exported in several formats. This function is intended to read in all of them. Note that because of the way that \R reads in files, the variable names will have appended periods, e.g. \code{t.1}.
#' @param exportFile Required. The name of the CRHM export file to be read in.
#' @param timezone Required. The name of the timezone of the data as a character string. This should be the timezone of your data, but omitting daylight savings time. Note that the timezone code is specific to your OS. To avoid problems, you should use a timezone without daylight savings time. Under Linux, you can use \option{CST} and \option{MST} for Central Standard or Mountain Standard time, respectively. Under Windows or OSX, you can use \option{etc/GMT+6} or \option{etc/GMT+7} for Central Standard and Mountain Standard time. DO NOT use \option{America/Regina} as the time zone, as it includes historical changes between standard and daylight savings time.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If successful, returns a \pkg{CRHMr} data frame. If unsuccessful, returns the value \code{FALSE}.
#' @author Kevin Shook
#' @seealso \code{\link{readOutputFile}} \code{\link{readObsFile}}
#' @examples
#' \dontrun{
#' brandon <- readExportFile('Brandon_export.txt', 'etc/GMT+6')}
#' @export


readExportFile <-
function(exportFile, timezone='', quiet=TRUE, logfile=''){
  # reads CRHM output that has been manually exported in any of the styles

  # check parameters
  if (timezone == ''){
    cat('Error: must specify a timezone\n')
    return(FALSE)
  }

  con <- file(exportFile, "r", blocking = FALSE)
  input <- readLines(con, encoding = "latin1")
  close(con)
  # find header
  header.linenum <- grep("##", input, fixed=TRUE)
  line1 <- input[header.linenum+1]

  # read in, skipping header
  export <- read.table(file=exportFile, header=FALSE, skip=header.linenum,
                       stringsAsFactors=FALSE, sep='\t')
  export.column.count <- ncol(export)

  # find type of date/time
  date1 <- export[1,1]

  # test date to see if it is an integer
  if (date1 != round(date1))
    Excel <- TRUE
  else
    Excel <- FALSE

  if (stringr::str_detect(line1,','))
    ExcelJdate <- TRUE
  else
    ExcelJdate <- FALSE

  if (stringr::str_detect(date1,'/'))
    DMY <- TRUE
  else
    DMY <- FALSE

  if (DMY & !ExcelJdate)
    date.type <- 'DMY'
  else if (ExcelJdate)
    date.type <- 'ExcelJdate'
  else if (Excel)
    date.type <- 'Excel'
  else
    date.type <- 'Obs'

  # read header and extract variables
  header <- input[2:(header.linenum-1)]
  variables <- header[!stringr::str_detect(header,stringr::fixed('$'))]

  # get variable name and count
  variables <- stringr::str_trim(variables)

  # remove parentheses
  variables <- stringr::str_replace_all(variables, stringr::fixed('('),'.')
  variables <- stringr::str_replace_all(variables, stringr::fixed(')'),'')

  # replace spaces with tabs to allow for parsing
  variables <- stringr::str_replace_all(variables, ' ','\t')

  # check for '#' symbols in header
  hash.present <- sum(stringr::str_detect(variables, '#'))
  if (hash.present > 0)
    variables <- stringr::str_replace_all(variables, '#','.calc')

  variables <- stringr::str_split(variables, '\t')

  variable.type.count <- length(variables)
  variable.data.frame <- list2df(variables)
  variable.name <- as.character(variable.data.frame[,1])
  variable.count <-  as.numeric(as.character(variable.data.frame[,2]))

  # get date and name columns
  if (date.type == 'Obs'){
    # have to re-parse
    datecol <- export[,1]
    datetime <- as.POSIXct(datecol, format='%Y %m %d %H %M',tz=timezone)
    variable.column.count <- export.column.count - 1
    export.vars <- export[, 2:export.column.count]
  }
  else if (date.type == 'Excel'){
    # convert Excel time to POSIX
    datetime <- as.POSIXct(as.numeric(export[,1])*24*3600, origin="1899-12-30", tz='UTC')
    datetime <- lubridate::force_tz(datetime, tzone=timezone)
    variable.column.count <- export.column.count - 1
    export.vars <- export[, 2:export.column.count]
  }
  else if (date.type == 'DMY'){
    datetime <- as.POSIXct(export[,1], format='%d/%m/%Y %H:%M',tz=timezone)

    variable.column.count <- export.column.count - 1
    export.vars <- export[, 2:export.column.count]
  }
  else if (date.type == 'ExcelJdate'){
    # this is a horrible format, as it has to be parsed twice
    col1 <- export[,1]
    cols1_4 <- stringr::str_split(col1,',')
    datetimecol <- as.character(as.data.frame(cols1_4)[,4])
    if (stringr::str_detect(date1,'/'))
      datetime <- as.POSIXct(datetimecol, format='%d/%m/%Y %H:%M ',tz=timezone)
    else
      datetime <- as.POSIXct(datetimecol, format='%Y-%m-%d %H:%M ',tz=timezone)

    variable.column.count <- export.column.count - 1
    export.vars <- export[, 2:export.column.count]
  }
  else
  {
    cat('File type is unknown\n')
    return(FALSE)
  }

  # assemble data frame and assign names to columns
  datetime <- as.data.frame(datetime)
  export <- cbind(datetime, export.vars)
  var.names <- c('datetime')
  for (i in 1:variable.type.count){
    var.nums <- seq(1:variable.count[i])
    var.seq <- paste(variable.name[i],'.',var.nums, sep='')
    var.names <- c(var.names, var.seq)
  }
  names(export) <- var.names
  if(date.type == 'Excel')
    export <- makeRegular(export, timezone)

  file.info <- CRHM_summary(export)
  if (!quiet)
    print(file.info)

  # write to log file
  comment <- paste('readExportFile exportFile:',exportFile,
                   ' datetype:',date.type, ' Timezone: ', timezone, sep='')
  result <- logAction(comment, logfile)

  if(result)
    return(export)
  else
    return(result)
}
