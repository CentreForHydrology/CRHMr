#' Reads a CRHM obs fileinto a dataframe
#'
#' @description Reads a file of CRHM observation data into a \pkg{CRHMr} data frame. Note that because of the way that \R reads in files, the variable names will have appended periods, e.g. \code{t.1}.
#' @param obsFile Required. Name of the CRHM obs file to be read in.
#' @param timezone Required. The name of the timezone of the data as a character string. This should be the timezone of your data, but omitting daylight savings time. Note that the timezone code is specific to your OS. To avoid problems, you should use a timezone without daylight savings time. Under Linux, you can use \option{CST} and \option{MST} for Central Standard or Mountain Standard time, respectively. Under Windows or OSX, you can use \option{etc/GMT+6} or \option{etc/GMT+7} for Central Standard and Mountain Standard time. DO NOT use \option{America/Regina} as the time zone, as it includes historical changes between standard and daylight savings time.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If successful, returns a \pkg{CRHMr} data frame. If unsuccessful, returns the value \code{FALSE}.
#' @author Kevin Shook
#' @seealso \code{\link{readExportFile}} \code{\link{readOutputFile}}
#' @examples
#' # output example obs data to a file
#' BadLake7376.obs <- writeObsFile(BadLake7376, 'BadLake7376.obs')
#' # now read data back in
#' BadLake <- readObsFile('BadLake7376.obs', 'etc/GMT+6')
#' @export

readObsFile <-
function(obsFile='', timezone='', quiet=TRUE, logfile=''){
  # reads in an obs file in any format
  # returns a dateframe with timeDate

  # check parameters
  if (obsFile == ''){
    cat('Error: must specify file name\n')
    return(FALSE)
  }

  if (timezone == ''){
    cat('Error: must specify a timezone\n')
    return(FALSE)
  }

  # find header
  con <- file(obsFile, "r", blocking = FALSE)
  input <- readLines(con, encoding = "latin1")
  close(con)
  # find header
  header.linenum <- grep("##", input, fixed=TRUE)

  # get first line
  line1 <- input[header.linenum+1]

  # get second line
  line2 <- input[header.linenum+2]

  # read in, skipping header
  obs <- read.table(obsFile, header=FALSE, skip=header.linenum,
                    stringsAsFactors=FALSE, comment.char = "#")
  obs.column.count <- ncol(obs)

  # find type of date/time
  date1 <- obs[1,1]
  if (date1 > 3000)
    date.type <- 'Excel'
  else
    date.type <- 'discrete'

  # read header and extract variables
  header <- input[2:(header.linenum-1)]
  variables <- header[!stringr::str_detect(header,stringr::fixed('$'))]

  # get variable name and count
  variables <- stringr::str_trim(variables)
  # replace tabs with spaces to allow for parsing
  variables <- stringr::str_replace_all(variables, ' ','\t')
  variables <- stringr::str_split(variables, '\t')
  variable.type.count <- length(variables)
  variable.name <- c(0)
  variable.count <- c(0)
  #variable.data.frame <- as.data.frame.list(variables)
  #variable.name <- variable.data.frame[,1]
  #variable.count <-  as.numeric(as.character(variable.data.frame[,2]))
  for (i in 1:variable.type.count){
    variable.name[i] <- variables[[i]][1]
    variable.count[i] <- variables[[i]][2]
  }

  # get date and name columns
  if (date.type == 'discrete'){
    # figure out number of date/time columns, i.e. are there minutes as well
    # also depends on if date/time columns are space or tab delimited

    tab.count <- stringr::str_count(line1, stringr::fixed('\t'))
    if (tab.count == 0){
      # all delimiters are spaces
      year <- obs[,1]
      month <- obs[,2]
      day <- obs[,3]
      hour <- obs[,4]
      minute <- obs[,5]
      datetime.colcount <- 5
    }
    else{
      if (tab.count > variable.type.count){
        # tab delimited columns
        year <- obs[,1]
        month <- obs[,2]
        day <- obs[,3]
        hour <- obs[,4]
        minute <- obs[,5]
        datetime.colcount <- 5
      }
      else{
        # space delimited columns
        datetimecol <- stringr::str_split_fixed(line1, '\t', 1000)[1]

        # replace double delimiting spaces with single
        while (stringr::str_count(datetimecol, stringr::fixed('  ')) > 0)
          datetimecol <- stringr::str_replace_all(datetimecol, '  ', ' ')

        datetimecol <- stringr::str_trim(datetimecol, side = "both")
        line1.spaces <- stringr::str_count(datetimecol, stringr::fixed(" "))

        datetime.colcount <- line1.spaces + 1
        year <- obs[,1]
        month <- obs[,2]
        day <- obs[,3]
        hour <- obs[,4]
        minute <- obs[,5]
      }
    }
    # get first column and count spaces

    date <- paste(year,month,day, sep='-')
    datetime <- paste(date, ' ', hour,' ', minute, sep='')
    datetime <- as.POSIXct(datetime, format='%Y-%m-%d %H %M',tz=timezone)

    variable.column.count <- obs.column.count - datetime.colcount
    obs.vars <- obs[, (datetime.colcount+1):obs.column.count]
  }
  else{
    # convert Excel time to POSIX
    datetime <- as.POSIXct(as.numeric(obs[,1])*24*3600, origin="1899-12-30", tz='UTC')
    datetime <- lubridate::force_tz(datetime, tzone=timezone)
    variable.column.count <- obs.column.count - 1
    obs.vars <- obs[, 2:obs.column.count]
  }
  # assemble data frame and assign names to columns
  datetime <- as.data.frame(datetime)
  obs <- cbind(datetime, obs.vars)
  var.names <- c('datetime')
  for (i in 1:variable.type.count){
    var.nums <- seq(1:variable.count[i])
    var.seq <- paste(variable.name[i],'.',var.nums, sep='')
    var.names <- c(var.names, var.seq)
  }
  names(obs) <- var.names

  if(date.type == 'Excel')
    obs <- makeRegular(obs, timezone)

  # output info to screen (if req'd) and write to log file
  file.info <- CRHM_summary(obs)
  if (!quiet)
    print(file.info)

  comment <- paste('readObsFile obsFile:',obsFile,
                    ' datetype:',date.type, ' timezone:', timezone, sep='')
  result <- logAction(comment, logfile)

  if(result)
    return(obs)
  else
    return(result)
}
