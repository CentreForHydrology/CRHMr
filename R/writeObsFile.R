#' Writes a dataframe to a .obs file
#'
#' @description Writes a CRHM dataframe to an obs file. It will not allow ea values - they must be
#' converted to RH values first. The CRHM filter command to convert RH values to ea is added
#' automatically to the header of the .obs file.
#' @param obs Required. A \pkg{CRHMr} obs data frame.
#' @param obsfile Required. Name for file to hold observations.
#' @param comment Optional. A comment to be added to the header of the .obs file. If
#' omitted, the header will consist of the name of the obs data frame
#' followed by \option{created by CRHMr function writeObsFile}.
#' @param obsfilter Optional. Default is \code{NULL} (not used). If specified, as a
#' string scalar or vector, each value will be added to the file as an an
#' observation filter. Commonly used to adjust 10m wind speed to 2m. Note
#' that the observation filter for converting RH to ea is added autmatically,
#' so you don't have to specify it.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you
#' are calling this function in an \R script, you will usually leave \code{quiet=TRUE}
#' (i.e. the default). If you are working interactively, you will probably want to
#' set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action.
#' Normally not used.
#' @author Kevin Shook
#' @note If there are missing values in the obs dataframe, the last line of the header will contain
#' the line \option{$$ Missing}.
#' @return If successful, returns \code{TRUE}. If unsuccessful, returns \code{FALSE}.
#' @seealso  \code{\link{trimObs}} \code{\link{changeEatoRH}}
#' @examples \dontrun{
#' # write sample obs dataframe to .obs file
#' result <- writeObsFile(BadLake7376, 'BadLake7376.obs')}
#' @export

writeObsFile <- function(obs, obsfile="", obsfilter=NULL, comment="", quiet=TRUE, logfile=""){
  eol.val <- win.eol()

  # check parameters
  rh.filter <- '$ea ea(t, rh)'
  if (obsfile == "") {
    cat('Missing .obs file name\n')
    return(FALSE)
  }

  obsName <- deparse(substitute(obs))

  obs.names <- names(obs)[-1]
  obs.names.uppercase <- toupper(obs.names)
  ea.loc <- grep("EA.", obs.names.uppercase, fixed = TRUE)
  rh.loc <- grep("RH", obs.names.uppercase, fixed = TRUE)

  if (length(ea.loc) > 0)
    ea.present <- TRUE
  else
    ea.present <- FALSE

  if (length(rh.loc) > 0)
    rh.present <- TRUE
  else
    rh.present <- FALSE

  if (ea.present & rh.present) {
    cat("Error: you can't have both ea and RH values in an obs file\n")
    return(FALSE)
  }

  # re-do names
  obs.names <- names(obs)[-1]
  obs.names.uppercase <- toupper(obs.names)

  # assemble header
  if (comment == '')
    comment <- paste(obsName,' created by CRHMr function writeObsFile', sep = '')
  cat(comment, eol.val, sep = '', file = obsfile, append = FALSE)

  # find unique variable names
  obs.strings <- stringr::str_split_fixed(obs.names, stringr::fixed('.'), 3)
  obs.strings <- as.data.frame(obs.strings)
  var.names <- as.character(unique(obs.strings[,1]))
  var.units <- rep.int(' ', length(var.names))
  var.units[var.names == 'rh'] <- '(%)'
  var.units[var.names == 't'] <- '(C)'
  var.units[var.names == 'u'] <- '(m/s)'
  var.units[var.names == 'u10'] <- '(m/s)'
  var.units[var.names == 'u2'] <- '(m/s)'
  var.units[var.names == 'p'] <- '(mm/int)'
  var.units[var.names == 'ppt'] <- '(mm)'
  var.units[var.names == 'ea'] <- '(kPa)'
  var.units[tolower(var.names) == 'qsi'] <- '(W/m^2)'
  var.units[tolower(var.names) == 'qso'] <- '(W/m^2)'
  var.units[tolower(var.names) == 'qsn'] <- '(W/m^2)'
  var.units[tolower(var.names) == 'qli'] <- '(W/m^2)'
  var.units[tolower(var.names) == 'qlo'] <- '(W/m^2)'
  var.units[tolower(var.names) == 'qln'] <- '(W/m^2)'
  var.units[tolower(var.names) == 'qn'] <- '(W/m^2)'

  # find number of each type of variable
  # can't use a table as it sorts alphabetically
  num.vars <- length(var.names)
  for (i in 1:num.vars) {
    var.count <- sum(obs.strings[,1] == var.names[i])
    cat(var.names[i], '\t', var.count, ' ', var.units[i], eol.val, sep = '',
        file = obsfile, append = TRUE)
  }

  if (rh.present)
    cat(rh.filter, eol.val, sep = "", file = obsfile, append = TRUE)

  if (!is.null(obsfilter)) {
    for (i in 1:length(obsfilter)) {
      cat(obsfilter[i], eol.val, sep = "", file = obsfile, append = TRUE)
    }
  }


  # check to see if missing values, and indicate in obsfile
  obs.clean <- na.omit(obs)
  if (nrow(obs.clean) != nrow(obs))
    cat('$$ Missing',eol.val, sep = '', file = obsfile, append = TRUE)

  # add names to columns
  colnames <- stringr::str_c(obs.names, collapse = '\t')
  cat('################','\t',colnames, eol.val, sep = '',
      file = obsfile, append = TRUE)


  # output info to screen and write to log file
  obs.info <- CRHM_summary(obs)
  if (!quiet)
    print(obs.info)

  # write values to file

  obs[,1] <- format(obs[,1], format = '%Y %m %d %H %M')
  write.table(obs, file = obsfile, sep = '\t', eol = eol.val,
              row.names = FALSE, quote = FALSE,
              col.names = FALSE, append = TRUE)


  comment <- paste('writeObsFile dataframe:', obsName,
                   ' .obs file: ', obsfile, sep = '')
  result <- logAction(comment, logfile)

  return(TRUE)
}
