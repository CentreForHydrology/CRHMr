#' Finds gaps in obs variables
#'
#' @description Finds successive missing values in an obs data frame. This function is useful to show where data are missing, before you use the \code{\link{interpolate}} or \code{\link{impute}} functions. All of the gaps in each variable are written to a .csv file.
#' @param obs Required. A \pkg{CRHMr} data frame containing obs values.
#' @param gapfile Optional. The name of the output file. If omitted the output file will be the name of the obs data frame, followed by \option{_gaps.csv}.
#' @param minlength The minimum gap length included in the analysis (in time steps). Default is 1 time step.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If successful, and there are gaps, returns \code{TRUE}. If successful, and there are no gaps, returns '\option{No gaps}. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @note If quiet=FALSE, the functions gives a summary of the gaps (number of gaps, their total length) for each year.
#' @seealso \code{\link{insertMissing}} \code{\link{interpolate}} \code{\link{impute}}
#' @examples
#' findGaps(BadLake7376, quiet=FALSE)
#' @export

findGaps <- function(obs, gapfile='', minlength=1, quiet=TRUE, logfile=''){
  # Finds gaps in an obs data frame, and writes them to a file
  if (nrow(obs) == 0){
    cat('Error: missing obs values\n')
    return(FALSE)
  }

  obsName <- deparse(substitute(obs))

  if (gapfile == ''){
    # create file name
    gapfile <- paste(obsName, '_gaps.csv', sep='')
  }

  # add missing values
  gap.filled <- insertMissing(obs, quiet=TRUE)

  if (nrow(obs) == nrow(gap.filled)){
    if (!quiet)
      cat('No gaps in datetimes\n')

    # check to see if any values are missing at all
    originalRows <- nrow(obs)
    clean <- na.omit(gap.filled)
    cleanRows <- nrow(clean)

    if (cleanRows == originalRows){
      cat('No missing values in dataset\n')
      return(FALSE)
    }

  }

  var.names <- names(gap.filled)[-1]
  cols <- length(var.names)

  # now look for gaps
  for (colnum in 1:cols){
    # find runs of NA values greater than specified
    selected <- gap.filled[, colnum+1]
    na.values <- is.na(selected)
    varname <- var.names[colnum]
    if (sum(na.values) > 0){
      # gaps are present
      runs <- rle(na.values)
      c.ndx <- cumsum(runs$lengths)
      c.size <- runs$lengths
      c.datetime <- gap.filled$datetime[c.ndx]
      c.values <- runs$values

      # now select only gaps > minlength
      gap.length <- c.size[c.values]
      gap.datetime <- c.datetime[c.values]
      all.runs <- data.frame(varname, gap.datetime, gap.length, stringsAsFactors=FALSE)
      names(all.runs) <- c('variable', 'datetime', 'gap.length')
      output <- all.runs[(all.runs$gap.length >= minlength),]

      if (!quiet){
        # do summary
        output$year <- as.numeric(format(output$datetime, format='%Y'))
        output.summary.length <- aggregate(output$gap.length, by=list(output$year), FUN='length')
        names(output.summary.length) <- c('year', 'number')
        output.summary.length$year <- as.numeric(output.summary.length$year)
        output.summary.total <- aggregate(output$gap.length, by=list(output$year), FUN='sum')
        names(output.summary.total) <- c('year', 'total')
        output.summary <- cbind(varname, output.summary.length,
                                output.summary.total$total )
        names(output.summary) <- c('variable', 'year', 'gaps', 'total.length')
        output.summary$variable <- as.character(output.summary$variable)
        if (colnum == 1)
          gap.summary <- output.summary
        else
          gap.summary <- rbind(gap.summary, output.summary)
      }

      if (colnum == 1)
        gaps <- output
      else
        gaps <- rbind(gaps, output)
    }
  }

  # check for output

  if(nrow(gaps > 0))
    write.csv(gaps, file=gapfile, row.names=FALSE)

  if (!quiet)
    if (nrow(gap.summary > 0))
      print(gap.summary)

  # output to logfile
  comment <- paste('findGaps obs:', obsName,
                   ' variables:', stringr::str_c(var.names, collapse=','),
                   sep='')

  result <- logAction(comment, logfile)

  return(result)
}
