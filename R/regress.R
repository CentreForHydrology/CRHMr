#' Regresses one set of CRHM observations against another
#'
#' @description The purpose of this function is to generate the multiplier and an offset values (if required) used by the impute function. Therefore, it performs a linear (least-squares) regression of the primary dataset (the values to be produced by imputation) against the secondary dataset. The function removes missing values and merges the data sets together before doing the regression to ensure that the variables are aligned by datetime.
#' @param primaryCRHM Required. The primary \pkg{CRHMr} data frame.
#' @param primary.columns Optional. Optional. A vector containing the columns to be imputed in the primary data frame, not including the datetime. If not specified, defaults to the first column.
#' @param secondaryCRHM Required. The secondary \pkg{CRHMr} data frame.
#' @param secondary.columns Optional. A vector containing the columns to be imputed in the secondary data frame, not including the datetime. If not specified, defaults to the first column.
#' @param forceOrigin Optional. If \code{TRUE} then the regressions will be forced through the origin. Note that in this case the values of r\eqn{^2}{^2} and the intercept will \emph{not} be calculated as they have no meaning.
#' @param plot Optional. Set \code{plot=TRUE} if you want a plot for each regression. Default is \code{FALSE}. The function has to guess the axis units from the variable names and it may be mistaken; you can always change the labels.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @author Kevin Shook
#' @return If successful, and plot=\code{TRUE}, returns a plot of all of the regressions. If successful, and plot=\code{FALSE}, returns a data frame containing the  regression parameters. If unsuccessful, returns \code{FALSE}.
#' @note If successful, exports a data frame with the columns \code{xvals} (secondary data frame), \code{yvals} (primary data frame), \code{first date} (in common data frame), \code{last date} (in common data frame), and \code{slope} for each variable to the regression file. If \code{forceOrigin=FALSE} then the parameters \code{intercept}, and \code{r2} will also be returned. This function will not permit regression on RH values, so convert these to Ea values first.
#' @seealso  \code{\link{impute}}
#' @examples
#' # this would be used to get the regression coefficients used to impute data from Saskatoon
#' # to fill gaps in the Regina data
#' \dontrun{
#' reg <- regress(regina, c(1,2,3), saskatoon, c(1,2,3))}
#' @export

regress <-
function(primaryCRHM, primary.columns=1,
         secondaryCRHM, secondary.columns=1, forceOrigin=FALSE,
         plot=FALSE, quiet=TRUE, logfile=''){
  eol.val <- win.eol()
  # suppress checking of data frame variables used by ggplot2
  secondary <- NULL
  primary <- NULL

  if ((nrow(primaryCRHM) == 0) | (nrow(secondaryCRHM) == 0)){
    cat('Error: missing values\n')
    return(FALSE)
  }

  primaryCRHMname <- deparse(substitute(primaryCRHM))
  secondaryCRHMname <- deparse(substitute(secondaryCRHM))

  # check to see of the same number of columns
  primary.columns.length <- length(primary.columns)
  secondary.columns.length <- length(secondary.columns)

  primaryCRHM.length <- ncol(primaryCRHM)
  secondaryCRHM.length <- ncol(secondaryCRHM)

  if (primary.columns.length != secondary.columns.length){
    cat('Error: different numbers of columns\n')
    return(FALSE)
  }

  if (primary.columns.length > primaryCRHM.length){
    cat('Error: more columns specified than exist in primary data frame\n')
    return(FALSE)
  }

  if (secondary.columns.length > secondaryCRHM.length){
    cat('Error: more columns specified than exist in secondary data frame\n')
    return(FALSE)
  }


  primary.variable.columns <- primary.columns + 1
  primary.variable.names <- names(primaryCRHM)[primary.variable.columns]
  primary.rh.loc.num <- length(grep("rh", tolower(primary.variable.names), fixed=TRUE))
  primary.columns.with.time <- c(1, primary.variable.columns)

  secondary.variable.columns <- secondary.columns + 1
  secondary.variable.names <- names(secondaryCRHM)[secondary.variable.columns]
  secondary.rh.loc.num <- length(grep("rh", tolower(secondary.variable.names), fixed=TRUE))
  secondary.columns.with.time <- c(1, secondary.variable.columns)

  if ((primary.rh.loc.num > 0) | (secondary.rh.loc.num > 0)){
    cat("ERROR: regressions with RH values are not permitted. Convert to ea first and then interpolate.\n")
    return(FALSE)
  }

  primaryCRHM <- primaryCRHM[, primary.columns.with.time]
  secondaryCRHM <- secondaryCRHM[, secondary.columns.with.time]

  if (!plot){
  # since data frames are OK, merge them together for regressions
    merged <- merge(primaryCRHM, secondaryCRHM, by='datetime')
    merged.names <- names(merged)

    # now do regressions
    # regressions are primary vs secondary
    variable.name <- c(0)
    slope <- c(0)
    intercept <- c(0)
    r2 <- c(0)
    xvals <- c(0)
    yvals <- c(0)
    first.date <- c(0)
    last.date <- c(0)

    for (var.num in 1:primary.columns.length){
      primary.col <- 1 + var.num
      secondary.col <- primary.col + primary.columns.length

      # remove missing values
      combined <- na.omit(merged[,c(1, primary.col, secondary.col)])
      first.date[var.num] <- format(combined[1,1], format='%Y-%m-%d %H:%M')
      last.date[var.num] <- format(combined[nrow(combined),1], format='%Y-%m-%d %H:%M')

      y <- combined[, 2]
      x <- combined[, 3]
      if(!forceOrigin)
        l <- lm(y~x)
      else
        l <- lm(y~x-1)

      # get coefficients
      r <- summary(l)["r.squared"][1]
      if(!forceOrigin){
        r2[var.num] <- as.numeric(r[["r.squared"]])
        slope[var.num] <- as.numeric(coefficients(l)[2])
        intercept[var.num] <- as.numeric(coefficients(l)[1])
      }
      else{
        slope[var.num] <- as.numeric(coefficients(l)[1])
      }



      primary.col.name <- names(primary.col)
      secondary.col.name <- names(secondary.col)
      xvals[var.num] <- paste(secondaryCRHMname,'_',
                                           secondary.variable.names[var.num], sep='')
      yvals[var.num] <- paste(primaryCRHMname,'_',
                                           primary.variable.names[var.num], sep='')
    }
  if (!forceOrigin)
    output <- data.frame(as.character(xvals), as.character(yvals), first.date,
                         last.date, slope, intercept, r2)

  else
    output <- data.frame(as.character(xvals), as.character(yvals), first.date,
                         last.date, slope)

  names(output)[1:4] <- c('independent', 'dependent', 'from', 'to')

  }
  else{
  # plot, if selected
    merged <- merge(primaryCRHM, secondaryCRHM, by=c('datetime'))
    for (var.num in 1:primary.columns.length){
      primary.col <- 1 + var.num
      secondary.col <- primary.col + primary.columns.length

      # remove missing values
      combined <- na.omit(merged[,c(1, primary.col, secondary.col)])
      names(combined) <- c('datetime', 'x', 'y')

      # find variable and units
      variable <- primary.variable.names[var.num]

      # find units
      if (stringr::str_detect(variable, stringr::fixed('ea',ignore_case=TRUE)))
        units <- '(kPa)'
      else if (stringr::str_detect(variable, stringr::fixed('t',ignore_case=TRUE)) &
               !(stringr::str_detect(variable, stringr::fixed('p',ignore_case=TRUE))))
        units <- '(C)'
      else if (stringr::str_detect(variable, stringr::fixed('u',ignore_case=TRUE)))
        units <- '(m/s)'
      else if (stringr::str_detect(variable, stringr::fixed('p',ignore_case=TRUE)))
        units<- '(mm)'
      else if (stringr::str_detect(variable, stringr::fixed('q',ignore_case=TRUE)))
        units <- '(W/m2)'
      else
        units <- ''
      combined$variable <- paste(variable, ' ', units, sep='')
      if (var.num == 1)
        plotvals <- combined
      else
        plotvals <- rbind(plotvals, combined)
    }

    # check if plot needs to be faceted
    if (primary.columns.length == 1){
      # don't facet
      xlabel <- paste(secondaryCRHMname, plotvals$variable[1])
      ylabel <- paste(primaryCRHMname, plotvals$variable[1])
      p <- ggplot2::ggplot(plotvals, ggplot2::aes(y,  x)) +
        ggplot2::geom_point()
      if (!forceOrigin)
        p <- p + ggplot2::stat_smooth(method='lm', formula=y~x, colour='red')
      else
        p <- p + ggplot2::stat_smooth(method='lm', formula=y~x-1, colour='red')
      p <- p + ggplot2::xlab(xlabel) + ggplot2::ylab(ylabel)
    }
    else{
      # facet plot
      xlabel <- secondaryCRHMname
      ylabel <- primaryCRHMname
      p <- ggplot2::ggplot(plotvals, ggplot2::aes(y,  x)) +
        ggplot2::geom_point()
      if (!forceOrigin)
        p <- p + ggplot2::stat_smooth(method='lm', formula=y~x, colour='red')
      else
        p <- p + ggplot2::stat_smooth(method='lm', formula=y~x-1, colour='red')

      p <- p + ggplot2::xlab(xlabel) + ggplot2::ylab(ylabel) +
        ggplot2::facet_wrap(~variable, scales="free")
    }

  return(p)
}


  comment <- paste('regress ', primaryCRHMname,'_vs_',secondaryCRHMname,
                   ' primary_variables:', stringr::str_c(primary.variable.names, collapse=','),
                   ' secondary_variables:', stringr::str_c(secondary.variable.names,
                                                 collapse=','), sep='')

  result <- logAction(comment, logfile)


  if(!quiet)
    print(output)

  return(output)
}
