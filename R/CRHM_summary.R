#' Summarizes a \pkg{CRHMr} data frame
#'
#' @description Summarizes the values in a \pkg{CRHMr} data frame. This is an internal
#' \pkg{CRHMr} function and should \emph{never} need to be called directly.
#' @param CRHMdataframe Required. Name of the \pkg{CRHMr} data frame to be summarized.
#'
#' @return If successful, returns a data frame containing the number of rows,
#' complete rows, starting and ending datetimes, and the variables in the specified
#' data frame. If unsuccessful, returns the value \code{FALSE}.
#' @author Kevin Shook
#' @importFrom stringr str_c
#' @importFrom stats na.omit
#' @export
#' @keywords internal
#' @examples \dontrun{
#' summary <- CRHMsummary(BadLake7376)}
#'
CRHM_summary <- function(CRHMdataframe){
  names <- c(0)
  values <- c(0)
  if (nrow(CRHMdataframe) == 0)
  {
    cat('Missing dataset\n')
    return(FALSE)
  }
  else{
    # check for missing values (OK for imputing to empty data frame)
    goodvals <- na.omit(CRHMdataframe)
    if (nrow(goodvals) == 0) {
      numrows <- nrow(CRHMdataframe)
      variables <- names(CRHMdataframe)[-1]
      variable.count <- length(variables)
      names[1] <- 'Time step (hours):'
      values[1] <- timestep.hours(CRHMdataframe[1,1], CRHMdataframe[2,1])
      names[2] <- 'Total rows:'
      values[2] <- numrows
      names[3] <- 'Complete rows:'
      values[3] <- 0
      names[4] <- 'From:'
      values[4] <- format(CRHMdataframe[1,1], format = '%Y-%m-%d')
      names[5] <- 'To:'
      values[5] <- format(CRHMdataframe[numrows,1], format = '%Y-%m-%d')
      names[6] <- 'First complete date:'
      values[6] <- 'NA'
      names[7] <- 'Last complete date:'
      values[7] <- 'NA'
      names[8] <- 'Number of variables:'
      values[8] <- variable.count
      names[9] <- 'Variable names:'
      values[9] <- str_c(variables, collapse = ' ')
      complete.summary <- data.frame(names, values)
      names(complete.summary) <- c('summary','value')
      return(complete.summary)
    }
    else{
      numrows <- nrow(CRHMdataframe)
      clean <- na.omit(CRHMdataframe)
      clean.numrows <- nrow(clean)
      variables <- names(CRHMdataframe)[-1]
      variable.count <- length(variables)
      names[1] <- 'Time step (hours):'
      values[1] <- timestep.hours(CRHMdataframe[1,1], CRHMdataframe[2,1])
      names[2] <- 'Total rows:'
      values[2] <- nrow(CRHMdataframe)
      names[3] <- 'Complete rows:'
      values[3] <- nrow(clean)
      names[4] <- 'From:'
      values[4] <- format(CRHMdataframe[1,1], format = '%Y-%m-%d')
      names[5] <- 'To:'
      values[5] <- format(CRHMdataframe[numrows,1], format = '%Y-%m-%d')
      names[6] <- 'First complete date:'
      values[6] <- format(clean[1,1], format = '%Y-%m-%d')
      names[7] <- 'Last complete date:'
      values[7] <- format(clean[clean.numrows,1], format = '%Y-%m-%d')
      names[8] <- 'Number of variables:'
      values[8] <- variable.count
      names[9] <- 'Variable names:'
      values[9] <- str_c(variables, collapse = ' ')
      complete.summary <- data.frame(names, values)
      names(complete.summary) <- c('summary','value')
      return(complete.summary)
    }
  }
}
