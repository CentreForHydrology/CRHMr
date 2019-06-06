#' Removes duplicated datetimes in obs data frame
#'
#' @description Removes duplicated datetime values. Many time series, especially from Environment
#' Canada, may contain duplicated datetimes. This function replaces the duplicated values. It is
#' important to use this function before interpolating or imputing values, and especially before
#' writing the data frame to an obs file.
#' @param obs Required. A \pkg{CRHMr} data frame containing the obs values.
#' @param action Optional. The action used to replace the duplicate values. Must be one of \option{min},
#' \option{max}, \option{mean}, \option{skip}, \option{delete}, \option{split} or \option{second}.
#' Default is \option{mean}.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function
#' in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working
#' interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If there are no duplicates, returns \code{'No duplicates'}. If duplicates exist, and are are
#' successfully removed, the de-duplicated data frame is returned. If the de-duplication is unsuccessful,
#' then an error is returned.
#' @author Kevin Shook
#' @note If action=\option{min}, \option{max}, or \option{mean}, the action function is applied to all of
#' the values for each duplicate datetime. If action=\option{skip} or \option{delete}, then the values of
#' the duplicate datetimes are deleted. If action=\option{split} then the original values are kept, and the duplicate values are written to an obs file. The name of the obs file is the name of the obs variable followed by \code{'_dupes.obs'}. If action=\option{second}, then the second duplicate values are used. This can be useful when dealing with duplicates caused by daylight savings time.
#' @seealso \code{\link{findDupes}}
#' @examples
#' BadLake.deduped <- deDupe(BadLake7376, action='mean')
#' @export


deDupe <- function(obs, action='mean', quiet=TRUE, logfile=""){
  # removes values having duplicated datetime
  if (nrow(obs) == 0){
    stop('Missing obs data frame')
  }
  obsName <- deparse(substitute(obs))

  if (action == '' | is.null(action)){
    stop('Missing action for duplicated obs')
  }

  var.names <- names(obs)
  dupes <- duplicated(obs$datetime)
  dupe.count <- sum(dupes)

  # find duplicates
  if (dupe.count > 0)
    any.dupes <- TRUE
  else{
    any.dupes <- FALSE
  }

  if (!quiet)
    cat(dupe.count,' duplicates found\n', sep='')

  if(any.dupes){
    if ((action == 'mean') | (action == 'max') | (action == 'min')){
      non.datetime <- obs[,-1]
      good <- aggregate(non.datetime, by=list(obs$datetime), FUN=action,
                        na.rm=TRUE, na.action=na.omit)
      names(good) <- var.names
      comment <- paste('deDupe dataframe:', obsName,
                       ' action: ', action, sep='')
      result <- logAction(comment, logfile)
      return(good)
    }

   else if ((action == 'skip') | (action == 'delete')){
      no.dupes <- !duplicated(obs$datetime)
      good <- obs[no.dupes,]
      comment <- paste('deDupe dataframe:', obsName,
                       ' action: ', action, sep='')
      result <- logAction(comment, logfile)
      return(good)
   }
    if ((action == 'second')){
      dupe.rows <- which(dupes)
      datetimes <- obs$datetime[dupe.rows]
      badLocs <- match(datetimes, obs$datetime)
      obs$datetime[badLocs] <- NA
      good <- obs[!is.na(obs$datetime),]
      comment <- paste('deDupe dataframe:', obsName,
                       ' action: ', action, sep='')
      result <- logAction(comment, logfile)
      return(good)
    }
   else if(action == 'split'){
     no.dupes <- !duplicated(obs$datetime)
     good <- obs[no.dupes,]

     dupes <- duplicated(obs$datetime)
     dupes <- obs[dupes,]

     dupefile <- paste(obsName, '_dupes.obs')
     writeObsFile(dupes,  obsfile=dupefile, quiet=TRUE, logfile='')
     comment <- paste('deDupe dataframe:', obsName,
                      ' action: ', action, sep='')
     result <- logAction(comment, logfile)
     return(good)
   }
   else{
     stop("Unknown action for duplicated obs")
   }
  }
  else{
    comment <- paste('deDupe obs:', obsName, sep='')
    result <- logAction(comment, logfile)

    # return either dataframe or graph
    if(result)
      return('No duplicates')
    else
      return(result)
  }
}
