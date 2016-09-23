#' Sets maximum values for an obs data frame
#'
#' @description Tests values in a \pkg{CRHMr} obs data frame to see if they exceed maximum thresholds. Values exceeding the thresholds can be set to either the maximum allowable value or to \code{NA_real_}, which is useful for infilling or imputing values.
#' @param obs Required. The \pkg{CRHMr} obs data frame.
#' @param varcols Optional. A vector containing the columns to be imputed in the obs data frame, not including the datetime. If not specified, defaults to all columns.
#' @param maxvals Optional. A vector containing the maximum permissible values for each of the specified columns. If omitted, the default values are used.
#' @param actions  Optional. A vector containing the methods to be used for replacing values that exceed the maximum threshold values. Currently supported actions are \option{max} (using the threshold) and \option{NA} (inserting \code{NA_real_} values). The default is \option{max}. If fewer actions are specified than variables, then the actions are recycled.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If successful, returns a modified version of the obs data frame. The values exceeding the specified \code{max} values are replaced by either \code{NA_real_} or the threshold \code{max} values. If unsuccessful, returns the value \code{FALSE}.
#' @author Kevin Shook
#' @note The default threshold values are \tabular{ll}{t\tab 40 C\cr ea \tab 3.5 kPa\cr rh \tab 100 percent\cr ppt\tab 100 mm\cr p\tab 20 mm\cr u \tab 10 m/s\cr SunAct\tab 18 hr\cr qsi\tab 1262 W/m\eqn{^2}{^2}\cr qso\tab 1000 W/m\eqn{^2}{^2}\cr qn\tab 250 W/m\eqn{^2}{^2}\cr}

#' @seealso  \code{\link{minObs}}
#' @examples
#' # use all of the default values
#' bad.max <- maxObs(BadLake7376)
#' summary(bad.max)
#' # use specified columns with default max values, replace with 'NA' values
#' bad.max2 <- maxObs(BadLake7376, varcols=c(1,2), actions='NA')
#' # use specfied columns with specified max values and actions
#' bad.max3 <- maxObs(BadLake7376, maxvals=c(35, 10), varcols=c(1,3), actions=c('NA', 'max'))
#' summary(bad.max3)
#' @export

maxObs <- function(obs, varcols='', maxvals='', actions='max',
                   quiet=TRUE,  logfile=''){
  # sets obs values to max values

  # defaults
  t.max <- 40
  ea.max <- 3.5
  rh.max <- 100
  ppt.max <- 100
  p.max <- 20
  SunAct.max <- 18
  qsi.max <- 1262
  qso.max <- 1000
  qn.max <- 250
  u.max <- 10

  if (nrow(obs) == 0){
    cat('Error: missing data values\n')
    return(FALSE)
  }

  obsName <- deparse(substitute(obs))

  # get action for each column
  na.cols <- which(stringr::str_detect(actions, stringr::fixed('na',ignore_case=TRUE))) + 1
  max.cols <- which(stringr::str_detect(actions, stringr::fixed('max',ignore_case=TRUE))) + 1

  # check permutations of parameters

  if (mode(varcols) == 'character'){
      varcols <- seq(2:ncol(obs))
  }
  else{
    # select specified cols, otherwise, use all columns
    obs <- obs[,c(1, (varcols+1))]
  }

  obs.names <- names(obs)[-1]

  if (!quiet)
    cat('Variables:', obs.names, '\n', sep=' ')

  if (mode(maxvals) == 'character'){
    # no max values specified, use default max values
    # assign max values by column name

    if (length(actions) > 1)
      actions <- actions[1]

    # find columns

    ea.cols <- which(stringr::str_detect(obs.names, stringr::fixed('ea.'))) + 1
    rh.cols <- which(stringr::str_detect(obs.names, stringr::fixed('rh.'))) + 1
    ppt.cols <- which(stringr::str_detect(obs.names, stringr::fixed('ppt.'))) + 1
    SunAct.cols <- which(stringr::str_detect(obs.names, stringr::fixed('SunAct.'))) + 1

    p.cols <- which(stringr::str_detect(obs.names, stringr::fixed('p.'))) + 1
    qsi.cols <- which(stringr::str_detect(obs.names, stringr::fixed('qsi.'))) + 1
    qso.cols <- which(stringr::str_detect(obs.names, stringr::fixed('qso.'))) + 1
    qn.cols <- which(stringr::str_detect(obs.names, stringr::fixed('qn.'))) + 1
    u.cols <- which(stringr::str_detect(obs.names, stringr::fixed('u.'))) + 1

    # exclude SunAct and ppt columns from t columns
    sun <- stringr::str_detect(obs.names, 'SunAct.')
    t <- stringr::str_detect(obs.names, 't.')
    ppt <- stringr::str_detect(obs.names, 'ppt.')
    t1 <- xor(t, sun)
    t2 <- xor(t1, ppt)
    t.cols <- which(t2) + 1

    # now apply actions
    if (actions == 'max'){
      if (length(t.cols) > 0)
        obs[, t.cols] <- pmin(obs[, t.cols], t.max)
      if (length(ea.cols) > 0)
        obs[, ea.cols] <- pmin(obs[, ea.cols], ea.max)
      if (length(rh.cols) > 0)
        obs[, rh.cols] <- pmin(obs[, rh.cols], rh.max)
      if (length(ppt.cols) > 0)
        obs[, ppt.cols] <- pmin(obs[, ppt.cols], ppt.max)
      if (length(p.cols) > 0)
       obs[, p.cols] <- pmin(obs[, p.cols], p.max)
      if (length(qsi.cols) > 0)
        obs[, qsi.cols] <- pmin(obs[, qsi.cols], qsi.max)
      if (length(qso.cols) > 0)
        obs[, qso.cols] <- pmin(obs[, qso.cols], qso.max)
      if (length(qn.cols) > 0)
        obs[, qn.cols] <- pmin(obs[, qn.cols], qn.max)
      if (length(u.cols) > 0)
        obs[, u.cols] <- pmin(obs[, u.cols], u.max)
      if (length(SunAct.cols) > 0)
        obs[, SunAct.cols] <- pmin(obs[, SunAct.cols], SunAct.max)
    }
    else{
      # actions == NA
      # replace with NA values
      for (t.col in t.cols){
        rows <- (obs[, t.col] > t.max) & (!is.na(obs[, t.col] > t.max))
        obs[rows, t.col] <- NA_real_
      }

      for (ea.col in ea.cols){
        rows <- (obs[, ea.col] > ea.max) & (!is.na(obs[, ea.col] > ea.max))
        obs[rows, ea.col] <- NA_real_
      }

      for (rh.col in rh.cols){
        rows <- (obs[, rh.col] > rh.max) & (!is.na(obs[, rh.col] > rh.max))
        obs[rows, rh.col] <- NA_real_
      }

      for (ppt.col in ppt.cols){
        rows <- (obs[, ppt.col] > ppt.max) & (!is.na(obs[, ppt.col] > ppt.max))
        obs[rows, ppt.col] <- NA_real_
      }

      for (p.col in p.cols){
        rows <- (obs[, p.col] > p.max) & (!is.na(obs[, p.col] > p.max))
        obs[rows, p.col] <- NA_real_
      }

      for (qsi.col in qsi.cols){
        rows <- (obs[, qsi.col] > qsi.max) & (!is.na(obs[, qsi.col] > qsi.max))
        obs[rows, qsi.col] <- NA_real_
      }

      for (qso.col in qso.cols){
        rows <- (obs[, qso.col] > qso.max) & (!is.na(obs[, qso.col] > qso.max))
        obs[rows, qso.col] <- NA_real_
      }

      for (qn.col in qn.cols){
        rows <- (obs[, qn.col] > qn.max) & (!is.na(obs[, qn.col] > qn.max))
        obs[rows, qn.col] <- NA_real_
      }

      for (u.col in u.cols){
        rows <- (obs[, u.col] > u.max) & (!is.na(obs[, u.col] > u.max))
        obs[rows, u.col] <- NA_real_
      }

      for (SunAct.col in SunAct.cols){
        rows <- (obs[, SunAct.col] > SunAct.max) & (!is.na(obs[, SunAct.col] > SunAct.max))
        obs[rows, SunAct.col] <- NA_real_
      }
    }
  }
  else{
    # have specified max values, make sure columns are also specified
    if (length(varcols) == 0){
      cat('Error: need to specify columns\n')
      return(FALSE)
    }

    # use specified max values for specified columns
    if (length(actions) < length(varcols)){
      # replicate
      actions <- rep(actions, len=length(varcols))
    }

    # find actions to be performed
    # get order of actions
    na.locs <- which(stringr::str_detect(actions, stringr::fixed('na',ignore_case=TRUE)))
    max.locs <- which(stringr::str_detect(actions, stringr::fixed('max',ignore_case=TRUE)))

    # now assign column numbers
    na.cols <- varcols[na.locs]
    max.cols <- varcols[max.locs]



    for (colloc in 1:length(varcols)){
      colnum <- varcols[colloc]
      if (colnum %in% na.cols){
        if (length(maxvals) > 1){
          maxval <- maxvals[colloc]
          rows <- (obs[, colloc+1] > maxval) & (!is.na(obs[, colloc+1] > maxval))
          obs[rows, colnum+1] <- NA_real_
        }
        else{
          maxval<- maxvals
          rows <- (obs[, colloc+1] > maxval) & (!is.na(obs[, colloc+1] > maxval))
          obs[rows, colloc+1] <- NA_real_
        }
      }
      if (colnum %in% max.cols){
        if (length(maxvals) > 1){
          maxval <- maxvals[colloc]
          rows <- (obs[, colloc+1] > maxval) & (!is.na(obs[, colloc+1] > maxval))
          obs[rows, colloc+1] <- maxval
        }

        else{
          maxval <- maxvals
          rows <- (obs[, colloc+1] > maxval) & (!is.na(obs[, colloc+1] > maxval))
          obs[rows, colloc+1] <- maxval
        }
      }
    }

  }

  # log to file
  comment <- paste('maxObs dataframe:', obsName, sep='')
  result <- logAction(comment, logfile)
  if (result)
    return (obs)
  else
    return(result)

}
