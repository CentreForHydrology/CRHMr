#' Sets minimum values for an obs data frame
#'
#' @description Tests values in a \pkg{CRHMr} obs data frame to see if they exceed minimum thresholds. Values exceeding the thresholds can be set to either the minimum allowable value or to \code{NA_real}, which is useful for infilling or imputing values.
#' @param obs Required. The \pkg{CRHMr} obs data frame.
#' @param varcols Optional. A vector containing the columns to be imputed in the obs data frame, not including the datetime. If not specified, defaults to all columns.
#' @param minvals Optional. A vector containing the minimum permissible values for each of the specified columns. If omitted, the default values are used.
#' @param actions  Optional. A vector containing the methods to be used for replacing values that exceed the minimum threshold values. Currently supported actions are \option{min} (using the threshold) and \option{NA} (using \code{NA_real_} values). The default is \option{min}. If fewer actions are specified than variables, then the actions are recycled.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If successful, returns a modified version of the obs data frame. The values smaller than the specified \code{min} values are replaced by either \code{NA_real_} or the threshold \code{min} values. If unsuccessful, returns the value FALSE.
#' @author Kevin Shook
#' @note The default threshold values are \tabular{ll}{t\tab -40 C\cr ea \tab 0.01 kPa\cr rh \tab 0.5 percent\cr ppt\tab 0 mm\cr p\tab 0 mm\cr u \tab 0 m/s\cr SunAct\tab 0 hr\cr qsi\tab 0 W/m\eqn{^2}{^2}\cr qso\tab 0 W/m\eqn{^2}{^2}\cr qn\tab -60 W/m\eqn{^2}{^2}\cr}

#' @seealso  \code{\link{minObs}}
#' @examples
#' # use all of the default values
#' bad.min <- minObs(BadLake7376)
#' # use specified columns with default min values, replace with 'NA' values
#' bad.min2 <- minObs(BadLake7376, varcols=c(1,2), actions='NA')
#' # use specfied columns with specified min values and actions
#' bad.min3 <- minObs(BadLake7376, minvals=c(-30, 22), varcols=c(1,2), actions=c('min', 'NA'))
#' @export

minObs <- function(obs, varcols='', minvals='', actions='min',
                   quiet=TRUE,  logfile=''){
  # sets obs values to min values

  # defaults
  t.min <- -40
  ea.min <- 0.01
  rh.min <- 0.05
  ppt.min <- 0
  p.min <- 0
  qsi.min <- 0
  qso.min <- 0
  qn.min <- -60
  u.min <- 0
  SunAct.min <- 0

  if (nrow(obs) == 0){
    cat('Error: missing data values\n')
    return(FALSE)
  }

  obsName <- deparse(substitute(obs))

  # get action for each column
  na.cols <- which(stringr::str_detect(actions, stringr::fixed('na',ignore_case=TRUE))) + 1
  min.cols <- which(stringr::str_detect(actions, stringr::fixed('min',ignore_case=TRUE))) + 1

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

  if (mode(minvals) == 'character'){
    # no min values specified, use default min values
    # assign min values by column name

    if (length(actions) > 1)
      actions <- actions[1]

    # find columns

    ea.cols <- which(stringr::str_detect(stringr::str_to_lower(obs.names), stringr::fixed('ea.'))) + 1
    rh.cols <- which(stringr::str_detect(stringr::str_to_lower(obs.names), stringr::fixed('rh.'))) + 1
    ppt.cols <- which(stringr::str_detect(stringr::str_to_lower(obs.names), stringr::fixed('ppt.'))) + 1
    SunAct.cols <- which(stringr::str_detect(stringr::str_to_lower(obs.names), stringr::fixed('sunact.'))) + 1

    p.cols <- which(stringr::str_detect(stringr::str_to_lower(obs.names), stringr::fixed('p.'))) + 1
    qsi.cols <- which(stringr::str_detect(stringr::str_to_lower(obs.names), stringr::fixed('qsi.'))) + 1
    qso.cols <- which(stringr::str_detect(stringr::str_to_lower(obs.names), stringr::fixed('qso.'))) + 1
    qn.cols <- which(stringr::str_detect(stringr::str_to_lower(obs.names), stringr::fixed('qn.'))) + 1
    u.cols <- which(stringr::str_detect(stringr::str_to_lower(obs.names), stringr::fixed('u.'))) + 1

    # exclude SunAct and ppt columns from t columns
    sun <- stringr::str_detect(stringr::str_to_lower(obs.names), 'sunact.')
    t <- stringr::str_detect(stringr::str_to_lower(obs.names), 't.')
    ppt <- stringr::str_detect(stringr::str_to_lower(obs.names), 'ppt.')
    t1 <- xor(t, sun)
    t2 <- xor(t1, ppt)
    t.cols <- which(t2) + 1

    # now apply actions
    if (actions == 'min'){
      if (length(t.cols) > 0)
        obs[, t.cols] <- pmax(obs[, t.cols], t.min)
      if (length(ea.cols) > 0)
        obs[, ea.cols] <- pmax(obs[, ea.cols], ea.min)
      if (length(rh.cols) > 0)
        obs[, rh.cols] <- pmax(obs[, rh.cols], rh.min)
      if (length(ppt.cols) > 0)
        obs[, ppt.cols] <- pmax(obs[, ppt.cols], ppt.min)
      if (length(p.cols) > 0)
        obs[, p.cols] <- pmax(obs[, p.cols], p.min)
      if (length(qsi.cols) > 0)
        obs[, qsi.cols] <- pmax(obs[, qsi.cols], qsi.min)
      if (length(qso.cols) > 0)
        obs[, qso.cols] <- pmax(obs[, qso.cols], qso.min)
      if (length(qn.cols) > 0)
        obs[, qn.cols] <- pmax(obs[, qn.cols], qn.min)
      if (length(u.cols) > 0)
        obs[, u.cols] <- pmax(obs[, u.cols], u.min)
      if (length(SunAct.cols) > 0)
        obs[, SunAct.cols] <- pmax(obs[, SunAct.cols], SunAct.min)
    }
    else{
      # actions == NA
      # replace with NA values
      for (t.col in t.cols){
        rows <- (obs[, t.col] < t.min) & (!is.na(obs[, t.col] < t.min))
        obs[rows, t.col] <- NA_real_
      }

      for (ea.col in ea.cols){
        rows <- (obs[, ea.col] < ea.min) & (!is.na(obs[, ea.col] < ea.min))
        obs[rows, ea.col] <- NA_real_
      }

      for (rh.col in rh.cols){
        rows <- (obs[, rh.col] < rh.min) & (!is.na(obs[, rh.col] < rh.min))
        obs[rows, rh.col] <- NA_real_
      }

      for (ppt.col in ppt.cols){
        rows <- (obs[, ppt.col] < ppt.min) & (!is.na(obs[, ppt.col] < ppt.min))
        obs[rows, ppt.col] <- NA_real_
      }

      for (p.col in p.cols){
        rows <- (obs[, p.col] < p.min) & (!is.na(obs[, p.col] < p.min))
        obs[rows, p.col] <- NA_real_
      }

      for (qsi.col in qsi.cols){
        rows <- (obs[, qsi.col] < qsi.min) & (!is.na(obs[, qsi.col] < qsi.min))
        obs[rows, qsi.col] <- NA_real_
      }

      for (qso.col in qso.cols){
        rows <- (obs[, qso.col] < qso.min) & (!is.na(obs[, qso.col] < qso.min))
        obs[rows, qso.col] <- NA_real_
      }

      for (qn.col in qn.cols){
        rows <- (obs[, qn.col] < qn.min) & (!is.na(obs[, qn.col] < qn.min))
        obs[rows, qn.col] <- NA_real_
      }

      for (u.col in u.cols){
        rows <- (obs[, u.col] < u.min) & (!is.na(obs[, u.col] < u.min))
        obs[rows, u.col] <- NA_real_
      }

      for (SunAct.col in SunAct.cols){
        rows <- (obs[, SunAct.col] < SunAct.min) & (!is.na(obs[, SunAct.col] < SunAct.min))
        obs[rows, SunAct.col] <- NA_real_
      }
    }
  }
  else{
    # have specified min values, make sure columns are also specified
    if (length(varcols) == 0){
      cat('Error: need to specify columns\n')
      return(FALSE)
    }

    # use specified min values for specified columns
    if (length(actions) < length(varcols)){
      # replicate
      actions <- rep(actions, len=length(varcols))
    }

    # find actions to be performed
    # get order of actions
    na.locs <- which(stringr::str_detect(actions, stringr::fixed('na',ignore_case=TRUE)))
    min.locs <- which(stringr::str_detect(actions, stringr::fixed('min',ignore_case=TRUE)))

    # now assign column numbers
    na.cols <- varcols[na.locs]
    min.cols <- varcols[min.locs]



    for (colloc in 1:length(varcols)){
      colnum <- varcols[colloc]
      if (colnum %in% na.cols){
        if (length(minvals) > 1){
          minval <- minvals[colloc]
          rows <- (obs[, colloc+1] < minval) & (!is.na(obs[, colloc+1] < minval))
          obs[rows, colnum+1] <- NA_real_
        }
        else{
          minval<- minvals
          rows <- (obs[, colloc+1] < minval) & (!is.na(obs[, colloc+1] < minval))
          obs[rows, colloc+1] <- NA_real_
        }
      }
      if (colnum %in% min.cols){
        if (length(minvals) > 1){
          minval <- minvals[colloc]
          rows <- (obs[, colloc+1] < minval) & (!is.na(obs[, colloc+1] < minval))
          obs[rows, colloc+1] <- minval
        }

        else{
          minval <- minvals
          rows <- (obs[, colloc+1] < minval) & (!is.na(obs[, colloc+1] < minval))
          obs[rows, colloc+1] <- minval
        }
      }
    }
  }
  # log to file
  comment <- paste('minObs dataframe:', obsName, sep='')
  result <- logAction(comment, logfile)
  if (result)
    return (obs)
  else
    return(result)
}
