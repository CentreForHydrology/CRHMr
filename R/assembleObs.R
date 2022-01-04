#' Assembles two CRHM obs data frames
#'
#' @description This function joins two data frames of CRHM obs. The data frames can have different columns (it is assumed that they will), but should have some dates in common. Both data frames must have the same time step.
#' @param obs1 Required. The first \pkg{CRHMr} data frame of obs values.
#' @param obs2 Required. The second obs data frame.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If successful, returns the merges values of both data frames. Note that where the datetimes in the data frames are not the same, the merged values will be set to \code{NA_real_}. If unsuccessful, returns the value \code{FALSE}.
#' @author Kevin Shook

#' @examples
#' \dontrun{
#' MSC.trh <- assembleObs(MSC.t, MSC.rh)}
#' @importFrom stringr str_c
#' @export


assembleObs <- function(obs1, obs2, quiet=TRUE, logfile=''){
  # assembles dissimilar obs data frames to produce a single obs data frame

  # check to see if worth doing
  if (nrow(obs1) == 0){
    stop('Missing first obs values')
  }

  if (nrow(obs2) == 0){
    stop('Missing second obs values\n')
  }

  obs1Name <- deparse(substitute(obs1))
  obs2Name <- deparse(substitute(obs2))

  # check time steps of both obs files
  ts1 <- timestep.hours(obs1$datetime[1], obs1$datetime[2])
  ts2 <- timestep.hours(obs2$datetime[1], obs2$datetime[2])

  if (ts1 != ts2){
    stop('Files have different time steps')
  }

  # now that we have 2 OK obs, append them
  obs1.names <- names(obs1)[1]
  obs2.names <- names(obs2)[1]
  merged <- merge(obs1, obs2, by='datetime', all=TRUE)

  new.data.info <- CRHM_summary(merged)
  if (!quiet)
    print(new.data.info)

  comment <- paste('assembleObs obs1:', obs1Name,
                   ' obs1_variables:', str_c(obs1.names,
                                                collapse=','),
                   ' obs2:', obs2Name,
                   ' obs2_variables:', str_c(obs2.names,
                                                  collapse=','),
                   sep='')

  result <- logAction(comment, logfile)

  if (result)
    return(merged)
  else
    return(result)

}
