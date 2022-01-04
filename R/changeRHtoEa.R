#' Changes RH values to Ea in a CRHM dataframe
#'
#' @description This function converts values of RH to Ea. \pkg{CRHMr} does not allow interpolation or imputation of RH values, so you must convert RH values to Ea before infilling. Note that the specified obs dataframe must contain both RH and air temperatures. For safety, \pkg{CRHMr} does not permit values of both RH and Ea in a data frame. The names of the variables containing air temperature and RH values must be of the form \code{t.x} and \code{rh.x}, respectively, where \code{x} is an number, even if the column numbers are specified.
#' @param obs Required. A \pkg{CRHMr} data frame of observations.
#' @param t.cols Optional. A vector containing the column numbers (not including the datetime) holding the air temperatures. If no columns are specified then the locations of the temperatures are guessed from the column names. The air temperatures must be in \eqn{^\circ}{ }C.
#' @param rh.cols Optional. A vector containing the column numbers (not including the datetime) holding the relative humidities. If no columns are specified then the locations of the RH values are guessed from the column names.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If successful, the function returns the original data frame with the RH columns converted to ea values in kPa. If unsuccessful, it returns the value \code{FALSE}.
#' @author Kevin Shook
#' @seealso  \code{\link{changeEatoRH}}
#' @examples
#' BadLake7376.ea <- changeRHtoEa(BadLake7376)
#' @importFrom stringr str_detect fixed
#' @export

changeRHtoEa <-
function(obs, t.cols=1, rh.cols=1, quiet=TRUE, logfile=''){
  # changes columns of RH values to ea, which are used for interpolation/imputation

  # the code will look for variable names containing 'T' and 'RH'
  # if the columns are not specified explicitly
  obsName <- deparse(substitute(obs))

  # see if it needs to be done
  obs.names <- names(obs)[-1]
  obs.names.lowercase <- tolower(obs.names)
  ea.loc <-which(str_detect(obs.names.lowercase, fixed('ea.')))
  if (length(ea.loc) > 0){
    cat('Error ea values already present\n')
    return(FALSE)
  }

  # look for t and rh data

  t.loc <- which(str_detect(obs.names.lowercase, fixed('t.')))
  ppt.loc <- which(str_detect(obs.names.lowercase, fixed('ppt.')))
  act.loc <- which(str_detect(obs.names.lowercase, fixed('act.')))

  # exclude PPT
  if (sum(ppt.loc) > 0)
    ok <- obs.names.lowercase[-ppt.loc]
  else
    ok <- obs.names.lowercase

  # exclude SunAct
  if (sum(act.loc) > 0)
    ok <- ok[-act.loc]

  t.loc <- which(str_detect(ok, fixed('t.')))
  rh.loc <- which(str_detect(obs.names.lowercase, fixed('rh.')))

  if ((t.cols == rh.cols) & (length(t.loc) == 0) | (length(rh.loc) == 0)){
    cat('T and/or RH missing\n')
    return(FALSE)
  }

  if (length(t.cols) != length(rh.cols)){
    cat('Unequal numbers of T and RH values\n')
    return(FALSE)
  }

  if (t.cols != rh.cols){
   # locations are specified
   t.cols <- t.cols + 1
   rh.cols <- rh.cols + 1
  }
  else{
    t.cols <- t.loc + 1
    rh.cols <- rh.loc + 1
  }

  t.vals <- obs[,t.cols]
  t.vals.na <- is.na(t.vals)
  rh.vals <- obs[,rh.cols]
  rh.vals.na <- is.na(rh.vals)
  bad.vals <- t.vals.na | rh.vals.na
  good.vals <- !bad.vals
  # call ea function
  ea.vals <- rh.vals
  ea.vals[good.vals] <- mapply(FUN='rh2vp', t.vals[good.vals], rh.vals[good.vals])
  ea.vals[bad.vals] <- NA_real_
  ea.vals <- as.data.frame(ea.vals)
  ea.count <- ncol(ea.vals)

  ea.nums <- seq(1:ea.count)
  ea.names <- paste('ea.',ea.nums, sep='')

  # replace RH values with ea
  obs[rh.cols] <-ea.vals
  names(obs)[rh.cols] <- ea.names


  # output info to screen and write to log file
  obs.info <- CRHM_summary(obs)
  if (!quiet)
    print(obs.info)

  comment <- paste('changeRHtoE dataframe:', obsName, sep='')
  result <- logAction(comment, logfile)
  if (result)
    return(obs)
  else
    return(result)
}
