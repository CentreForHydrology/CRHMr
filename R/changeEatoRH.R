#' Changes Ea values to RH in a CRHM data frame
#'
#' @description This function converts values of Ea to RH. Note that the specified obs data frame must contain both Ea and air temperatures. For safety, \pkg{CRHMr} does not permit values of both RH and Ea in a data frame. The names of the variables containing air temperature and ea values must be of the form \code{t.x} and \code{ea.x}, respectively,  where \code{x} is an number, even if the column numbers are specified.
#' @param obs Required. A \pkg{CRHMr} data frame of observations.
#' @param t.cols Optional. A vector containing the column numbers (not including the datetime) holding the air temperatures. If no columns are specified then the locations of the temperatures are guessed from the column names. The air temperatures must be in \eqn{^\circ}{ }C.
#' @param ea.cols Optional. A vector containing the column numbers (not including the datetime) holding the vapour pressures. If no columns are specified then the locations of the vapour pressures are guessed from the column name. The ea values must be in kPa.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If successful, the function returns the original data frame with the ea columns converted to RH values in perceent. If unsuccessful, it returns the value \code{FALSE}.
#' @author Kevin Shook
#' @seealso \code{\link{changeRHtoEa}}
#' @examples
#' # First, convert RH values to Ea
#' BadLake7376.ea <- changeRHtoEa(BadLake7376)
#' # now convert Ea values back to RH
#' BadLake7376.rh <- changeEatoRH(BadLake7376.ea)
#' @export

changeEatoRH <-
function(obs, t.cols=1, ea.cols=1, quiet=TRUE, logfile=''){
  # changes columns of ea values to RH, which are used for output
  
  # the code will look for variable names containing 't' and 'ea'
  # if the columns are not specified explicitly
  
  obsName <- deparse(substitute(obs))
  
  # see if it needs to be done
  obs.names <- names(obs)[-1]
  obs.names.lowercase <- tolower(obs.names)

  # look for t and rh data
  t.loc <- which(stringr::str_detect(obs.names.lowercase, stringr::fixed('t.')))
  ppt.loc <- which(stringr::str_detect(obs.names.lowercase, stringr::fixed('ppt.')))
  act.loc <- which(stringr::str_detect(obs.names.lowercase, stringr::fixed('act.')))
  
  # exclude PPT
  if (sum(ppt.loc) > 0)
    ok <- obs.names.lowercase[-ppt.loc]
  else
    ok <- obs.names.lowercase
  
  # exclude SunAct
  if (sum(act.loc) > 0)
    ok <- ok[-act.loc]
  
  t.loc <- which(stringr::str_detect(ok, stringr::fixed('t.')))
  ea.loc <- which(stringr::str_detect(obs.names.lowercase, stringr::fixed('ea.')))
  
  if ((t.cols == ea.cols) & (length(t.loc) == 0) | (length(ea.loc) == 0)){
    cat('T and/or Ea missing\n')
    return(FALSE)  
  }
  
  if (length(t.cols) != length(ea.cols)){
    cat('Unequal numbers of T and Ea values\n')
    return(FALSE)  
  }  
  
  if (t.cols != ea.cols){
    # locations are specified
    t.cols <- t.cols + 1
    ea.cols <- ea.cols + 1
  }
  else{
    t.cols <- t.loc + 1
    ea.cols <- ea.loc + 1 
  }
  
  t.vals <- obs[,t.cols]
  t.vals.na <- is.na(t.vals)
  ea.vals <- obs[,ea.cols]
  ea.vals.na <- is.na(ea.vals)
  bad.vals <- t.vals.na | ea.vals.na
  good.vals <- !bad.vals
  
  # call rh function
  rh.vals <- ea.vals
  rh.vals[good.vals] <- mapply(FUN='vp2rh', t.vals[good.vals], ea.vals[good.vals])
  rh.vals[bad.vals] <- NA_real_
  rh.vals <- as.data.frame(rh.vals)
  rh.count <- ncol(rh.vals)
  
  rh.nums <- seq(1:rh.count)
  rh.names <- paste('rh.',rh.nums, sep='')
  
  # replace ea values with rh
  obs[ea.cols] <- rh.vals
  names(obs)[ea.cols] <- rh.names
  
  
  # output info to screen and write to log file
  obs.info <- CRHM_summary(obs)
  if (!quiet)
    print(obs.info)
  
  comment <- paste('changeEAtoRH dataframe: ', obsName, sep='')
  result <- logAction(comment, logfile)
  if (result)
    return(obs)
  else
    return(result)
}
