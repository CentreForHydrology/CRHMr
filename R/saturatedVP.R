#' Converts calculates saturated vapour pressure from air temperature
#'
#' @description This is an internal \pkg{CRHMr} function and should normally not need to be called directly.
#' @param airTemp Required. Air temperature in \eqn{^\circ}{ }C.
#'
#' @return If successful, returns the saturated vapour pressure in kPa. If unsuccessful, returns the value \code{FALSE}.
#' @author Kevin Shook
#' @export
#'
#' @examples
#' saturatedVP(-5)
saturatedVP <- function(airTemp){
  #check parameter
  
  if (length(airTemp) == 0){
    cat('Error: air temps missing\n')
    return(FALSE)  
  }

  if(max(airTemp) > 200){
    cat('Error: air temps must be in C NOT K\n')
    return(FALSE) 
  }  

  if (length(airTemp) == 1){
    if (airTemp <= 0)
      estar <- 0.611 * exp((21.88 * airTemp) / ( airTemp + 265.5))
    else
      estar <- 0.611 * exp((17.27 * airTemp) / ( airTemp + 237.3))
  }
  else{
    estar <- 0.611 * exp((21.88 * airTemp) / ( airTemp + 265.5))
    estar[airTemp > 0] <- 0.611 * exp((17.27 * airTemp[airTemp > 0]) / 
                                        ( airTemp[airTemp > 0] + 237.3))
  }

  return(estar)
}