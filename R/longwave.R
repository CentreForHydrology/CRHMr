#' Calculates incoming longwave radiation
#' @description Calculates the incoming longwave radiation from the specified emissivity and air temperatures.
#' This is an internal \pkg{CRHMr} function and should \emph{never} need to be called directly, as it does \emph{NO} data checking.
#' @param em Required. Emissivity (0-1).
#' @param tK Required. Air temperatures in K.
#'
#' @return Returns the longwave radiation in W/m\eqn{^2}{^2}.
#' @export
#' @author Kevin Shook
#' @seealso \code{\link{distributeQli}} \code{\link{emissivity}}
#' @examples
#' longwave(0.2, 293)
longwave <- function(em, tK){
  stefanBoltzmann <- 5.670367e-8 #W m-2 K-4
  lw <- (stefanBoltzmann * em* tK ^ 4)
  return(lw)
}
