#' Gets the Windows end of line characters
#'
#' @description Finds the end of line (eol) characters required for writing Windows files, such as CRHM obs files. No parameters are required. This is an internal \pkg{CRHMr} function and should \emph{never} need to be called directly.
#' @return Returns the Windows end of line characters (cr and lf).
#' @export
#' @author Kevin Shook
#' @note This function is used to make the creation of Windows-specific files work on all platforms. CRHM requires its obs and project files to use the Windows end of line characters, which are expressed differently on UNIX-based operating systems such as Linux and OSX.
#' @seealso  \code{\link{automatePrj}} \code{\link{setPrjDates}} \code{\link{writeObsFile}} \code{\link{runCRHM}}
#' @examples 
#' windowsEndOfLine <- win.eol()
#' 
win.eol <- function(){
  # set line end characters for all OS
  if (stringr::str_detect(.Platform$OS.type, stringr::fixed('win',ignore_case=TRUE)))
    eol <- '\n'
  else
    eol <- '\r\n'

  return(eol)
}
