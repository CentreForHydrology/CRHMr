#' @export
win.eol <- function(){
  # set line end characters for all OS
  if (str_detect(.Platform$OS.type, fixed('win',ignore_case=TRUE)))
    eol <- '\n'
  else
    eol <- '\r\n'

  return(eol)
}
