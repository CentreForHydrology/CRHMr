#' Parses a string containing several sub-strings
#'
#' @description This is an internal \pkg{CRHMr} function and should normally not need to be called directly. It is used for reading data from messy files such as CRHM projects.
#' @param textString Required. A character string containing strings separated by any number of spaces.
#'
#' @return Returns a character vector.
#' @author Kevin Shook
#' @export
#'
#' @examples
#' parseText(' red  green    blue      black')
parseText <- function(textString){
  #
  # returns a vector

  # remove padding
  textString <- stringr::str_trim(textString)

  # swap tabs for chracters
  textString <- stringr::str_replace_all(textString, stringr::fixed('\t'), ' ' )
  double.spaces <- stringr::str_detect(textString, '  ')
  # replace all double spaces with single spaces

  while(double.spaces){
    textString <- stringr::str_replace_all(textString, '  ', ' ' )
    double.spaces <- stringr::str_detect(textString, '  ')
  }

  texts <- unlist(stringr::str_split(textString, ' '))
  return(texts)
}
