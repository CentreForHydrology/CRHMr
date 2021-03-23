#' Parses a string containing several sub-strings
#'
#' @description This is an internal \pkg{CRHMr} function and should normally not
#' need to be called directly. It is used for reading data from messy files such
#' as CRHM projects.
#' @param textString Required. A character string containing strings separated by
#' any number of spaces and/or tabs.
#'
#' @return Returns a vector of individual strings.
#' @author Kevin Shook
#' @export
#' @importFrom stringr str_trim str_replace_all str_detect str_split
#' @keywords internal
#'
#' @examples
#' vals <- parseText(' red  green    blue      black')
#'
parseText <- function(textString){
  # remove padding
  textString <- str_trim(textString)

  # swap tabs for characters
  textString <- str_replace_all(textString, stringr::fixed('\t'), ' ' )
  double.spaces <- str_detect(textString, '  ')

  # replace all double spaces with single spaces

  while (double.spaces) {
    textString <- str_replace_all(textString, '  ', ' ' )
    double.spaces <- str_detect(textString, '  ')
  }

  texts <- unlist(str_split(textString, ' '))
  return(texts)
}
