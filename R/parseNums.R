#' Parses a string containing numbers
#'
#' @description This is an internal \pkg{CRHMr} function and should normally not
#' need to be called directly. It is used for reading data from messy files such
#' as CRHM projects.
#' @param numString Required. A character string containing numbers separated by
#' any number of spaces.
#'
#' @return Returns a numeric vector.
#' @author Kevin Shook
#' @export
#' @importFrom stringr str_trim str_replace_all fixed str_detect str_split
#' @keywords internal
#'
#' @examples
#' nums <- parseNums(" 1  2 3   4     5 ")
#'
parseNums <- function(numString){
  # remove padding
  numString <- str_trim(numString)

  # swap tabs for characters
  numString <- str_replace_all(numString, fixed('\t'), ' ' )
  double_spaces <- str_detect(numString, '  ')
  # replace all double spaces with single spaces

  while (double_spaces) {
    numString <- str_replace_all(numString, '  ', ' ' )
    double_spaces <- str_detect(numString, '  ')
  }

  nums <- as.numeric(unlist(str_split(numString, ' ')))
  return(nums)
}
