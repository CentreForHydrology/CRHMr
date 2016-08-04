#' Parses a string containing numbers
#'
#' @description This is an internal \pkg{CRHMr} function and should normally not need to be called directly. It is used for reading data from messy files such as CRHM projects.
#' @param numString Required. A character string containing numbers separated by any number of spaces.
#'
#' @return Returns a numeric vector.
#' @author Kevin Shook
#' @export
#'
#' @examples
#' parseNums(' 1  2 3   4     5 ')
parseNums <- function(numString){
  # remove padding
  numString <- stringr::str_trim(numString)
  
  # swap tabs for chracters
  numString <- stringr::str_replace_all(numString, stringr::fixed('/t'), ' ' )
  double.spaces <- stringr::str_detect(numString, '  ')  
  # replace all double spaces with single spaces

  while(double.spaces){
    numString <- stringr::str_replace_all(numString, '  ', ' ' )
    double.spaces <- stringr::str_detect(numString, '  ')    
  }

  nums <- as.numeric(unlist(stringr::str_split(numString, ' ')))
  return(nums)
}