#' Reads text values from a CRHM project text object
#' @description Reads text values from a project file variable. The number of
#' values to be read in does not need to be specified. This is an internal function
#' and is not able to be called by a function outside of \pkg{CRHMr}, as it is
#' not exported. This documentation is for maintenance purposes.
#'
#' @param prj Required. A CRHM file read into memory as a sequence of lines, as
#' returned from \code{readPrj}.
#' @param searchString Required. The identifying string.
#' @param valueCount Required. The number of values to be read in.
#'
#' @return If successful, returns a vector with the numeric values.
#' If unsuccessful, returns the value \code{FALSE}.
#' @keywords internal
#' @examples \dontrun{
#' hru_names <- readPrjTextVals(Bologna,'hru_names',19)}
#'
readPrjTextVals <- function(prj, searchString, valueCount){
  # searches for a specified string and reads in the following lines and parses it
  prj_lines <- length(prj)
  line_num <- grep(searchString, prj, fixed = TRUE) + 1

  # search read until all values read in
  all_vals <- c()
  total_val_count <- 0
  done <- FALSE

  while (!done) {
    current <- prj[line_num]
    current_vals <- parseText(current)
    current_val_count <- length(current_vals)
    all_vals <- c(all_vals, current_vals)
    total_val_count <- total_val_count + current_val_count

    if (total_val_count == valueCount)
      done <- TRUE
    else
      line_num <- line_num + 1

    if ((total_val_count > valueCount) | (line_num > prj_lines)) {
      cat("Error: couldn't find values\n")
      return(FALSE)
    }
  }

  # remove single quotes
  all_vals <- stringr::str_replace_all(all_vals,"'",'')
  return(all_vals)
}
