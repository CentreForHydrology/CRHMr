#' Reads variable units
#' @description Gets units for all variables in a specified CRHM output file. If the
#' file is the new CRHM format, then the units are read directly from the second
#' line of the file. If the file is in the older CRHM format, then units are found
#' by matching the variables to those in the internal data frame \code{CRHM_vars}.
#' @param outputFile Required. A CRHM output file.
#' @param quiet Optional. Suppresses display of messages, except for errors. If
#' you are calling this function in an \R script, you will usually leave
#' \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to
#' set \code{quiet = FALSE}.

#' @return If successful, returns a \pkg{CRHMr} data frame containing the name of
#' each variable and its units. If unsuccessful, returns the value \code{FALSE}.
#' @author Kevin Shook
#' @seealso  \code{\link{readOutputFile}}
#' @examples
#' \dontrun{
#' units <- readOutputFile('CRHM_output_1.txt')}
#' @export
readOutputUnits <- function(outputFile, quiet=TRUE){
  # check parameters
  if (outputFile == '') {
    cat('Error: must specify a file name\n')
    return(FALSE)
  }

  # check for '#' symbols in header
  con <- file(outputFile, "r", blocking = FALSE)
  input <- readLines(con, n = 2)
  close(con)

  line1 <- input[1]

  # remove parentheses
  variables <- stringr::str_replace_all(line1, stringr::fixed('('),'.')
  variables <- stringr::str_replace_all(variables, stringr::fixed(')'),'')

  # replace tabs with spaces to allow for parsing
  variables <- stringr::str_replace_all(variables, '#','.calc')
  variables <- stringr::str_split(variables, '\t')
  variables <- unlist(variables)


  # check for units
  line2 <- input[2]
  units_present <- stringr::str_detect(line2, 'units')

  if (units_present) {
    if (!quiet)
      cat('Units found in ', outputFile, '\n', sep = '')
    # parse units from file

    # remove parentheses
    units <- stringr::str_replace_all(line2, stringr::fixed('('),'.')
    units <- stringr::str_replace_all(units, stringr::fixed(')'),'')

    # replace tabs with spaces to allow for parsing
    units <- stringr::str_split(units, '\t')
    units <- data.frame(variables, units)
    names(units)[1] <- variables

  }
  else{
    # units not present, so read from CRHM_vars
    if (!quiet)
      cat('Units NOT found in ', outputFile, '\n', sep = '')

    CRHM_vars <- CRHM_vars

    # remove HRU numbers from variables
    vars <- stringr::str_split_fixed(variables, stringr::fixed('.'), 2)
    variable_names <- unlist(vars[,1])
    variable_names <- variable_names[-1]
    variable_names <- data.frame(variable_names)

    # match with variable names in CRHM_vars
    merged <- merge(variable_names, CRHM_vars, by.x = 'variable_names',
                    by.y = 'name')

    units <- data.frame(variables[-1], merged$units)
    names(units) <- c('variable', 'units')

  }

  return(units)

}
