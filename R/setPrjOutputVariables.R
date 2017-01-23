#' Sets the output variables in a CRHM model \code{.prj} file
#'
#' @param inputPrjFile Required. Name of the \code{.prj} file.
#' @param variables Required. Variables to be written to output file. These may be either a character vector combining the variable module, variable name and HRUs or a dataframe with the same information in 3 separate columns. Either format can be returned by \code{setOutputVariables}. If the \option{null} is specified for \code{variables}, then all variables are removed.
#' @param outputPrjFile Optional. If omitted, the input \code{.prj} file will be overwritten.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If successful, returns \code{TRUE}. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @seealso  \code{\link{readPrjOutputVariables}}
#' @export
#'
#' @examples
#' \dontrun{
#' # read in existing variables as a vector
#' variables <- readPrjOutputVariables('Bad_Lake_1974-1975.prj', asDataframe=FALSE)
#'
#' # delete the first set of variables and write to a new file
#' variables <- variables[-1]
#' setPrjOutputVariables('Bad_Lake_1974-1975.prj', variables, 'Bad_Lake_1974-1975_revised.prj')
#'
#' # read in existing variables as a dataframe and write to new file
#' variables <- readPrjOutputVariables('Bad_Lake_1974-1975.prj', asDataframe=TRUE)
#' newVariables <- c('evap', 'evapGrangerD', '1 2 3 4 5')
#' variables <- rbind(variables, newVariables)
#' setPrjOutputVariables('Bad_Lake_1974-1975.prj', variables, 'Bad_Lake_1974-1975_revised2.prj')
#'
#' # deletes all output variables and overwrites the file
#' result <- setPrjOutputVariables('Bad_Lake_1974-1975.prj', 'null')}
setPrjOutputVariables <- function(inputPrjFile='', variables='', outputPrjFile='', logfile=''){

  # check parameters
  if (inputPrjFile == ''){
    cat('Missing CRHM input .prj file name\n')
    return(FALSE)
  }

  if (!is.data.frame(variables) & !is.vector(variables)){
    if (variables == ''){
      cat('Missing variable values\n')
      return(FALSE)
    }
  }

  # read in .prj file
  prj <- readPrj(inputPrjFile)

  # find variables
  # find location of variables and read them in
  searchString <- 'Display_Variable:'
  linenum <- grep(searchString, prj, fixed=TRUE)

  # skip 1 line
  linenum <- linenum + 1

  # copy to new variable
  newPrj <- prj[1:linenum]

  # delete first lines from prj
  prj <- prj[-(1:linenum)]
  prjLines <- length(prj)

  # find end of display variables
  searchString <- '#'
  endlinenum <- grep(searchString, prj, fixed=TRUE)
  if (length(endlinenum) > 1)
    endlinenum <- endlinenum[1]
  rest <- prj[endlinenum:prjLines]

  # construct variables
    if (is.data.frame(variables)){
      variables <- paste(variables[,1], variables[,2], variables[,3], sep=' ')
      allLines <- c(newPrj, variables, rest)
      } else{
        if (length(variables) > 1){
          allLines <- c(newPrj, variables, rest)
        } else{
            if(stringr::str_to_lower(variables) == 'null'){
              allLines <- c(newPrj, rest)
            } else{
                allLines <- c(newPrj, variables, rest)
            }
          }
        }

  # write to file
  if (outputPrjFile == '')
    outputPrjFile <- inputPrjFile

  writePrj(allLines, outputPrjFile)

  # log action
  comment <- paste('setPrjOutputVariables input file: ', inputPrjFile,
                   ' output file:', outputPrjFile, sep='')

  result <- logAction(comment, logfile)
  return(result)
}
