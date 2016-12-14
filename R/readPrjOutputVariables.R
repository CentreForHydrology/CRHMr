#' Reads output variables from a CRHM model \code{.prj} file
#'
#' @param prjFile Required. Name of CRHM .prj file to read.
#' @param asDataframe Optional. If \code{FALSE} (the default), returns the set of output variables as a vector. If \code{TRUE}, returns a data frame containing the \code{module}, \code{variable} and \code{HRUs} as a string. The vector formis useful if you want to delete some variables. The data frame is more useful if you want to change the variable or its HRUs.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If successful, returns the output variables. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @seealso  \code{\link{setPrjOutputVariables}}
#' @export
#'
#' @examples \dontrun{
#' # read in as a vector
#' variables <- readPrjOutputVariables('c:/CRHM/Bad Lake 1974-1975.prj')
#'
#' # read in as a data frame
#' variables <- readPrjOutputVariables('c:/CRHM/Bad Lake 1974-1975.prj', asDataframe=TRUE)
#' # change value
#' variables$HRUs[3] <- '1 2'}
readPrjOutputVariables <- function(prjFile, asDataframe=FALSE, logfile=''){

  # check variables
  if (prjFile == ''){
    cat('Missing CRHM input .prj file name\n')
    return(FALSE)
  }

  prj <- readPrj(prjFile)

  variables <- c(0)
  # find location of variables and read them in
  searchString <- 'Display_Variable:'
  linenum <- grep(searchString, prj, fixed=TRUE)

  # skip 2 lines
  linenum <- linenum + 2
  done <- FALSE
  varNum <- 0

  while(!done){
    current <- prj[linenum]
    # check for #
    if (stringr::str_detect(current, stringr::fixed('#'))){
      done <- TRUE
    }else{
      varNum <- varNum + 1
      variables[varNum] <- current
      linenum <- linenum + 1
    }
  }

  if(!asDataframe){
    all <- variables
  } else{
    # parse output
    varCount <- length(variables)
    module <- c(0)
    variable <- c(0)
    HRUs <- c(0)

    for(i in 1:varCount){
      strings <- parseText(variables[i])
      module[i] <- as.character(strings[1])
      variable[i] <- as.character(strings[2])
      HRUs[i] <- as.character(paste(strings[-(1:2)], collapse = ' '))
      }
    all <- data.frame(module, variable, HRUs, stringsAsFactors = FALSE)
    return(all)
  }

  # log action
  comment <- paste('readPrjOutputVariables prjFile: ', prjFile, sep='')
  result <- logAction(comment, logfile)
  if(!result){
    return(all)
  }
  else{
    return(result)
  }

}
