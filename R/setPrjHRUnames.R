#' Sets the names of HRUs in a CRHM model \code{.prj} file
#' @description Replaces the existing names of all HRUs, in a \code{.prj} file.
#' @param inputPrjFile Required. Name of the \code{.prj} file.
#' @param HRUnames Required. Vector containing new HRU names. There must be at least as many values in this vector as HRU names in the orginal \code{.prj} file.
#' @param outputPrjFile Optional. If omitted, the input \code{.prj} file will be overwritten.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If successful, returns \code{TRUE}. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @note Currently, this function has only been tested on simple \code{.prj} files, i.e. without REW Grp parameters.
#' @seealso  \code{\link{runCRHM}} \code{\link{setPrjParameters}} \code{\link{setPrjDates}}
#' @export
#'
#' @examples
#' \dontrun{
#' result <- setPrjHRUnames('c:/CRHM/Bad74_Frozen.prj', c('fallow', 'stubble', 'grass'))}
setPrjHRUnames <- function(inputPrjFile='', HRUnames='', outputPrjFile='', logfile='') {
  eol_val <- win.eol()
  
  # check parameters
  if (inputPrjFile == ''){
    cat('Missing CRHM input .prj file name\n')
    return(FALSE)  
  }

  if (length(HRUnames) <=  0){
    cat('Missing HRU names\n')
    return(FALSE)  
  }
  
  # read in .prj file
  prj <- readPrj(inputPrjFile)

  # find number of HRUs in basin
  hru_line <- grep('nhru', prj, fixed=TRUE)
  hru_count <- parseText(prj[hru_line])[2]
  hru_count <- as.numeric(hru_count)
  
  # find HRU names
  start_line <- grep('hru_names', prj, fixed=TRUE)
  if(length(start_line) == 0){
    cat('Could not find the existing HRU names\n')
    return(FALSE)
  }
  
  
  line_num <- start_line + 1
  done <- FALSE
  total_vals <- 0
  while (!done){
    # check to see if numeric or not
    current <- prj[line_num]
    current_vals <- parseText(current)
    num_vals <- length(current_vals)
    total_vals <- total_vals + num_vals
    
    if (total_vals > hru_count){
      cat('More hru names in file than specified\n')
      return(FALSE)
    }
      
    # assemble replacement values
    replace_start <- (total_vals - num_vals) + 1
    replace_vals <- paste("'", 
                          stringr::str_c(HRUnames[replace_start:total_vals],  
                                         collapse="' '"), "'", sep='')
      
    prj[line_num] <- replace_vals 
      
    if (total_vals == hru_count)
        done <- TRUE       

    if(line_num > start_line + hru_count){
      done <- TRUE
      cat('Could not find the all the hru names\n')
      return(FALSE)
    }
    line_num <-  line_num + 1
  }
  
  # write to file
  if (outputPrjFile == '')
    outputPrjFile <- inputPrjFile
  
  writePrj(prj, outputPrjFile)

  # log action
  allHRUnames <- stringr::str_c(HRUnames,  collapse=",")
  comment <- paste('setPrjHRUnames input file: ', inputPrjFile,
                   ' output file:', outputPrjFile,
                   ' HRUnames:', allHRUnames)
  
  result <- logAction(comment, logfile)
  return(result)
}