#' Writes information about changes to a \pkg{CRHMr} dataframe to a file
#'
#' @description This is an internal \pkg{CRHMr} function and should normally not need to be called directly. This function writes a file containing the a summary of the files as well as the changed data values and a description of their source.
#' @param action Required. A character string indicating the action taken - ususally this is the name of the function making the changes.
#' @param original.data.info Required. Information about the original dataframe from \code{CRHM_summary}.
#' @param changed.data.info Required. Information about the changed dataframe from \code{CRHM_summary}.
#' @param changed.data.type Required. A vector indicating the type/source of data in the changed dataframe.
#' @param comment1 Optional. A comment about the data and/or changes.
#' @param comment2 Optional. A comment about the data and/or changes.
#' @param comment3 Optional. A comment about the data and/or changes.
#' @param comment4 Optional. A comment about the data and/or changes.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#'
#' @return If successful, returns \code{TRUE}. If unsucessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @export
#'
#' @examples \dontrun{
#' writeChangeLogFile('appendObs', obs1.info, obs2.info, output.type,
#' 'primary data: obs1', 'secondary data: obs2')}
writeChangeLogFile <-
function(action, original.data.info, changed.data.info, 
                               changed.data.type, comment1='', comment2='', 
                               comment3='', comment4='', quiet){
  eol.val <- win.eol()
  
  # creates log file for a changed dataframe
  # writes up to 4 lines of comments to a log file
  date.time <- date()
  date.time.compressed <- format(Sys.time(), format='%Y%m%d%H%M')

  if (nrow(original.data.info) > 0){
    if (!quiet){
      cat('Original data\n')
      print(original.data.info)
    }
  }
  else{
    cat('Missing data set information\n')
    return(FALSE)
  }
  
  if (nrow(changed.data.info) > 0){
    if (!quiet){
      cat('Changed data\n')    
      print(changed.data.info) 
    }
  }
  else{
    cat('Missing data set information\n')
    return(FALSE)
  }
 
  LogFileName  <- paste(action,'_',date.time.compressed, '.log',sep='')  
  cat('Created: ', date.time, eol.val, sep='', file=LogFileName, append=FALSE)
  if (comment1 !=''){
    cat(comment1, eol.val, sep='', file=LogFileName, append=TRUE)    
  }
  if (comment2 !=''){
    cat(comment2, eol.val, sep='', file=LogFileName, append=TRUE)    
  }
  if (comment3 !=''){
    cat(comment3, eol.val, sep='', file=LogFileName, append=TRUE)    
  }
  if (comment4 !=''){
    cat(comment4, eol.val, sep='', file=LogFileName, append=TRUE)    
  }
  cat('Original data', eol.val, sep='', file=LogFileName, append=TRUE)
  write.table(original.data.info, sep='\t', file=LogFileName, append=TRUE, eol=eol.val, 
              col.names=FALSE, row.names=FALSE, quote=FALSE)
  
  cat('New data', eol.val, sep='', file=LogFileName, append=TRUE)
  write.table(changed.data.info, sep='\t', file=LogFileName, append=TRUE, eol=eol.val, 
              col.names=FALSE, row.names=FALSE, quote=FALSE) 
  
  cat('New data types', eol.val, sep='', file=LogFileName, append=TRUE)
  
  # write col names out manually, to prevent a warning
  col.names <- names(changed.data.type)
  cat(col.names, eol.val, sep='\t', file=LogFileName, append=TRUE)
  write.table(changed.data.type, sep='\t', file=LogFileName, append=TRUE, eol=eol.val, 
              col.names=FALSE, row.names=FALSE, quote=FALSE) 
  
  return(TRUE)

  
}
