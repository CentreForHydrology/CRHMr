#' Summarises all obs files in a directory
#'
#' @description This function summarises all of the obs files in specified directory, optionally plotting the values in each obs file in a separate graph. 
#' @param file.dir Optional Directory containing .obs files. If not specified, defaults to current directory. Note that this is an \R path, which uses the \code{'/'} symbol on ALL operating systems.
#' @param timezone Required. The name of the timezone of the data as a character string. This should be the timezone of your data, but omitting daylight savings time. Note that the timezone code is specific to your OS. To avoid problems, you should use a timezone without daylight savings time. Under Linux, you can use \option{CST} and \option{MST} for Central Standard or Mountain Standard time, respectively. Under Windows or OSX, you can use \option{etc/GMT+6} or \option{etc/GMT+7} for Central Standard and Mountain Standard time. DO NOT use \option{America/Regina} as the time zone, as it includes historical changes between standard and daylight savings time.
#' @param summaryFile Optional. File to contain summary. Defaults to \option{obsSummary.csv} in the same directory as the obs files.
#' @param plot Optional. Logical. Do you want to plot graphs of each obs file? Defaults to \code{FALSE}.
#' @return If successful returns no value. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @seealso  \code{\link{plotObs}}
#' @examples
#' \dontrun{
#' summariseObsFiles('c:/data/BadLake/MSC', timezone='etc/GMT+6', plot=TRUE)}
#' @export

summariseObsFiles <- function(file.dir = '', timezone='', summaryFile='obsSummary.csv', plot=FALSE){
  # summarizes a whole directory of .obs files
  # check parameters
  if (timezone == ''){
    cat('Error: must specify a timezone\n')
    return(FALSE)
  }
  
  current.dir <- getwd()
  if (file.dir != '')
    setwd(file.dir)
  
  filespec <- '*.obs'
  FilePattern <- glob2rx(filespec)
  FileList <- list.files(pattern=FilePattern)
  NumFiles <- length(FileList)
  
  if (NumFiles < 1){
    cat('Error: no obs files in specified directory\n')
    return(FALSE)
  }
  
  cat(file=summaryFile,'Obs file, Time step (hours), Total rows, Complete rows, From, To, First complete date, Last complete date, Number of variables, Variable names\n')
  
  for (i in 1:NumFiles){ 
    obsfile <- FileList[i]
    obs <- readObsFile(obsfile, 'obs', timezone=timezone, quiet=TRUE)   
    # get info about file
    obs.summary <- CRHM_summary(obs)
    obs.summary <- as.character(obs.summary[,2])
    obs.summary.values <- stringr::str_c(obs.summary, collapse=',')
    cat(file=summaryFile, obsfile,',', obs.summary.values, '\n', append=TRUE, sep='')
    
    if (plot){
      p <- plotObs(obs, plotType='points')
      a  <-  stringr::str_split(obsfile, stringr::fixed('.obs', ignore_case=TRUE))
      mainfilename  <-  a[[1]][1]
      plotfile <- paste(mainfilename, '.png', sep='')
      ggplot2::ggsave(p, filename=plotfile, width=8, height=6 )
    }
  }
  
  # return to current directory
  setwd(current.dir)
}