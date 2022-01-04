#' Logs information about the user
#'
#' @description Writes information about the user and the user's computer to the logfile.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return Returns nothing.
#' @author Kevin Shook
#' @note This function records information about your computer (the user, the operating system, the version of \R, and all of the packages in use). It will make it easier to debug your problems and may help you to figure out why \R code that used to work is no longer giving the correct answer.
#' @examples
#' user()
#' @export

user <-
function(logfile=''){
  # this function adds information about the user and the user's system to the log file 
  s <- Sys.info()  
  a <- data.frame(names(s),s)
  
  # get packages
  session <- sessionInfo()
  basePackages <- paste(session$basePkgs, sep=" ", collapse=" ")
  otherPackages <- session$otherPkgs
  
  # extract package info
  for (i in 1:length(otherPackages)){
    pkg <- otherPackages[[i]]$Package
    ver <- otherPackages[[i]]$Version
    packagever <- paste(pkg, ":", ver, sep='')
    if (i==1)
      otherPack <- packagever
    else
      otherPack <- paste(otherPack, ',', packagever, sep='')
  }
  
  info <- paste('User:',a[8,2],' OS:',a[1,2],' R_version:',a[2,2],
                ' Base packages:', basePackages, ' Other packages:', otherPack,
                sep='')
  
  result <- logAction(info, logfile) 
}
