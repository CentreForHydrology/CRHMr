#' Runs a CRHM model
#'
#' @description Runs CRHM with a specified \code{.prj} file and (optionally) \code{.obs} file(s) and \code{.par} file(s). Under OSX and Linux, CRHM requires either 1) the program \code{Wine} to run \code{CRHM.exe} or 2) a native-compiled version. This function will use \code{Wine} if it is required. Note that none of the file names or their paths can contain spaces.
#' @param CRHMfile Required. CRHM executable file (usually \code{CRHM.exe}.
#' @param prjFile Required. Name of \code{.prj} file. If the file does not contain any displayed observations or variables, or if any of the settings \code{Auto_Run}, \code{Auto_Exit} or \code{Log_All} are missing, then the function will terminate with an error message.
#' @param obsFiles Optional. Name(s) of obs file(s).
#' @param parFiles Optional. Name(s) of parameter file(s).
#' @param outFile Optional. Name(s) of output file(s). If not specified, the original CRHM output file name (e.g. \option{CRHM_output_1.txt}) is kept.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @details If \code{Wine} is required, the function will copy the specified .obs, and .par files to the directory of the .prj file, before execution of CRHM. After the program has finished, the copies will be deleted. Running a CRHM model requires that the .prj file has been setup to run autmatically, as shown in the function automatePrj.
#' @return If successful, returns \code{TRUE}. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @seealso  \code{\link{automatePrj}}
#' @examples
#' \dontrun{
#' result <- runCRHM('c:/CRHM/CRHM.exe', 'c:/BadLake/BadLake1975.prj',
#' outfile='c:/BadLake/BadLake1975Output.txt')}
#' @export
runCRHM <- function(CRHMfile='', prjFile='', obsFiles='',  parFiles='',
                    outFile='', logfile='') {
  currentDir <- getwd()
  eol_val <- win.eol()

  # check for spaces in file names
  if(stringr::str_detect(CRHMfile ,' ')){
    cat('Error: space present in CRHM executable file ath\n')
    return(FALSE)
  }

  if(stringr::str_detect(prjFile ,' ')){
    cat('Error: space present in .prj file path\n')
    return(FALSE)
  }

  # figure out OS type and if Wine is being used
  CRHM_file_name <- basename(CRHMfile)
  exe_present <- stringr::str_detect(CRHM_file_name, stringr::fixed('.exe', TRUE))


  if (eol_val == '\n')
    OS_type <- 'Windows'
  else
    OS_type <- 'unix'

  if ((OS_type == 'unix') & exe_present)
    # use wine
    exe_prefix <- 'wine '
  else
    # don't use wine - must be a native version present
    exe_prefix <- ''

  # check parameters
  if (CRHMfile==''){
    cat('Error: missing CRHM file name\n')
    return(FALSE)
  }


  # check for executable file in specified path
  CRHM_present <- file.exists(CRHMfile)
  if (!CRHM_present){
    cat("Error: can't find CRHM executable file\n")
    return(FALSE)
  }

  # check for project file
  prj_present <- file.exists(prjFile)


  if (!prj_present){
    cat("Error: can't find CRHM project file\n")
    return(FALSE)
  }


  # check contents of CRHM file to be sure that it's worth running
  con <- file(prjFile, "r", blocking = FALSE, encoding="ISO_8859-2")
  prj <- readLines(con)
  close(con)

  display_variable <- readPrjLineText(prj, 'Display_Variable')
  display_observation <- readPrjLineText(prj, 'Display_Observation')

  missing_display_variable <- sum(stringr::str_detect(display_variable, '#'))
  missing_display_observation <- sum(stringr::str_detect(display_observation, '#'))

  if ((missing_display_variable > 0) & (missing_display_observation > 0)){
    cat("Error: no variables output in .prj file\n")
    return(FALSE)
  }

  # check for Auto_Run, Log_All and Auto_Exit, if any are absent then quit
  auto_run_present <- sum(stringr::str_detect(prj, 'Auto_Run'))
  auto_exit_present <- sum(stringr::str_detect(prj, 'Auto_Exit'))
  log_all_present <- sum(stringr::str_detect(prj, 'Log_All'))

  if(auto_run_present <= 0){
    cat("Error: missing Auto_Run command in .prj file\n")
    return(FALSE)
  }
  if(auto_exit_present <= 0){
    cat("Error: missing Auto_Exit command in .prj file\n")
    return(FALSE)
  }
  if(log_all_present <= 0){
    cat("Error: missing Log_All command in .prj file\n")
    return(FALSE)
  }


  # get output file created by CRHM
  # find RUN_ID by reading prj file

  RUN_ID <- readPrjLine(prj, 'RUN_ID')
  CRHM_output_file <- paste('CRHM_output_', RUN_ID, '.txt', sep='')

  if (exe_prefix == 'wine '){
    prjFileBasename <- basename(prjFile)
    CRHM_execution_string <- paste(exe_prefix,' ', CRHMfile, ' ', prjFileBasename, sep='')
    # copy obs and par files to location of prj file
    prj_dir <- dirname(prjFile)
    if (obsFiles != ''){
      numfiles <- length(obsFiles)
      obsFilesBasenames <- basename(obsFiles)
      for (i in 1:numfiles){
        # check to see if obs file is already in prj directory
        obs_dir <- dirname(obsFiles[i])
        if (obs_dir != prj_dir){
          copy_command <- paste('cp ', obsFiles[i],' ', prj_dir, sep='')
          system(copy_command)
        }
      }
      obs <- stringr::str_c(obsFilesBasenames, collapse=' ')
      if(stringr::str_detect(obs ,' ')){
        cat('Error: space present in .obs file path\n')
        return(FALSE)
      }
      CRHM_execution_string <- paste(CRHM_execution_string, obs, sep=' ')
    }

    if (parFiles != ''){
      numfiles <- length(parFiles)
      parFilesBasenames <- basename(parFiles)
      for (i in 1:numfiles){
        obs_dir <- dirname(parFiles[i])
        if (par_dir != prj_dir){
          copy_command <- paste('cp ', parFiles[i],'', prj_dir, sep = '')
          system(copy_command)
        }
      }
      par <- stringr::str_c(parFilesBasenames, collapse=' ')
      if(stringr::str_detect(par ,' ')){
        cat('Error: space present in parameter file path\n')
        return(FALSE)
      }
      CRHM_execution_string <- paste(CRHM_execution_string, par, sep = ' ')
    }

    # set working directory to be .prj directory
    if (currentDir != prj_dir)
      setwd(prj_dir)

    # now run CRHM
    system(CRHM_execution_string)

    # delete copied files
    if (obsFiles != ''){
      numfiles <- length(obsFiles)
      for (i in 1:numfiles)
        file.remove(obsFiles[i])
    }

    if (parFiles != ''){
      numfiles <- length(parFiles)
      for (i in 1:numfiles)
        file.remove(parFiles[i])
    }
  }
  else{
    # running under Windows
    CRHM_execution_string <- CRHMfile
    CRHM_parameter_string <- prjFile

    if (length(obsFiles) > 1){
      if(any(stringr::str_detect(obs ,' '))){
        cat('Error: space present in .obs file path\n')
        return(FALSE)
      }
      obs <- stringr::str_c(obsFiles, collapse=' ')
      CRHM_parameter_string <- paste(CRHM_parameter_string, ' ', obs, sep='')

    } else {
      if(obsFiles != ''){
        if(stringr::str_detect(obsFiles ,' ')){
          cat('Error: space present in .obs file path\n')
          return(FALSE)
        }
      CRHM_parameter_string <- paste(CRHM_parameter_string, ' ', obsFiles, sep='')
      }
    }

    if (length(parFiles) > 1){
      if(any(stringr::str_detect(par,' '))){
        cat('Error: space present in parameter file path\n')
        return(FALSE)
      }
      par <- stringr::str_c(parFiles, collapse=' ')
      CRHM_parameter_string <- paste(CRHM_parameter_string, ' ', par, sep='')

    } else {
      if (parFiles != ''){
        if(stringr::str_detect(par ,' ')){
          cat('Error: space present in parameter file path\n')
          return(FALSE)
        }
        CRHM_parameter_string <- paste(CRHM_parameter_string, ' ', parFiles, sep='')
      }
    }
    # now call CRHM
    system2(CRHM_execution_string, args=CRHM_parameter_string)

    # create fake execution string for logging
    CRHM_execution_string <- paste(CRHM_execution_string, ' ', CRHM_parameter_string, sep='')
  }

  # move/rename output
  file.rename(CRHM_output_file, outFile)

  # reset working directory
  setwd(currentDir)

  # log action
  comment <- paste('runCRHM command:', CRHM_execution_string, sep='')
  result <- logAction(comment, logfile)
  return(result)
}
