#' Runs a CRHM model
#'
#' @description Runs CRHM with a specified \code{.prj} file and (optionally) \code{.obs} file(s)
#' and \code{.par} file(s). Under OSX and Linux, CRHM requires either 1) the program \code{Wine} to
#' run \code{CRHM.exe} or 2) a native-compiled version. This function will use \code{Wine} \emph{if} it is
#' indicated (the executable file has the extension \code{.exe}), and the parameter \code{useWine} =
#' \code{TRUE}.
#' @param CRHMfile Required. CRHM executable file (i.e. \code{CRHM.exe} for Windows). If the path
#' to the executable file is not specified, it is assumed that the executable file is on the system path.
#' @param prjFile Required. Name of \code{.prj} file. If the file does not contain any displayed
#' observations or variables, or if any of the settings \code{Auto_Run}, \code{Auto_Exit} or \code{Log_All}
#' are missing, then the function will terminate with an error message.
#' @param obsFiles Optional. Name(s) of obs file(s).
#' @param parFiles Optional. Name(s) of parameter file(s).
#' @param outFile Optional. Name(s) of output file(s). If not specified, the original CRHM output
#' file name (e.g. \option{CRHM_output_1.txt}) is kept.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @param useWine Optional. If \code{FALSE} (the default) and the executable file has the extension
#' \code{.exe} then \code{Wine} will be used to run the model under OSX and Linux. Otherwide, a native
#' version of CRHM will be assumed.
#' @details Running a CRHM model requires that the .prj file has been setup to run autmatically,
#' as shown in the function \code{automatePrj}.
#'
#' If \code{Wine} is required, the function will copy the specified .obs, and .par files to
#' the directory of the .prj file, before execution of CRHM. After the program has finished, the copies
#' will be deleted.
#'
#' If the CRHM executable file is not located on the system path, and is not found in the specified
#' directory, then an error message will be returned.
#'
#' @return If successful, returns \code{TRUE}. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @seealso  \code{\link{automatePrj}}
#' @export
#' @importFrom stringr str_detect str_c str_to_upper
#' @examples \dontrun{
#' # Automate the .prj before use
#' automatePrj("c:/BadLake/BadLake1975.prj")
#'
#' # Using specified paths - in this case the .obs files reference in
#' # the .prj must contain the file path.
#' # It's a good idea to use absolute paths for the .prj and .obs files.
#' # If you use relative paths, they have to be w.r.t. the CRHM executable file.
#'
#' setPrjObs("c:/BadLake/BadLake1975.prj", obsFiles = "c:/BadLake/Badlake73_76.obs")
#' result <- runCRHM("c:/CRHM/CRHM.exe", "c:/BadLake/BadLake1975.prj",
#' outfile = "c:/BadLake/BadLake1975Output.txt")
#'
#' # Omitting paths - all files are stored in the current directory.
#'
#' # Remove paths from the reference to the .obs files in the .prj file
#' setPrjObs("c:/BadLake/BadLake1975.prj", obsFiles = "Badlake73_76.obs")
#'
#' # Set the working directory to the one holding the model files.
#' setwd("c:/BadLake")
#'
#' # Execute the model.
#' result <- runCRHM("c:/CRHM/CRHM.exe", "BadLake1975.prj",
#' outfile = "BadLake1975Output.txt")
#'
#' # If CRHM is on the system path you can run it without specifying the location.
#' # This command will show the current path:
#' Sys.getenv("PATH")
#'
#' # Execute the model without specifying the path to CRHM.
#' result <- runCRHM("CRHM.exe", "BadLake1975.prj",
#' outfile = "BadLake1975Output.txt")}

runCRHM <- function(CRHMfile='', prjFile='', obsFiles='',  parFiles='',
                    outFile='', logfile='', useWine = FALSE) {
  currentDir <- getwd()
  eol_val <- win.eol()

  # figure out OS type and if Wine is being used
  CRHM_file_name <- basename(CRHMfile)
  exe_present <- str_detect(CRHM_file_name, fixed('.exe', TRUE))


  if (eol_val == '\n')
    OS_type <- 'Windows'
  else
    OS_type <- 'unix'

  if ((OS_type == 'unix') & exe_present & useWine)
    # use wine
    exe_prefix <- 'wine '
  else
    # don't use wine - must be a native version present
    exe_prefix <- ''

  # check parameters
  if (CRHMfile == '') {
    cat('Error: missing CRHM file name\n')
    return(FALSE)
  }

  # check for executable file in path or specified location

  CRHM_path <- Sys.which(CRHMfile)
  CRHM_present <- file.exists(CRHMfile)
  if (CRHM_path == "" & !CRHM_present) {
    cat("Error: can't find CRHM executable file\n")
    return(FALSE)
  }

  # check if Borland or CRHMcode GUI
  CRHMcode <- FALSE
  if (str_detect(str_to_upper(CRHMfile), "CRHM_GUI"))
     CRHMcode <- TRUE


  # check for project file
  prj_present <- file.exists(prjFile)

  if (!prj_present) {
    cat("Error: can't find CRHM project file\n")
    return(FALSE)
  }
  # replace spaces with escape sequences
  CRHMfile_no_spaces <- gsub(" ", "\\\ ", CRHMfile, fixed = TRUE)
  prjFile_no_spaces <- gsub(" ", "\\\ ", prjFile, fixed = TRUE)

  # check contents of CRHM file to be sure that it's worth running
  con <- file(prjFile, "r", blocking = FALSE, encoding = "ISO_8859-2")
  prj <- readLines(con)
  close(con)

  display_variable <- readPrjLineText(prj, 'Display_Variable')
  display_observation <- readPrjLineText(prj, 'Display_Observation')

  missing_display_variable <- sum(str_detect(display_variable, '#'))
  missing_display_observation <- sum(str_detect(display_observation, '#'))

  if ((missing_display_variable > 0) & (missing_display_observation > 0)) {
    cat("Error: no variables output in .prj file\n")
    return(FALSE)
  }

  # check for Auto_Run, Log_All and Auto_Exit, if any are absent then quit
  auto_run_present <- sum(str_detect(prj, 'Auto_Run'))
  auto_exit_present <- sum(str_detect(prj, 'Auto_Exit'))
  log_all_present <- sum(str_detect(prj, 'Log_All'))

  if (auto_run_present <= 0) {
    cat("Error: missing Auto_Run command in .prj file\n")
    return(FALSE)
  }
  if (auto_exit_present <= 0) {
    cat("Error: missing Auto_Exit command in .prj file\n")
    return(FALSE)
  }
  if (log_all_present <= 0) {
    cat("Error: missing Log_All command in .prj file\n")
    return(FALSE)
  }

  # get output file created by CRHM
  # find RUN_ID by reading prj file

  RUN_ID <- readPrjLine(prj, 'RUN_ID')
  if (!CRHMcode)
     CRHM_output_file <- paste('CRHM_output_', RUN_ID, '.txt', sep = '')
  else
     CRHM_output_file <- paste('CRHM_output_', RUN_ID, '.obs', sep = '')

  if (exe_prefix == 'wine ') {
    prjFileBasename <- basename(prjFile_no_spaces)
    CRHM_execution_string <- paste(exe_prefix,' ', CRHMfile_no_spaces, ' ', prjFileBasename, sep = '')
    # copy obs and par files to location of prj file
    prj_dir <- dirname(prjFile)
    if (obsFiles != '') {
      numfiles <- length(obsFiles)
      obsFilesBasenames <- basename(obsFiles)
      for (i in 1:numfiles) {
        # check to see if obs file is already in prj directory
        obs_dir <- dirname(obsFiles[i])
        if (obs_dir != prj_dir) {
          copy_command <- paste('cp ', obsFiles[i],' ', prj_dir, sep = '')
          system(copy_command)
        }
      }
      obs <- str_c(obsFilesBasenames, collapse = ' ')
      obs_no_spaces <- gsub(" ", "\\\ ", obs, fixed = TRUE)
      CRHM_execution_string <- paste(CRHM_execution_string, obs_no_spaces, sep = ' ')
    }


    if (parFiles != '') {
      numfiles <- length(parFiles)
      parFilesBasenames <- basename(parFiles)
      for (i in 1:numfiles) {
        par_dir <- dirname(parFiles[i])
        if (par_dir != prj_dir) {
          copy_command <- paste('cp ', parFiles[i],'', prj_dir, sep = '')
          system(copy_command)
        }
      }
      par <- str_c(parFilesBasenames, collapse = ' ')
      par_no_spaces <- gsub(" ", "\\\ ", par, fixed = TRUE)
      CRHM_execution_string <- paste(CRHM_execution_string, par_no_spaces, sep = ' ')
    }

    # set working directory to be .prj directory
    if (currentDir != prj_dir)
      setwd(prj_dir)

    # now run CRHM
    system(CRHM_execution_string)

    # delete copied files
    if (obsFiles != '') {
      numfiles <- length(obsFiles)
      for (i in 1:numfiles)
        file.remove(obsFiles[i])
    }

    if (parFiles != '') {
      numfiles <- length(parFiles)
      for (i in 1:numfiles)
        file.remove(parFiles[i])
    }
  }
  else{
    # running under native OS
    CRHM_execution_string <- paste(CRHMfile_no_spaces, " ", prjFile_no_spaces, sep = "")

    if (length(obsFiles) > 1) {
      obs <- str_c(obsFiles, collapse = ' ')
      obs_no_spaces <- gsub(" ", "\\\ ", obs, fixed = TRUE)
      CRHM_execution_string <- paste(CRHM_execution_string, obs_no_spaces, sep = ' ')

    } else {
      if (obsFiles != '') {
        obs_no_spaces <- gsub(" ", "\\\ ", obsFiles, fixed = TRUE)
        CRHM_execution_string <- paste(CRHM_execution_string, obs_no_spaces, sep = ' ')
      }
    }

    if (length(parFiles) > 1) {
      if (any(str_detect(parFiles,' '))) {
        cat('Error: space present in parameter file path\n')
        return(FALSE)
      }
      par <- str_c(parFiles, collapse = ' ')
      par_no_spaces <- gsub(" ", "\\\ ", par, fixed = TRUE)
      CRHM_execution_string <- paste(CRHM_execution_string, par_no_spaces, sep = " ")

    } else {
      if (parFiles != '') {
        par_no_spaces <- gsub(" ", "\\\ ", parFiles, fixed = TRUE)
        CRHM_execution_string <- paste(CRHM_execution_string,  par_no_spaces, sep = " ")
      }
    }
    # now call CRHM
    system(CRHM_execution_string)

    prj_dir <- dirname(prjFile)
  }

  # move/rename output
  if (prj_dir != '')
    CRHM_output_file <- paste(prj_dir, '/', CRHM_output_file, sep = '')

  if (outFile != "" & !is.null(outFile))
    file.rename(CRHM_output_file, outFile)

  # reset working directory
  setwd(currentDir)

  # log action
  comment <- paste('runCRHM command:', CRHM_execution_string, sep = '')
  result <- logAction(comment, logfile)
  return(result)
}
