#' Automates a CRHM .prj file
#'
#' @description Converts a CRHM .prj file to be able to be run automatically. It adds the settings \code{Auto_Run}, \code{Auto_Exit} and \code{Log_All} to the end of the .prj file, if they are required.
#' @param inputPrjFile Required. Name of the the CRHM .prj to be converted.
#' @param outputPrjFile Optional. If omitted, the input .prj file will be overwritten.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If successful, returns \code{TRUE}. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @seealso  \code{\link{runCRHM}}
#' @importFrom stringr str_detect
#' @export
#'
#' @examples
#' \dontrun{
#' result <- automatePrj('Bad Lake 1974-1975.prj','Bad Lake 1974-1975 auto.prj')}
automatePrj <- function(inputPrjFile="", outputPrjFile="", quiet=TRUE,
                        logfile="") {
  # adds parameters to CRHM .prj file to allow it to be run automatically
  # check parameters
  eol_val <- win.eol()

  # check parameters
  if (inputPrjFile == "") {
    stop("Missing CRHM input .prj file name")
  }

  # read in .prj file
  con <- file(inputPrjFile, "r", blocking = FALSE, encoding = "ISO_8859-2")
  prj <- readLines(con)
  close(con)

  # check for Auto_Run, Log_All and Auto_Exit, if any are absent then quit
  auto_run_present <- sum(str_detect(prj, "Auto_Run"))
  auto_exit_present <- sum(str_detect(prj, "Auto_Exit"))
  log_all_present <- sum(str_detect(prj, "Log_All"))

  if (auto_run_present > 0) {
    if (!quiet) {
      cat("Auto_Run command already present in .prj file\n")
    }
  }
  else {
    # insert Auto_Run
    last_line <- prj[length(prj)]
    if (sum(str_detect(last_line, "#")) >= 1) {
      prj <- c(prj, c("Auto_Run", "#####"))
    } else {
      prj <- c(prj, c("#####", "Auto_Run", "#####"))
    }
  }
  if (auto_exit_present > 0) {
    if (!quiet) {
      cat("Auto_Exit command already present in .prj file\n")
    }
  }
  else {
    # insert Auto_Exit
    last_line <- prj[length(prj)]
    if (sum(str_detect(last_line, "#")) >= 1) {
      prj <- c(prj, c("Auto_Exit", "#####"))
    } else {
      prj <- c(prj, c("#####", "Auto_Exit", "#####"))
    }
  }
  if (log_all_present > 0) {
    if (!quiet) {
      cat("Log_all command already present in .prj file\n")
    }
  }
  else {
    # insert Log_All
    last_line <- prj[length(prj)]
    if (sum(str_detect(last_line, "#")) >= 1) {
      prj <- c(prj, c("Log_All", "#####"))
    } else {
      prj <- c(prj, c("#####", "Log_all", "#####"))
    }
  }

  # write to file
  if (outputPrjFile == "") {
    outputPrjFile <- inputPrjFile
  }

  con <- file(outputPrjFile, "w", blocking = FALSE)
  writeLines(prj, con, sep = eol_val)
  close(con)

  # log action
  comment <- paste("automatePrj input file: ", inputPrjFile, "output file: outputPrjFile", sep = "")
  result <- logAction(comment, logfile)
  return(result)
}
