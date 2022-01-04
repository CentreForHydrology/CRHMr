readPrjREWGrps <- function(prjFile = "", logfile = "") {
  # check parameter values
  if (prjFile == '' | is.null(prjFile)) {
    cat('Missing CRHM input .prj file name\n')
    return(FALSE)
  }

  # read in .prj file
  prj <- readPrj(prjFile)

  # find number of groups
  # get rid of lines before "Macros:"
  param_start_line <- grep("Macros:", prj, fixed = TRUE) + 1
  prj <- prj[-(1:param_start_line)]

  # get rid of lines after parameters
  param_end_line <- min(grep("#", prj, fixed = TRUE)) - 1
  prj <- prj[1:param_end_line]

  # now look for REW_Grp
  GRP_line <- grep('REW_Grp', prj, fixed = TRUE) + 1
  num_GRP <- parseNums(prj[GRP_line])

  # read in all REW Grp info



}
