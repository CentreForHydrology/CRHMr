
deletePrjHRUs <- function(inputPrjFile = "", deletedHRUs = NULL, outputPrjFile,
                          quiet = TRUE, logfile = "") {
  # check parameters
  if (inputPrjFile == "")
    stop("Missing CRHM input .prj file name")

  if (is.null(deletedHRUs))
    stop("No HRUs specified")

  # get HRU names
  HRUnames <- readPrjHRUnames(prjFile)

  if ((length(deletedHRUs) > length(HRUnames)) |
     (max(deletedHRUs) > length(HRUnames)))
    stop("Invalid HRU number")

  num_deleted_HRUs <- length(deletedHRUs)

  # remove names of selected HRUs
  newHRUnames <- HRUnames[-deletedHRUs]

  # get number of parameters to change

  prj <- readPrj(inputPrjFile)
  parameter_lines <- grep("<", prj, fixed = TRUE)
  num_parameters <- length(parameter_lines)

  if (outputPrjFile == "")
    outputPrjFile <- inputPrjFile

  setPrjHRUnames(inputPrjFile = inputPrjFile, HRUnames = newHRUnames,
                 outputPrjFile = outputPrjFile)

  omit_parameters <- c("distrib_Route", "HRU_OBS",
                       "obs_elev", "soil_withdrawal")
  param_names <- prj[parameter_lines]
  for (i in num_parameters) {
    param_name <- param_names[i]
    if (!param_names %in% omit_parameters) {
       params <- readPrjParameters(inputPrjFile, paramName = param_name)
       new_params <- params[-deletedHRUs]
       setPrjParameters(inputPrjFile = inputPrjFile,
                        paramName = param_name,
                        paramVals = new_params,
                        outputPrjFile = outputPrjFile)
    }
  }

  # do unusual parameters





}
