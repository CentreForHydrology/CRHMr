#' Removes specified HRUs from .prj file
#'
#' @description Deletes names and parameter values for the specified set of HRUs. Note
#' that this function is very destructive, so it's a good idea to have a backup copy
#' of your \code{.prj} file \emph{before} running it!
#' @param inputPrjFile Required. Name of the \code{.prj} file to read.
#' @param deletedHRUs Required. Vector of HRU numbers to delete.
#' @param outputPrjFile Optional. If omitted, the input \code{.prj} file will be overwritten.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling
#' this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default).
#' If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used
#'
#' @return If successful, returns \code{TRUE}. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @note Currently, this function has only been tested on simple \code{.prj} files, i.e.
#' without REW Grp parameters.
#' @seealso  \code{\link{replacePrjParameters}}
#' @export
#' @examples \dontrun{
#' result <- deletePrjParameters("CRHM/Bad74_Frozen.prj", c(3, 5, 7)}
#'
deletePrjHRUs <- function(inputPrjFile = NULL, deletedHRUs = NULL, outputPrjFile = NULL,
                          quiet = TRUE, logfile = "") {
  # check parameters
  if (is.null(inputPrjFile))
    stop("Missing CRHM input .prj file name")

  if (is.null(deletedHRUs))
    stop("No HRUs specified")

  if (is.null(outputPrjFile))
    outputPrjFile <- inputPrjFile

  # get HRU names
  HRUnames <- readPrjHRUnames(inputPrjFile)

  if ((length(deletedHRUs) > length(HRUnames)) |
     (max(deletedHRUs) > length(HRUnames)))
    stop("Invalid HRU number")

  # remove names of selected HRUs
  newHRUnames <- HRUnames[-deletedHRUs]
  prj <- readPrj(inputPrjFile)
  start_line <- grep('hru_names', prj, fixed = TRUE)
  # find end line
  done <- FALSE
  numvals <- 0
 end_line <- start_line
  while (!done) {
    end_line <- end_line + 1
    vals <- parseText(prj[end_line])
    numvals <- numvals + length(vals)
    if (numvals >= length(HRUnames))
      done <- TRUE
  }

  replace_vals <- paste("'", stringr::str_c(newHRUnames,
                                       collapse = "' '"), "'", sep = '')
  prj[start_line + 1] <- replace_vals
  changed_lines <- end_line - start_line
  if (changed_lines > 1)
    prj <- prj[-((start_line + 2):(end_line))]

  writePrj(prj, outputPrjFile)

  # set new dimensions
  old_dimensions <- readPrjDimensions(inputPrjFile)
  new_dimensions <- old_dimensions[, 2]
  new_dimensions[1] <- length(newHRUnames)
  setPrjDimensions(inputPrjFile = outputPrjFile,
                   dimensionVals = new_dimensions)

  # get number of parameters to change
  prj <- readPrj(inputPrjFile)
  param_loc <- grep("Parameters:", prj, fixed = TRUE)
  prj <- prj[(param_loc + 1):length(prj)]
  parameter_lines <- grep("<", prj, fixed = TRUE)
  num_parameters <- length(parameter_lines)

  omit_parameters <- c("basin_area", "basin_name", "distrib_Route", "HRU_OBS",
                       "obs_elev", "soil_withdrawal", "RUN_END", "RUN_ID",
                       "RUN_START")

  param_names <- prj[parameter_lines]
  # extract parameter names from strings
  param_names_alone <- stringr::str_split_fixed(param_names, fixed(" "), 3)
  param_names_alone <- param_names_alone[, 2]

  for (i in 1:num_parameters) {
    param_name <- param_names[i]
    param_name_alone <- param_names_alone[i]
    if (!param_name_alone %in% omit_parameters) {
       params <- readPrjParameters(inputPrjFile, paramName = param_name)
       new_params <- params[-deletedHRUs]
       result <- replacePrjParameters(inputPrjFile = outputPrjFile,
                        paramName = param_name,
                        paramVals = new_params,
                        oldParamCount = old_dimensions[1, 2],
                        quiet = quiet)
    }
  }

  # do matrix parameters

  if (sum(stringr::str_detect(prj, "distrib_Route")) > 0) {
    route_vals <- readPrjMatrix(inputPrjFile, "distrib_Route")
    # delete specified rows and columns
    new_routevals <- route_vals[-deletedHRUs, -deletedHRUs]
    result <- setPrjMatrix(outputPrjFile, paramName = "distrib_Route",
                 paramMatrix = new_routevals)

  }

  if (sum(stringr::str_detect(prj, "HRU_OBS")) > 0) {
    old_obs_vals <- readPrjParameters(inputPrjFile, "HRU_OBS")
    old_obs_vals <- matrix(old_obs_vals, nrow = length(old_obs_vals) / length(HRUnames),
                             ncol = length(HRUnames))
    new_obs_vals <- old_obs_vals[, -deletedHRUs]

    result <- replacePrjParameters(inputPrjFile = outputPrjFile,
                     paramName = "HRU_OBS",
                     paramVals = new_obs_vals,
                     oldParamCount = length(old_obs_vals))
  }

  if (sum(stringr::str_detect(prj, "obs_elev")) > 0) {
    old_elev_vals <- readPrjParameters(inputPrjFile, "obs_elev")
    old_elev_vals <- matrix(old_elev_vals, nrow = length(old_elev_vals) / length(HRUnames),
                           ncol = length(HRUnames))
    new_elev_vals <- old_elev_vals[, -deletedHRUs]

    result <- replacePrjParameters(inputPrjFile = outputPrjFile,
                     paramName = "obs_elev",
                     paramVals = new_elev_vals,
                     oldParamCount = length(old_elev_vals))
  }

  if (sum(stringr::str_detect(prj, "soil_withdrawal")) > 0) {
    old_withdraw_vals <- readPrjParameters(inputPrjFile, "soil_withdrawal")
    old_withdraw_vals <- matrix(old_withdraw_vals, nrow = length(old_withdraw_vals) / length(HRUnames),
                            ncol = length(HRUnames))
    new_withdraw_vals <- old_withdraw_vals[, -deletedHRUs]

    result <- replacePrjParameters(inputPrjFile = outputPrjFile,
                     paramName = "soil_withdrawal",
                     paramVals = new_withdraw_vals,
                     oldParamCount = length(old_withdraw_vals))
  }

  # log action
  comment <- paste('deletePrjHRUs input file: ', inputPrjFile,
                   ' output file:', outputPrjFile)

  result <- logAction(comment, logfile)
  return(result)
}
