#' Adds a specified HRU to .prj file
#'
#' @description Adds a single HRU to a .prj file. Note
#' that this function is potentially destructive, so it's a good idea to have a backup copy
#' of your \code{.prj} file \emph{before} running it!
#' @param inputPrjFile Required. Name of the \code{.prj} file to read.
#' @param numOldHRUs. Optional. If not specified (the default), the number of HRUs in the existing
#' file will be read from the .prj file. If you are reading a REWGrp file, where the
#' specified number of HRUs is actually the number of groups, then you should specify this
#' value.
#' @param addHRUnumber Required. Number of HRU to be inserted. All existing HRUs with numbers
#' greater than or equal to this number will be incremented by one.
#' @param addHRUname  Required. Name for the added HRU.
#' @param copyHRUvalues Optional. If specified, values from this HRU will be used
#' for the new HRU. Note that the value of this parameter refers to the the HRU numbers
#' \emph{before} the addition of the new HRU. So it would be possible to have
#' \code{addHRUnmber = 5}, and \code{copyHRUvalues = 5}. This would add a new HRU at
#' position 5, and would set the parameters to be those of the existing HRU 5.
#' @param outputPrjFile Optional. If omitted, the input \code{.prj} file will be overwritten.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling
#' this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default).
#' If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used
#'
#' @return If successful, returns \code{TRUE}. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @seealso  \code{\link{deletePrjHRUs}}
#' @export
#' @examples \dontrun{
#' result <- addPrjHRU("CRHM/Bad74_Frozen.prj", c(3, 5, 7)}
#'
addPrjHRUs <- function(inputPrjFile = NULL,
                          numOldHRUs = NULL,
                          addHRUnumber = NULL,
                          addHRUname = NULL,
                          copyHRUvalues = NULL,
                          outputPrjFile = NULL,
                          quiet = TRUE, logfile = "") {
  # check parameters
  if (is.null(inputPrjFile))
    stop("Missing CRHM input .prj file name")

  if (is.null(addHRUnumber))
    stop("No HRU number specified")

  if (is.null(addHRUname))
    stop("No HRU name specified")

  if (is.null(outputPrjFile))
    outputPrjFile <- inputPrjFile

  # get HRU names
  oldHRUnames <- readPrjHRUnames(inputPrjFile)

  # check to ensure that new HRU number is OK

  if ((addHRUnumber < 1) | (addHRUnumber > (numOldHRUs+1)))
    stop("Invalid HRU number specified")

  # add names of selected HRU
  newHRUnames <- append(oldHRUnames, addHRUname, after = addHRUnumber)

  # read in prj file and find end of parameters
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
    if (numvals >= length(oldHRUnames))
      done <- TRUE
  }

  replace_vals <- paste("'", stringr::str_c(newHRUnames,
                                       collapse = "' '"), "'", sep = '')
  prj[start_line + 1] <- replace_vals
  changed_lines <- end_line - start_line
  if (changed_lines > 1)
    prj <- prj[-((start_line + 2):(end_line))]

  writePrj(prj, outputPrjFile)

  # set new dimensions - if REW_Grp not present
  REW_Grp_found <- sum(str_detect(prj, "REW_Grp"))

  if (REW_Grp_found > 0) {
    old_dimensions <- readPrjDimensions(inputPrjFile)
    num_old_HRUs <-  old_dimensions[1, 2]
    new_dimensions <- old_dimensions[, 2]
    new_dimensions[1] <- length(newHRUnames)
    setPrjDimensions(inputPrjFile = outputPrjFile,
                     dimensionVals = new_dimensions)
  }



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
       if (!is.null(copyHRUvalues))
         new_params <- params[copyHRUvalues]
       else
         new_params <- 0

       new_params <- append(params, new_params, after = addHRUnumber)
       result <- replacePrjParameters(inputPrjFile = outputPrjFile,
                        paramName = param_name,
                        paramVals = new_params,
                        oldParamCount = old_dimensions[1, 2],
                        quiet = quiet)
    }
  }

  # do matrix parameters

  if (sum(stringr::str_detect(prj, "distrib_Route")) > 0) {
    old_route_vals <- readPrjMatrix(inputPrjFile, "distrib_Route")
    num_new_HRUs <- length(newHRUnames)
    new_routevals <- matrix(nrow = num_new_HRUs, ncol = num_new_HRUs)

   # add new row
    new_routevals <- apply(old_route_vals, 1, append, values = 0, after = addHRUnumber)

   # add new col
    if (addHRUnumber == 1)
      new_routevals <- cbind(0, old_route_vals)
    else if(addHRUnumber >= num_old_HRUs)
      new_routevals <- cbind(old_route_vals, 0)
    else {
      left <- new_routevals[, 1:(addHRUnumber - 1)]
      right <- new_routevals[, addHRUnumber:ncol(new_routevals)]
      new_routevals <- cbind(left, 0, right)
    }



    result <- setPrjMatrix(outputPrjFile, paramName = "distrib_Route",
                 paramMatrix = new_routevals)

  }

  if (sum(stringr::str_detect(prj, "HRU_OBS")) > 0) {
    old_obs_vals <- readPrjParameters(inputPrjFile, "HRU_OBS")
    old_obs_vals <- matrix(old_obs_vals, nrow = (length(old_obs_vals) / numOldHRUs),
                             ncol = numOldHRUs)

    if (!is.null(copyHRUvalues))
      copy_col <- old_obs_vals[, copyHRUvalues]
    else
      copy_col <- rep.int(1, numOldHRUs)


    if (addHRUnumber < num_old_HRUs) {
      new_obs_vals <- cbind(copy_col, old_obs_vals)
    } else if(addHRUnumber >= num_old_HRUs) {
        new_obs_vals <- cbind(old_obs_vals, copy_col)
    }   else {
         left <- old_obs_vals[, 1:(addHRUnumber - 1)]
         right <- new_routevals[, addHRUnumber:ncol(new_routevals)]
         new_obs_vals <- cbind(left, copy_col, right)
    }




    result <- replacePrjParameters(inputPrjFile = outputPrjFile,
                     paramName = "HRU_OBS",
                     paramVals = new_obs_vals,
                     oldParamCount = numOldHRUs)
  }

  if (sum(stringr::str_detect(prj, "obs_elev")) > 0) {
    old_elev_vals <- readPrjParameters(inputPrjFile, "obs_elev")
    old_elev_vals <- matrix(old_elev_vals, nrow = length(old_elev_vals) / length(oldHRUnames),
                           ncol = length(oldHRUnames))

    if (!is.null(copyHRUvalues))
      copy_col <- old_elev_vals[, copyHRUvalues]
    else
      copy_col <- rep.int(-1, length(oldHRUnames))

    if (addHRUnumber < num_old_HRUs) {
      new_obs_elev_vals <- cbind(copy_col, old_elev_vals)
    } else if(addHRUnumber >= num_old_HRUs) {
      new_obs_elev_vals <- cbind(old_elev_vals, copy_col)
    } else {
      left <- old_obs_vals[, 1:(addHRUnumber - 1)]
      right <- old_routevals[, addHRUnumber:ncol(new_routevals)]
      new_obs_elev_vals <- cbind(left, copy_col, right)
    }



    result <- replacePrjParameters(inputPrjFile = outputPrjFile,
                     paramName = "obs_elev",
                     paramVals = new_obs_elev_vals,
                     oldParamCount = numOldHRUs)

  }

  if (sum(stringr::str_detect(prj, "soil_withdrawal")) > 0) {
    old_withdraw_vals <- readPrjParameters(inputPrjFile, "soil_withdrawal")
    old_withdraw_vals <- matrix(old_withdraw_vals, nrow = length(old_withdraw_vals) / length(oldHRUnames),
                            ncol = length(oldHRUnames))

    if (!is.null(copyHRUvalues))
      copy_col <- old_withdraw_vals[, copyHRUvalues]
    else
      copy_col <- rep.int(-1, length(oldHRUnames))


    if (addHRUnumber < num_old_HRUs) {
      new_withdraw_vals <- cbind(copy_col, old_withdraw_vals)
    } else if(addHRUnumber >= num_old_HRUs) {
        new_withdraw_vals <- cbind(old_withdraw_vals, copy_col)
    } else {
        left <- old_withdraw_vals[, 1:(addHRUnumber - 1)]
        right <- old_withdraw_vals[, addHRUnumber:ncol(old_withdraw_vals)]
        new_withdraw_vals <- cbind(left, copy_col, right)
    }


    result <- replacePrjParameters(inputPrjFile = outputPrjFile,
                     paramName = "soil_withdrawal",
                     paramVals = new_withdraw_vals,
                     oldParamCount = numOldHRUs)
  }

  # log action
  comment <- paste('deletePrjHRUs input file: ', inputPrjFile,
                   ' output file:', outputPrjFile)

  result <- logAction(comment, logfile)
  return(result)
}
