#' Aggregates simple CRHM model output flows to daily values
#'
#' @description Calculates daily values of the CRHM outputs of water storages and fluxes, for models \emph{without} sub-basins, for each HRU by its area, and calculates the net fluxes for the basin and each sub-basin. Note that this function requires a built-in dataframe \code{CRHM_vars} which contains information about \code{CRHM} variables. If your model uses a variable which is not in the dataframe, then you will get an error message.
#' @param CRHMoutput Required. The CRHM model output as a standard \pkg{CRHMr} dataframe (obs, export or output data)
#' @param vars Optional. Variable column numbers to be used (not including the \code{datetime}). The default \option{all} selects all columns.
#' @param prjFile Required. The CRHM .prj file.
#' @param basinMean Optional. If \code{TRUE} (the default) then the fluxes and storages will be reported as depths over the entire basin. If \code{FALSE}, then fluxes and storages are reported as depths over each HRU's area.
#' @param summarize Optional. If \code{TRUE} (the default), then a single basin-wide value of each flux and storage is returned. If \code{FALSE} then the values of the fluxes and storages are returned for each HRU.
#' @param units Optional. The units for output. The default is \option{mm}, but \option{m3/s} can also be specified.
#' @param quiet Optional. Suppresses display of messages, except for errors. The default is TRUE. If you are calling this function in an R script, you will usually leave quiet=TRUE (i.e. the default). If you are working interactively, you will probably want to set quiet=FALSE.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If successful returns a data frame containing the daily values of all of the water fluxes and storages.
#' @author Kevin Shook
#' @seealso \code{\link{cumulDailyWater}}
#' @export
#'
#' @examples \dontrun{
#' b <- readOutputFile('Bologna1984_30y_output.txt', timezone='MST')
#' # get daily values without aggregation; values calculated w.r.t the HRU area
#' dailyHRU <- simpleDailyWater(b, prjFile='Bologna1984_02.prj', basinMean=FALSE)
#'
#' # get daily values without aggregation; values calculated w.r.t the basin area
#' dailyHRU_per_basin <- simpleDailyWater(b, prjFile='Bologna1984_02.prj', basinMean=TRUE)
#'
#' get daily basin values
#' daily_basin <- simpleDailyWater(b, prjFile='Bologna1984_02.prj', basinMean=TRUE, summarize=TRUE)
#'
#' }
simpleDailyWater <- function(CRHMoutput, vars="all", prjFile="", basinMean=TRUE,
                             summarize=TRUE, units="mm", quiet=TRUE, logfile="") {
  # check parameters
  if (nrow(CRHMoutput) == 0) {
    cat("Error: missing model output\n")
    return(FALSE)
  }
  output_name <- deparse(substitute(CRHMoutput))

  # read in .prj file
  if (prjFile == "") {
    cat("Error: no specified model .prj file\n")
    return(FALSE)
  }
  prj <- readPrj(prjFile)

  # get dimensions
  model_dims <- prjDimensions(prj)
  hru_count <- model_dims[1]
  if (!quiet) {
    cat(hru_count, " HRUs found in ", prjFile, "\n", sep = "")
  }

  # subset dataframe
  if (length(vars) == 1) {
    if (vars == "all") {
      var_cols <- seq(2, ncol(CRHMoutput))
      CRHMoutput <- CRHMoutput
    }
    else {
      var_cols <- vars + 1
      CRHMoutput <- CRHMoutput[, c(1, var_cols)]
    }
  }
  else {
    var_cols <- vars + 1
    CRHMoutput <- CRHMoutput[, c(1, var_cols)]
  }

  # create variable dataframe
  variable_name <- names(CRHMoutput)[-1]
  if (length(variable_name) == 1) {
    CRHM_values <- data.frame(CRHMoutput[, -1])
    names(CRHM_values) <- variable_name
    variable_count <- 1
  }
  else {
    CRHM_values <- CRHMoutput[, -1]
    variable_count <- ncol(CRHM_values)
  }

  if (!quiet) {
    cat(variable_count, " variables found\n", sep = "")
  }

  variable_col <- seq(from = 2, to = (variable_count + 1))
  agg_cols <- seq(from = 1, to = variable_count)

  parsed <- stringr::str_split_fixed(variable_name, stringr::fixed("."), 2)
  variable_hru_num <- as.numeric(parsed[, 2])
  variable_name <- parsed[, 1]
  variables <- data.frame(variable_name, variable_col, variable_hru_num)

  # add variables which are not used for simple .prj
  variables$groupName <- NA_character_
  variables$groupArea <- NA_real_

  # find if file contains GRPs
  REW_line_num <- grep("REW_Grp", prj, fixed = TRUE) + 1

  if (length(REW_line_num) == 0) {
    REW_group_count <- 0
  } else {
    REW_line <- prj[REW_line_num]
    REW_group_count <- suppressWarnings(parseNums(REW_line))
    if (!quiet) {
      cat(REW_group_count, " REW groups found - they will be ignored\n", sep = "")
    }
  }


  if (REW_group_count > 0) {
    # parse names  outflow@A(1)
    parsed_variables <- as.numeric(stringr::str_split(
      variables$variable_name,
      stringr::fixed("@"), 2
    ))
    variables$groupName <- parsed_variables[2]
  }


  # create hru dataframe
  hru_name <- readPrjTextVals(prj, "hru_names", hru_count)


  # simple dataframe
  basin_area <- readPrjLine(prj, "basin_area")
  hru_area <- readPrjNumVals(prj, "hru_area", hru_count)
  hru_num <- seq(from = 1, to = hru_count)

  hru_basin_frac <- hru_area / basin_area
  hrus <- data.frame(hru_num, hru_name, hru_area, hru_basin_frac)
  hrus$group_name <- NA_character_
  hrus$group_frac <- NA_real_


  # classify variables so they can be aggregated
  # add units and type to each variable
  CRHM_vars <- CRHM_vars
  CRHM_variables <- CRHM_vars
  variables <- merge(variables, CRHM_variables, by.x = "variable_name", by.y = "name")

  # connect variables to their HRU attributes
  variables <- merge(variables, hrus, by.x = "variable_hru_num", by.y = "hru_num")
  variables$end_of_day <- as.logical(variables$end_of_day)

  # fix BASIN variables
  basin_vars <- stringr::str_detect(variables$type, stringr::fixed("BASIN"))
  cols <- variable_col[basin_vars]

  variables$variable_hru_num[basin_vars] <- NA_integer_
  variables$hru_name[basin_vars] <- NA_character_
  variables$hru_area[basin_vars] <- basin_area
  variables$hru_basin_frac[basin_vars] <- 1

  # convert flow vars to m3/s
  flow_vars <- stringr::str_detect(variables$units, stringr::fixed("m^3/int"))
  interval <- timestep.hours(CRHMoutput$datetime[1], CRHMoutput$datetime[2])
  cols <- variable_col[flow_vars] - 1
  CRHM_values[, cols] <- CRHM_values[, cols] * (interval / 3600)
  variables$units[flow_vars] <- "m^3/s"

  # find cumulative variables
  cumul_vars1 <- stringr::str_detect(variables$variable_name, stringr::fixed("cum"))
  cumul_vars2 <- stringr::str_detect(variables$description, stringr::fixed("cum"))
  cumul_vars <- cumul_vars1 | cumul_vars2

  # deaccumulate all cumulative variables
  if (sum(cumul_vars) > 0) {
    cumul_cols <- variable_col[cumul_vars]
    cumul_col_count <- length(cumul_cols)

    for (i in 1:cumul_col_count) {
      colnum <- cumul_cols[i] - 1
      vals <- CRHM_values[, colnum]
      deaccum <- c(vals[1], diff(vals))
      CRHM_values[, colnum] <- deaccum
    }
  }
  variables$units[cumul_vars] <- paste("deaccum_", variables$units[cumul_vars], sep = "")

  # re-count variables in case some names were not identified
  variable_count <- nrow(variables)
  agg_cols <- seq(from = 1, to = variable_count)

  # figure out aggregation functions
  variables$agg_functions <- rep.int("mean", variable_count)

  # instantaneous flow variables
  flow_vars <- stringr::str_detect(variables$units, stringr::fixed("m^3/s"))
  flow_cols <- agg_cols[flow_vars]
  variables$agg_functions[flow_cols] <- "mean"

  # depth/interval variables
  depth_vars <- stringr::str_detect(variables$units, stringr::fixed("mm"))
  depth_cols <- agg_cols[depth_vars]
  variables$agg_functions[depth_cols] <- "sum"

  depth_vars <- stringr::str_detect(variables$units, stringr::fixed("kg/m^2*int"))
  depth_cols <- agg_cols[depth_vars]
  variables$agg_functions[depth_cols] <- "sum"

  # SWE variables - averaged over each period
  depth_vars <- stringr::str_detect(variables$variable_name, stringr::fixed("SWE"))
  depth_cols <- agg_cols[depth_vars]
  variables$agg_functions[depth_cols] <- "mean"

  # glacier ice variables - averaged over each period
  depth_vars <- stringr::str_detect(variables$variable_name, stringr::fixed("glacier_h2o"))
  depth_cols <- agg_cols[depth_vars]
  variables$agg_functions[depth_cols] <- "mean"

  # daily variables
  daily_vars <- stringr::str_detect(variables$units, stringr::fixed("/d"))
  daily_cols <- agg_cols[daily_vars]
  variables$agg_functions[daily_cols] <- "mean"

  # end of day variables
  endday_vars <- variables$end_of_day
  variables$agg_functions[endday_vars] <- "sum"

  # now aggregate all variables

  # simple, no groups
  # add datetimes to CRHM_values
  CRHM_values <- cbind(CRHMoutput$datetime, CRHM_values)
  names(CRHM_values)[1] <- "datetime"

  # figure out which columns to sum and which to average
  num_sum <- sum(variables$agg_functions == "sum")
  num_mean <- sum(variables$agg_functions == "mean")
  num_endday <- sum(variables$endday_vars)

  count_agg_cols <- 0

  if (num_sum > 0) {
    sum_cols <- (variables$agg_functions == "sum")
    sum_cols_num <- variables$variable_col[sum_cols] - 1
    CRHM_agg_sum <- aggDataframe(CRHM_values,
      columns = sum_cols_num,
      period = "daily",
      funs = "sum"
    )
    agg_sum_names <- names(CRHM_agg_sum)
    count_agg_cols <- length(sum_cols_num)
    agg_dates <- CRHM_agg_sum[, 1]
  }

  if (num_mean > 0) {
    mean_cols <- (variables$agg_functions == "mean")
    mean_cols_num <- variables$variable_col[mean_cols] - 1
    CRHM_agg_mean <- aggDataframe(CRHM_values,
      columns = mean_cols_num,
      period = "daily",
      funs = "mean"
    )
    agg_mean_names <- names(CRHM_agg_mean)
    count_agg_cols <- count_agg_cols + length(mean_cols_num)
    agg_dates <- CRHM_agg_mean[, 1]
  }



  # now reassemble data
  CRHM_agg <- data.frame(agg_dates)
  names(CRHM_agg) <- "date"

  for (colnum in 1:count_agg_cols) {
    # get variable info
    var_name <- variables$variable_name[colnum]
    var_hru_num <- variables$variable_hru_num[colnum]
    if (is.na(var_hru_num)) {
      var_hru_num <- "1"
    }

    var_hru_function <- variables$agg_functions[colnum]

    # assemble name
    agg_var_name <- paste(var_name, ".", var_hru_num, ".", var_hru_function, sep = "")

    # lookup data
    if (var_hru_function == "mean") {
      values <- data.frame(CRHM_agg_mean[, agg_var_name])
      names(values) <- agg_var_name
      CRHM_agg <- cbind(CRHM_agg, values)
    }
    else {
      values <- data.frame(CRHM_agg_sum[, agg_var_name])
      names(values) <- agg_var_name
      CRHM_agg <- cbind(CRHM_agg, values)
    }
  }


  # look for daily mean values output at end of the day and shift back one day

  if (num_endday > 0) {
    num_rows <- nrow(CRHM_agg)
    agg_values <- CRHM_agg[, -1]

    original <- agg_values[, endday_vars]
    agg_values[1:(num_rows - 1), endday_vars] <- original[2:num_rows, ]
    agg_values[num_rows, endday_vars] <- NA_real_
    CRHM_agg[, -1][, endday_vars] <- agg_values
  }


  # do unit conversions and weighting
  # convert mm*km2 to mm
  area_vars <- stringr::str_detect(variables$units, stringr::fixed("mm*km^2"))
  cols <- variable_col[area_vars]
  areas <- variables$hru_area[cols]
  CRHM_agg[, cols] <- CRHM_agg[, cols] / areas
  # update units
  variables$units[area_vars] <- stringr::str_replace(
    variables$units[area_vars],
    stringr::fixed("mm*km^2"),
    "mm"
  )

  # convert energy units to depths
  sublimation_vars <- stringr::str_detect(variables$variable_name, stringr::fixed("Ge_ebsm"))
  cols <- variable_col[sublimation_vars]
  CRHM_agg[, cols] <- CRHM_agg[, cols] * 0.3527337
  variables$units[area_vars] <- stringr::str_replace(
    variables$units[area_vars],
    stringr::fixed("MJ/d"),
    "mm"
  )

  if (units == "mm") {
    # convert all units to mm over HRU
    flow_vars <- stringr::str_detect(variables$units, stringr::fixed("m^3/s"))

    basin_flow_vars <- basin_vars & flow_vars
    hru_flow_vars <- !basin_vars & flow_vars

    basin_flow_count <- sum(basin_flow_vars)
    hru_flow_count <- sum(hru_flow_vars)

    # check process hru and basin variables
    if (hru_flow_count > 0) {
      cols <- variable_col[hru_flow_vars]
      # this is wrong - needs a loop here
      for (colnum in cols)
        CRHM_agg[, colnum] <- CRHM_agg[, colnum] * 24 * 3600 * 1000 /
          (variables$hru_area[colnum] * 1e6)
      variables$units[flow_vars] <- "mm"
    }
    if (basin_flow_count > 0) {
      cols <- variable_col[basin_flow_vars]
      for (colnum in cols)
        CRHM_agg[, colnum] <- CRHM_agg[, colnum] * 24 * 3600 * 1000 /
          (basin_area * 1e6)

      variables$units[flow_vars] <- "mm"
      names(CRHM_agg)[mean_cols_num + 1] <- names(CRHM_agg_mean)[-1]
    }
  }
  else {
    # convert all units to m3/s
    depth_vars <- stringr::str_detect(variables$units, stringr::fixed("mm"))
    cols <- variable_col[depth_vars]
    for (colnum in cols)
      CRHM_agg[, colnum] <- (CRHM_agg[, colnum] / 1000) *
        (variables$hru_area[colnum] * 1e6) / 3600

    variables$units[flow_vars] <- "m^3/s"
  }

  # calculate fluxes by basin
  if (basinMean) {
    # convert depth to average over basin
    if (units == "mm") {
      # convert all units to basin average mm
      cols <- seq(2, ncol(CRHM_agg))
      for (colnum in cols)
        CRHM_agg[, colnum] <- CRHM_agg[, colnum] * variables$hru_basin_frac[colnum - 1]
      variables$units[flow_vars] <- "mm"
    }
  }

  if (summarize) {
    all_var_names <- unique(variables$variable_name)
    all_name_count <- length(all_var_names)
    summary_df <- data.frame(CRHM_agg$date)

    # get all variables with same name
    for (i in 1:all_name_count) {
      var_name <- as.character(all_var_names[i])
      var_col <- (variables$variable_name == var_name)
      var_col_num <- agg_cols[var_col]
      selected <- CRHM_agg[, var_col_num + 1]
      if (length(var_col_num) > 1) {
        total <- rowSums(selected)
      } else {
        total <- selected
      }
      summary_df <- cbind(summary_df, total)
      names(summary_df)[i + 1] <- var_name
    }
    names(summary_df)[1] <- "date"
    daily_water <- summary_df
  }
  else {
    daily_water <- CRHM_agg
  }


  # return values

  comment <- paste("simpleDailyWater CRHMoutput:", output_name,
    " prjFile:", prjFile, " basinMean:", basinMean,
    " summarize:", summarize, " units:", units,
    sep = ""
  )
  result <- logAction(comment, logfile)

  if (result) {
    return(daily_water)
  } else {
    return(result)
  }
}
