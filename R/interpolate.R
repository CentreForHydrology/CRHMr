#' Fills missing obs values by interpolation
#'
#' @description Missing (\code{NA}) values in a \pkg{CRHMr} obs data frame are filled by
#' linear or spline interpolation. The user can set the maximum gap length (in time steps)
#' allowed to be filled. Note that this function will \emph{NOT} interpolate RH or
#' precipitation values.
#' @param obs Required. The \pkg{CRHMr} data frame of obs values.
#' @param varcols Optional. A vector containing the columns to be imputed in the obs
#' data frame, not including the datetime. If not specified, defaults to the first column.
#' @param methods Optional. A vector containing the methods to be used for interpolation
#' for each of the variables. Currently supported methods are \option{linear} and
#' \option{spline}. The default is to use linear interpolation. If fewer methods than
#' columns are specified, the methods are recycled.
#' @param maxlength Optional. The maximum gap length to be interpolated. Defaults to 5 time steps.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you
#' are calling this function in an \R script, you will usually leave \code{quiet=TRUE}
#' (i.e. the default). If you are working interactively, you will probably want to set
#' \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If successful, returns a modified version of the \code{obs} data frame.
#' The missing values in the specified data frame are replaced by interpolated values.
#' If unsuccessful, returns an error.
#' @author Kevin Shook
#' @note In addition to the usual notation in the logfile, this function also
#' writes a separate logfile which summarises the original obs data frame, and the
#' new infilled data frame. The summaries are also printed to the screen, if
#' \code{quiet=FALSE}. The logfile also contains a complete listing of the source
#' of each value in the infilled data frame. Each value is listed as being
#' \option{original} (unmodified from the primary obsframe), \option{linear interpolation}
#' (infilled by linear interpolation), \option{spline interpolation}
#' (infilled by spline interpolation) or \option{NA} (missing).
#' @seealso  \code{\link{impute}}  \code{\link{distributeP}}
#' @examples
#' \dontrun{
#' v.filled <- interpolate(v, c(1, 3))
#' }
#' @importFrom zoo na.spline na.approx
#' @importFrom stringr str_c
#' @export

interpolate <- function(obs, varcols = 1, methods = "linear", maxlength = 5,
             quiet = TRUE, logfile = "") {
    # check parameters
    if (nrow(obs) == 0) {
      stop("No obs values")
    }
    obsName <- deparse(substitute(obs))

    varcols <- varcols + 1
    variable.names <- names(obs)[varcols]
    rh.loc.num <- length(grep("rh", tolower(variable.names), fixed = TRUE))

    if (rh.loc.num > 0) {
      stop("Can't interpolate RH data")
    }

    p.loc.num <- length(grep("p", tolower(variable.names), fixed = TRUE))
    if (p.loc.num > 0) {
      if (!quiet) {
        cat("Warning: you should only interpolate cumulative precip data\n")
      }
    }

    # get selected variables
    varcols.with.time <- c(1, varcols)
    obs <- obs[, varcols.with.time]

    first.datetime <- obs$datetime[1]
    last.datetime <- obs$datetime[nrow(obs)]
    # now trim missing values at beginning and end
    dt <- timestep.hours(obs[2, 1], obs[1, 1])

    if (dt == 24) {
      # daily
      clean <- na.omit(obs)
      last.row <- nrow(clean)
      first.clean.datetime <- clean$datetime[1]
      last.clean.datetime <- clean$datetime[last.row]
      trimmed <- obs[(obs$datetime >= first.clean.datetime) &
        (obs$datetime <= last.clean.datetime), ]
    }
    else {
      # non-daily
      clean <- na.omit(obs)
      last.row <- nrow(clean)
      first.clean.datetime <- clean$datetime[1]
      last.clean.datetime <- clean$datetime[last.row]
      trimmed <- obs[(obs$datetime >= first.clean.datetime) &
        (obs$datetime <= last.clean.datetime), ]
    }

    # generate time synthetic series from beginning to end
    datetime <- seq(from = first.clean.datetime, to = last.clean.datetime, by = dt * 3600)

    # create data frame of times and merge variables into it
    filled <- as.data.frame(datetime)
    filled <- merge(filled, trimmed, by = "datetime", all = TRUE)

    # see if worth doing
    filled.good <- na.omit(filled)
    if (nrow(filled) == nrow(filled.good)) {
      stop("Error: NO missing values")
    }

    # get interpolation methods
    method <- tolower(methods)
    if (length(methods) < length(varcols)) {
      # replicate
      methods <- rep(methods, len = length(varcols))
    }

    # interpolate columns according to methods

    spline.cols <- which(methods == "spline") + 1
    linear.cols <- which(methods == "linear") + 1
    output.data <- filled
    output.type <- matrix(data = " ", nrow = nrow(filled), ncol = ncol(filled))
    output.type <- as.data.frame(output.type, stringsAsFactors = FALSE)
    output.type[, 1] <- filled[, 1]
    names(output.type) <- names(filled)
    if (length(spline.cols) > 0) {
      splines.original <- filled[, spline.cols]
      splines.type <- output.type[, spline.cols]
      splines.filled <- as.data.frame(na.spline(splines.original,
        maxgap = maxlength,
        na.rm = TRUE
      ))

      # figure out type of data now in data frame
      original.na <- is.na(splines.original)
      filled.na <- is.na(splines.filled)

      splines.type[!original.na] <- "original"
      splines.type[xor(original.na, filled.na)] <- "spline interpolation"
      splines.type[filled.na] <- "NA"

      output.data[, spline.cols] <- splines.filled
      output.type[, spline.cols] <- splines.type
    }

    if (length(linear.cols) > 0) {
      linears.original <- filled[, linear.cols]
      linears.type <- output.type[, linear.cols]
      linears.filled <- as.data.frame(na.approx(linears.original,
        maxgap = maxlength, na.rm = TRUE
      ))

      # figure out type of data now in data frame
      original.na <- is.na(linears.original)
      filled.na <- is.na(linears.filled)

      linears.type[!original.na] <- "original"
      linears.type[xor(original.na, filled.na)] <- "linear interpolation"
      linears.type[filled.na] <- "NA"

      output.data[, linear.cols] <- linears.filled
      output.type[, linear.cols] <- linears.type
    }

    # now reassemble obs data, inclufing missing rows at beginning and end
    if (first.clean.datetime != first.datetime) {
      first.bad <- obs[(obs$datetime < first.clean.datetime), ]
      output.data <- rbind(first.bad, output.data)
      first.type <- first.bad
      first.type[, -1] <- NA_real_
      output.type <- rbind(first.type, output.type)
    }

    if (last.clean.datetime != last.datetime) {
      last.bad <- obs[(obs$datetime > last.clean.datetime), ]
      last.type <- last.bad
      last.type[, -1] <- NA_real_
      output.data <- rbind(output.data, last.bad)
      output.type <- rbind(output.type, last.type)
    }
    # output log files
    obs.info <- CRHM_summary(obs)
    if (!quiet) {
      print(obs.info)
    }

    comment <- paste("interpolate dataframe:", obsName,
      " Variables:", str_c(names(filled)[-1], collapse = ","),
      " methods:", str_c(methods, collapse = ","),
      " maxlength:", maxlength,
      sep = ""
    )

    result <- logAction(comment, logfile)


    original.data.info <- CRHM_summary(obs)
    new.data.info <- CRHM_summary(output.data)

    comment1 <- paste("Data infilled by interpolation:", obsName, sep = "	")
    comment2 <- paste("Variables:", str_c(names(filled)[-1], collapse = ","), sep = "	")
    comment3 <- paste("Interpolation method:", str_c(methods, collapse = ","), sep = "	")
    comment4 <- paste("Max interpolation distance:", maxlength, sep = "	")
    action <- "interpolate"
    result <- writeChangeLogFile(
      action, original.data.info, new.data.info, output.type,
      comment1, comment2, comment3, comment4, quiet
    )

    if (result) {
      return(output.data)
    } else {
      return(result)
    }
  }
