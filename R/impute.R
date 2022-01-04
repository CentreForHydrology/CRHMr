#' Imputes CRHM obs values
#'
#' @description This function fills in missing (\code{NA}) values in a \pkg{CRHMr} obs
#' data frame by imputation. The primary values are the data that will be infilled. The
#' secondary values are those used to fill in the gaps. The secondary values are adjusted
#' using the specified multipliers (regression slopes) and offsets (regression intercepts).
#' @param primaryObs Required. The primary \pkg{CRHMr} data frame of obs values.
#' @param primaryCols Optional. A vector containing the columns to be imputed in the primary
#' data frame, not including the datetime. If not specified, defaults to the first column.
#' @param secondaryObs Required. The secondary CRHM obs data frame.
#' @param secondaryCols Optional. A vector containing the columns to be imputed in the
#' secondary data frame, not including the datetime. If not specified, defaults to the first column.
#' @param multipliers Optional. A vector of multipliers applied to secondary observations.
#' These may be obtained using the regress function. The default value is \code{1}.
#' @param offsets Optional. A vector of offsets added to secondary observations. These may
#' be obtained using the regress function. The default value is \code{0}.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are
#' calling this function in an \R script, you will usually leave \code{quiet=TRUE}
#' (i.e. the default). If you are working interactively, you will probably want to set
#' \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If successful returns a modified version of the \code{primaryObs} data frame.
#' The missing values in the primary data frame are replaced by corresponding values from
#' the secondary data frame, after adjustment using the specified multipliers and offsets.
#' If unsuccessful, returns an error.
#' @author Kevin Shook
#' @note In addition to the usual notation in the logfile, this function also writes a
#' separate logfile which summarizes the primaryObs data frame, and the new infilled data
#' frame. The summaries are also printed to the screen, if quiet=\code{FALSE}. The logfile
#' also contains a complete listing of the source of each value in the infilled data frame.
#' Each value is listed as being \option{original} (unmodified from the \code{primaryObs}
#'  data frame), \option{imputed} (derived from the adjusted values of the \code{secondaryObs}
#'  data frame) or \option{NA} (missing).
#' @seealso  \code{\link{interpolate}} \code{\link{regress}} \code{\link{distributeP}}
#' @examples
#' \dontrun{
#' v.filled <- impute(
#'   veg, c(1, 2, 3), st,
#'   c(2, 3, 1), c(0.895, 0.708, 1.1209), c(-0.8128, 0.0607, 9.005)
#' )
#' }
#' @importFrom stringr str_c
#' @export

impute <- function(primaryObs, primaryCols = 1,
             secondaryObs, secondaryCols = 1,
             multipliers = 1, offsets = 0, quiet = TRUE, logfile = "") {
    # fills gaps in the primary CRHM Obs dataset from the secondary dataset

    if (nrow(primaryObs) == 0) {
      stop("Missing primary obs values")
    }

    if (nrow(secondaryObs) == 0) {
      stop("Missing secondary obs values")
    }

    primaryName <- deparse(substitute(primaryObs))
    secondaryName <- deparse(substitute(secondaryObs))

    # check for required parameters
    primaryCols <- primaryCols + 1
    secondaryCols <- secondaryCols + 1
    primaryCols.with.time <- c(1, primaryCols)
    secondaryCols.with.time <- c(1, secondaryCols)

    primary.columns.length <- length(primaryCols)
    secondary.columns.length <- length(secondaryCols)

    primaryObs.length <- ncol(primaryObs)
    secondaryObs.length <- ncol(secondaryObs)

    if (primary.columns.length != secondary.columns.length) {
      stop("Different numbers of columns specified")
    }

    if (primary.columns.length > primaryObs.length) {
      stop("More columns specified than exist in primary file")
    }

    if (secondary.columns.length > secondaryObs.length) {
      stop("More columns specified than exist in secondary file")
    }

    primary.variable.names <- names(primaryObs)[primaryCols]
    rh.loc.num <- length(grep("rh", tolower(primary.variable.names), fixed = TRUE))

    if (rh.loc.num > 0) {
      stop("Can't impute to RH data in primary obs")
    }

    secondary.variable.names <- names(secondaryObs)[secondaryCols]
    rh.loc.num <- length(grep("rh", tolower(secondary.variable.names), fixed = TRUE))

    if (rh.loc.num > 0) {
      stop("Can't impute from RH data in secondary obs")
    }

    # recycle multipliers and offsets
    if (length(multipliers) < length(primaryCols)) {
      # replicate
      multipliers <- rep(multipliers, len = length(primaryCols))
    }
    if (length(offsets) < length(primaryCols)) {
      # replicate
      offsets <- rep(offsets, len = length(primaryCols))
    }

    # select columns
    primaryObs.selected <- primaryObs[, primaryCols.with.time]
    secondaryObs.selected <- secondaryObs[, secondaryCols.with.time]

    # scale secondary time series
    colCount <- ncol(secondaryObs.selected)
    for (colNum in 2:colCount) {
      secondaryObs.selected[, colNum] <- secondaryObs.selected[, colNum] * multipliers[(colNum - 1)] + offsets[(colNum - 1)]
    }

    # merge data frames together
    merged <- merge(primaryObs.selected, secondaryObs.selected, by = "datetime", all.x = TRUE)
    merged.na <- is.na(merged)

    # select secondary values, only when primary=NA
    merged.primary <- data.frame(merged[, c(2:(primary.columns.length + 1))])
    merged.primary.na <- is.na(merged.primary)
    merged.secondary <- data.frame(merged[, c((primary.columns.length + 2) :
    (primary.columns.length * 2 + 1))])
    merged.secondary.na <- is.na(merged.secondary)
    merged.primary[merged.primary.na] <- merged.secondary[merged.primary.na]
    merged.final.na <- is.na(merged.primary)

    # re-attach date and time
    output.data <- cbind(merged[, 1], merged.primary)
    names(output.data) <- c("datetime", primary.variable.names)

    # indicate which values are original and which are imputed

    output.type <- matrix(
      data = "original", nrow = nrow(merged.primary),
      ncol = ncol(merged.primary)
    )
    output.type <- as.data.frame(output.type, stringsAsFactors = FALSE)

    output.type[(merged.primary.na & !merged.secondary.na)] <- "imputed"
    output.type[merged.final.na] <- "NA"
    output.type <- cbind(merged[, 1], output.type)
    names(output.type) <- names(output.data)

    # now output to logfiles
    original.data.info <- CRHM_summary(primaryObs)
    new.data.info <- CRHM_summary(output.data)


    comment <- paste("impute primaryObs:", primaryName,
      " primary_variables:",
      str_c(primary.variable.names, collapse = ","),
      " secondaryObs:", secondaryName,
      " secondary_variables:",
      str_c(secondary.variable.names, collapse = ","),
      sep = ""
    )

    result <- logAction(comment, logfile)

    comment1 <- paste("Data infilled by imputation:", primaryName, sep = "	")
    comment2 <- paste("Data imputed from: ", secondaryName, sep = "	")
    comment3 <- paste("multipliers:", str_c(multipliers, collapse = ","), sep = "	")
    comment4 <- paste("offsets:", str_c(offsets, collapse = ","), sep = "	")

    action <- "impute"
    result <- writeChangeLogFile(
      action, original.data.info, new.data.info,
      output.type, comment1, comment2, comment3,
      comment4, quiet
    )
    if (result) {
      return(output.data)
    } else {
      return(result)
    }
  }
