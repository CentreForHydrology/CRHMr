#' @title Contains functions to perform pre- and post- processing on data used with the Cold Regions Hydrological Modelling (CRHM) platform
#' @name CRHMr-package
#'
#' @description
#' All data in \pkg{CRHMr}, whether model input or output must be stored in the standard
#' type of data frame. The file input functions (\code{\link{readObsFile}}
#' \code{\link{readExportFile}} \code{\link{readOutputFile}}) will automatically create
#' standard \pkg{CRHMr} data frames. If you are reading data from another type of
#' file using standard \R functions, then you will have to force it to be in the
#' standard format.\cr The first column of the data frame is labeled
#' \code{datetime}, and as is the date and time stored as a \code{POSIXct} value,
#' with the correctly-specified time zone. The only exception is for daily CRHM
#' data or for aggregated values which use a daily or longer time step. In these
#' cases the first column will have the appropriate name.\cr
#'
#' Because CRHM allows there to be many values of a variable, each variable will
#' have a trailing number
#' such as \code{.1}. This is added automatically when importing CRHM data. Most
#' of the functions allow you to select a variable by its column number. In all
#' cases, the column number does \emph{NOT} include the datetime column.\cr
#' To make your research more reproducible, each function writes information about
#' what it did, including the date and time it was run, to a log file (
#' \code{CRHMr.log}) in the default directory. It is suggested that you also run
#' the function \code{\link{user}} when you first start to use \pkg{CRHMr} as it
#' writes information about your computer to the log file. This may be helpful
#' when trying to figure out bugs. Please send the output of your log file
#' (including the \code{user} output) when reporting bugs.\cr
#'
#' The package contains functions to do the following:
#' \enumerate{
#'   \item Read in CRHM data into a \pkg{CRHMr} data frame. This includes data
#'   from .obs files (observation data used by CRHM), and model run outputs,
#'   either output automatically, or manually exported.
#'   \item Manipulate obs data. Includes functions to plot the values, to find
#'   missing values, or datetimes, and to infill gaps by interpolation (linea or
#'   spline) and by imputation from other datasets.
#'   \item Convert Ea values to RH and vice-versa. Interpolation and imputation
#'   require Ea values, as RH values depend on the air temperature. For safety,
#'   \pkg{CRHMr} functions do not permit both Ea and RH values in a data frame,
#'   as it would be impossible to know which was correct.
#'   \item Write a data frame data to a CRHM obs file.
#'   \item Execute CRHM. This allows CRHM to be run automatically, which is very
#'   useful for doing sensitivity analyses. There are functions to prepare a CRHM
#'   model to be executed, including setting the run start and end dates, and to
#'   run CRHM from inside R.
#'   \item Examine output from CRHM runs. Includes functions to read in model
#'   output and to aggregate, summarize and plot the values.
#' }
#'
#' @references
#' To cite \pkg{CRHMr} in publications, use the command \code{citation('CRHMr')}
#' to get the current version of the citation.\cr
#' The CRHM program is described in:\cr
#' \cite{Pomeroy, John W, D M Gray, T Brown, N Hedstrom, W L Quinton, R J Granger,
#' and S K Carey. 2007. \dQuote{The Cold Regions Hydrological Model : A Platform
#' for Basing Process Representation and Model Structure on Physical Evidence}.
#' Hydrological Processes 21 (19): 2650-2567.}\cr
#' The CRHM model may be downloaded from \url{http://www.usask.ca/hydrology/CRHM.php}.\cr
#'
#' @import ggplot2 scales stringr
#' @importFrom stats aggregate coefficients lm na.omit qqplot sd
#' @importFrom utils glob2rx read.csv read.table sessionInfo type.convert write.csv write.table
#' @importFrom signal sgolayfilt
#' @importFrom lubridate force_tz guess_formats year ceiling_date floor_date is.Date year tz
#' @importFrom plyr ddply
#' @importFrom reshape2 melt
#' @importFrom zoo zoo na.spline na.approx
#'
"_PACKAGE"
