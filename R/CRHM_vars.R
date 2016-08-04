#' CRHM variables
#'
#' A data frame containing information about all of the variables in CRHM that are likely to be output from a model run.
#'
#' @format A data frame with 674 rows and 5 variables:
#' \describe{
#'   \item{name}{name of the variable}
#'   \item{type}{type of variable (its dimensions)}
#'   \item{description}{description of the variable}
#'   \item{units}{variable units}
#'   \item{end_of_day}{\code{TRUE} if the value is only output at the end of the day (00:00), otherwise \code{FALSE}}
#' }
#' @source The variable information was abstacted from the \code{CRHM} c++ source code.
"CRHM_vars"
NULL