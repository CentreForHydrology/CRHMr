#' Synthetic weighing precipitation gauge data
#'
#' A dataframe containing data for use with the weighingGauge functions.
#'
#' @format A dataframe with 17460 rows and 2 variables (including the datetime):
#' \describe{
#'   \item{datetime}{date and time as a POSIXct object}
#'   \item{p}{cumulative precipitation (mm)}
#' }
#' @source This data was derived from real data provided by Craig Smith. It has been adjusted by incorporating missing values and a simulated gauge reset.
"wg"
NULL