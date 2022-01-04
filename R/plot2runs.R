#' Plots the results from two CRHM runs
#' @description Creates a time series plot facetted by variable of values from
#' two CRHM runs. The same variables must be present in the came columns in
#' both runs.
#'
#' @param run1 Required. Standard \pkg{CRHMr} data frame of output from first run.
#' @param run2 Required. Standard \pkg{CRHMr} data frame of output from second run.
#' @param cols Variable columns to plot. If omitted, all variables are used. Otherwise
#' all columns are specified w.r.t. the \code{datetime}.
#' @param start_datetime Optional. A POSIXct datetime. If specified, only values >=
#' the \code{start_datetime} are plotted.
#' @param end_datetime Optional. A POSIXct datetime. If specified, only values <=
#' the \code{end_datetime} are plotted.
#' @param model1_name Optional. Name of the first model. Default is \option{Model 1}.
#' @param model2_name Optional. Name of the second model. Default is \option{Model 2}.
#'
#' @return Returns ggplot time series plot of the CRHM output, facetted by variable.
#' @export
#' @importFrom ggplot2 ggplot aes geom_line xlab ylab facet_wrap
#' @importFrom reshape2 melt
#' @importFrom stringr str_split_fixed
#'
#' @examples \dontrun{p <- plot2runs("CRHM_run1", "CRHM_run2", cols = c(1, 2, 3))}
#'
plot2runs <- function(run1, run2, cols = NULL, start_datetime = NULL, end_datetime = NULL,
                      model1_name = "Model 1", model2_name = "Model 2") {

  # set up ggplot variables
  datetime <- NULL
  value <- NULL
  model <- NULL
  variable_name <- NULL
  variable <- NULL

  if (!is.list(run1)) {
    cat('Error: missing run1 values\n')
    return(FALSE)
  }

  if (!is.list(run2)) {
    cat('Error: missing run1 values\n')
    return(FALSE)
  }

  if (is.null(cols)) {
    cols = seq(2, (ncol(run1) - 1))
  }

  # select variables for plotting
  run1 <- run1[, c(1, (cols + 1))]
  run2 <- run2[, c(1, (cols + 1))]

  # get names of variables and HRU numbers from CRHM outputs
  var_names <- names(run1)[-1]
  var_pieces <- str_split_fixed(var_names, fixed("."), 3)
  HRU_nums <- var_pieces[, 2]
  variable_name <- var_pieces[1, 1]
  HRU_names <- paste("HRU", HRU_nums)

  # rename data frame columns with revised HRU names
  names(run1)[-1] <- HRU_names
  run1$model <- model1_name
  names(run2)[-1] <- HRU_names
  run2$model <- model2_name

  # assemble data frame for plotting
  both <- rbind(run1, run2)

  # set start and end datetimes if required

  if (!is.null(start_datetime))
    both <- both[(both$datetime >= start_datetime),]

  if (!is.null(end_datetime))
    both <- both[(both$datetime <= end_datetime),]

  melted <- melt(both, id.vars = c("datetime", "model"))

  p <- ggplot(melted, aes(datetime, value, colour = model)) +
    geom_line() +
    xlab("") +
    ylab(variable_name) +
    facet_wrap(~variable, scales = "free_y")

  return(p)
}
