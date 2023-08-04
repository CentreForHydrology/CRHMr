#' Finds values in a obs dataframe column that have a flatline
#'
#' @param obs Required. A \pkg{CRHMr} obs data frame.
#' @param colnum Optional. The number of the column to test for spikes, not
#'   including the datetime.
#' @param window_size Optional. The min number of values within the vector
#'   required to be the same before is flagged as a flat line.
#' @param logfile Optional. Name of the file to be used for logging the action.
#'   Normally not used.
#'
#' @return obs dataframe with col flat lines replaced with NANs
#' @export
#'
#' @author Alex Cebulski
#'
#' @examples
#' findFlatLines(BadLake7376, colnum = 3)
#'
findFlatLines <- function(obs, colnum = 1, window_size = 5, logfile = "") {
  if (nrow(obs) == 0) {
    stop("Missing obs values")
  }
  obsName <- deparse(substitute(obs))

  if (any(is.na(obs[, colnum + 1]))) {
    stop("Missing values. Remove before searching for spikes")
  }

  if (window_size == 0) {
    stop("Missing threshold. Set before searching for spikes")
  }

  vector <- obs[,colnum+1]

  if (length(vector) <= 1) {
    stop("Column does not contain any values")  # A vector with 0 or 1 element cannot have a flat line or repeating values.
  }

  change_count <- 0
  previous_element <- vector[1]
  flat_line_length <- 1  # Initialize the length of the current potential flat line
  flagged_indices <- c()  # Store the indices of repeating values with 5 or more occurrences

  for (i in 2:length(vector)) {
    current_element <- vector[i]

    if (current_element != previous_element) {
      # Reset the flat_line_length if there was a change
      flat_line_length <- 1
      change_count <- change_count + 1
    } else {
      flat_line_length <- flat_line_length + 1

      # Check if the current value is repeating 5 or more times
      if (flat_line_length >= window_size) {
        # Flag the indices of repeating values
        flagged_indices <- c(flagged_indices, (i - flat_line_length + 1):i)
      }
    }

    previous_element <- current_element
  }

  # the above function has duplicates so need to reduce
  flagged_indices_unique <- unique(flagged_indices)

  num_flats <- length(flagged_indices_unique)
  if(num_flats == 0){
    outputMessage <- " The obs col has no flat lines"
    returnvalue <- 0
  }
  else {
    locs <- obs[flagged_indices_unique, 1]
    outputMessage <- paste(" ", num_flats, " flatline vals found", sep = "")
    returnvalue <- locs
  }

  # output to logfile
  comment <- paste("findFlatlines obs:", obsName, outputMessage, sep = "")
  result <- logAction(comment, logfile)

  return(returnvalue)
}
