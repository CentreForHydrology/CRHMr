#' Processes CRHM wetland (depressional-storage) model outputs
#' @name wetland-methods
#'
#' @description These methods are used to process output from CRHM models
#' which contain many HRUs functioning as depressionals storage,
#' particularly where the Pothole Cascade Model (PCM) is implemented. Note that
#' the term \emph{pond} refers to the water within a depression.
#' \describe{
#'  \item{wetland_h_s}{Calculates h and s parameters for the H-v relationship}
#'  \item{wetland_pond_area}{Estimates the area(s) of pond(s) from the H-v relationship}
#'  \item{wetland_pond_depth}{Estimates the max depth(s) of pond(s) from the H-v relationship}
#'  \item{wetland_pond_size}{Estimates the sizes(depths, areas and perimeters) of all ponds
#'  for each time step from CRHM output}
#' }

NULL
