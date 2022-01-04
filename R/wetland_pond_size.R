#' Estimates pond area, depth and perimeter (circular)
#'
#' @param CRHM_output Required. Data frame of CRHM outputs including Sd values for all wetland HRUs.
#' @param HRU_name Required. Names of all HRUs. Can be obtaibes using \code{readPrjParameters}.
#' @param HRU_SD_max Required. SDmax values for all HRUs. Can be obtaibes using \code{readPrjParameters}.
#' @param HRU_area Required. Areas of all HRUs. Can be obtaibes using \code{readPrjParameters}.
#' @param wetland_HRU Optional. A vector of the number of each wetland HRUs.
#' @param area_p_vals Optional. A data frame containing two columns. The first column is the
#' a wetland area (in m\eqn{^2}{^2}), the second column is the corresponding value of \code{p} to use.
#' If this parameter is not specified, then the default values will be used. The default is to use
#' \code{p = 1.72} for areas greater than 0 and smaller than 10,000 m\eqn{^2}{^2}, and \code{p = 3.3}
#' for areas greater than or equal to 10,000 m\eqn{^2}{^2}. These values were estimated for
#' Smith Creek basin.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @author Kevin Shook
#' @family wetland_functions
#'
#' @return If successful, returns a data frame consisting of the \code{datetime} (or \code{date}), and
#' the pond area (m\eqn{^2}{^2}), depth (m), and perimeter (m) (assuming the pond is circular) for
#' each time step in \code{CRHM_output}.
#' @note Because this function may be slow and generates a very large quantity of output, it
#' is a good idea to restrict the range of \code{CRHM_output} \emph{before} calling the function.
#' @export
#'
#' @examples \dontrun{
#' CRHM_output <- readOutputFile("CRHM_output_1.txt", "Etc/GMT+6")
#' HRU_name <- readPrjHRUnames("SmithCreek.prj")
#' HRU_SD_max <- readPrjParameters("SmithCreek.prj", "Sdmax")
#' HRU_area <- readPrjParameters("SmithCreek.prj", "hru_area")
#' area <- c(0, 5000, 10000)
#' p <- c(1.4, 1.7, 3.3)
#' area_p_vals <- data.frame(area, p)
#' pond_sizes <- wetland_pond_size(CRHM_output, HRU_name, HRU_SD_max, HRU_area, area_p_vals)}
wetland_pond_size <- function(CRHM_output = NULL,
                              HRU_name = NULL,
                              HRU_SD_max = NULL,
                              HRU_area = NULL,
                              wetland_HRU = NULL,
                              area_p_vals = NULL,
                              logfile = ""
                              ) {


  # check for missing parameters
  if (is.null(CRHM_output)) {
    cat('Error: CRHM_output is missing\n')
    return(FALSE)
  }

  if (is.null(HRU_name)) {
    cat('Error: HRU_name is missing\n')
    return(FALSE)
  }

  if (is.null(HRU_SD_max)) {
    cat('Error: HRU_SD_max is missing\n')
    return(FALSE)
  }

  if (is.null(HRU_area)) {
    cat('Error: HRU_areas is missing\n')
    return(FALSE)
  }

  # if wetland_HRUs not specified, infer them from the names
  if (is.null(wetland_HRU)) {
    wetland_HRUs <- grep( "wetland", HRU_name,ignore.case = TRUE)
  }

  CRHMname <- deparse(substitute(CRHM_output))

  # get wetland HRU dimensions
  wetland_Sdmax <- HRU_SD_max[wetland_HRUs]                 # mm
  wetland_area <- HRU_area[wetland_HRUs] * 1e6              # km2 -> m2
  wetland_volume <- (wetland_Sdmax / 1000) * wetland_area      # m3
  wetland_dimensions <- data.frame(wetland_Sdmax, wetland_area, wetland_volume)
  num_wetlands <- nrow(wetland_dimensions)

  # if area_pvals not specified, use defaults
  if (is.null(area_p_vals)) {
    area <- c(0, 10000)
    p <- c(1.72, 3.33)
    area_p_vals <- data.frame(area, p)
  } else {
    names(area_p_vals) <- c("area", "p")
  }

  wetland_dimensions$p <- area_p_vals$p[findInterval(wetland_dimensions$wetland_area, area_p_vals$area) ]

  wetland_dimensions$h <- NULL
  wetland_dimensions$s <- NULL

  # solve for h and S for all wetlands with volume > 0
  for (i in 1: num_wetlands) {
    if ((wetland_dimensions$wetland_volume[i] > 0) & (wetland_dimensions$wetland_area[i] > 0)) {
      h_s <- wetland_h_s(wetland_dimensions$wetland_volume[i],
                         wetland_dimensions$wetland_area[i],
                         wetland_dimensions$p[i])
      wetland_dimensions$h[i] <- h_s[1]
      wetland_dimensions$s[i] <- h_s[2]
    } else {
      wetland_dimensions$h[i] <- 0
      wetland_dimensions$s[i] <- wetland_dimensions$wetland_area[i]
    }
  }

  # get the wetland sd values

  Sd_vals <- grep("Sd", names(CRHM_output), ignore.case = TRUE)
  all_sd_vals <- CRHM_output[, Sd_vals]

  # get the wetland Sd values only
  wetland_sd_vals <- all_sd_vals[wetland_HRUs]
  intervals <- nrow(CRHM_output)

  # create data structure to hold pond area, and depth
  wetland_pond_depths <- matrix(nrow = intervals, ncol = num_wetlands)
  wetland_pond_areas <- wetland_pond_depths
  wetland_pond_volumes <- wetland_pond_depths

  output_names <- names(CRHM_output)

  # calculate the area and depth for each interval
 numcols <- ncol(wetland_sd_vals)
 for (col in 1:numcols) {
    wetland_pond_volumes[, col] <- (wetland_sd_vals[, col] / 1000) * wetland_area[col]
    wetland_pond_depths[, col] <- wetland_pond_depth(pond_volume = wetland_pond_volumes[, col],
                                                    p = wetland_dimensions$p[col],
                                                    s = wetland_dimensions$s[col])

    wetland_pond_areas[, col] <- wetland_pond_area(pond_depth = wetland_pond_depths[, col],
                                                   p = wetland_dimensions$p[col],
                                                   s = wetland_dimensions$s[col])
  }


  # now assemble output
  wetland_pond_perimeters <- 2 * sqrt(wetland_pond_areas * pi)

  all_values <- data.frame(CRHM_output[, 1], wetland_pond_areas, wetland_pond_depths, wetland_pond_perimeters)


  pond_nums <- seq(1:num_wetlands)
  pond_area_names <- paste("pond_area.", pond_nums, sep = "")
  pond_depth_names <- paste("pond_depth.", pond_nums, sep = "")
  pond_perimeter_names <- paste("pond_perimeter.", pond_nums, sep = "")
  all_names <- c(names(CRHM_output)[1], pond_area_names, pond_depth_names, pond_perimeter_names)
  names(all_values) <- all_names

  comment <- paste('wetland_pond_size CRHM_output:', CRHMname, sep='')
  result <- logAction(comment, logfile)

  # return either data frame or graph
  if(result)
    return(all_values)
  else
    return(result)
}
