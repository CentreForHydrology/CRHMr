#' Reads names of modules from .prj files
#'
#' @param prjFile Required. Name of .prj file.
#' @param unique_modules If \code{TRUE}, only unique module names will be
#' returned, when there are groups. If \code{FALSE}, each module will be
#' retuned for each group.
#'
#' @return If successful, returns a data frame containing the columns
#' \code{module}, \code{type}, and \code{date} for each module. If \option{unique_modules} =
#' \code{TRUE}, then the variable \code{group} is also returned for each module.
#' @author Kevin Shook
#' @seealso \code{\link{readPrjModuleParams}}
#' @import stringr
#' @export
#'
#' @examples \dontrun{
#' modules <- readPrjModuleNames("Bad74_Frozen.prj")
#' }

readPrjModuleNames <- function(prjFile, unique_modules = TRUE) {
  # check parameter values
  if (prjFile == '' | is.null(prjFile)) {
    cat('Missing CRHM input .prj file name\n')
    return(FALSE)
  }

  # read in .prj file
  prj <- readPrj(prjFile)

  # extract modules
  # get rid of lines before "Modules:"
  module_start_line <- grep("Modules:", prj, fixed = TRUE) + 1
  prj <- prj[-(1:module_start_line)]

  # get rid of lines after modules
  module_end_line <- min(grep("##", prj, fixed = TRUE)) - 1
  prj <- prj[1:module_end_line]


  # check to see if multiple groups are present
  if (length(grep("+", prj, fixed = TRUE)) > 0) {
    # multiple groups are present, check to see if all modules are returned together
    if (unique_modules) {
      module_lines <- grep("+", prj, fixed = TRUE)
      module_str <- prj[module_lines]

      # trim leading space and # from all lines
      module_str <- str_trim(module_str, side = "left")
      module_str <- str_remove(module_str, fixed("+"))

      # now spilt
      modules <- str_split_fixed(module_str, " ", 3)
      modules <- data.frame(modules, stringsAsFactors = FALSE)
      names(modules) <- c("module", "type", "date")

      # get unique modules
      modules <- modules[!duplicated(modules), ]

    } else  {
      # return modules for each group
      #get groups
      group_lines <- grep("+", prj, fixed = TRUE, invert = TRUE)
      group_str <- prj[group_lines]

      # trim leading space and # from all lines
      group_str <- str_trim(group_str, side = "left")

      # now spilt
      groups <- str_split_fixed(group_str, " ", 3)[,1]

      # generate group names for all modules
      line_nums <- c(group_lines, (module_end_line + 1))
      line_count <- diff(line_nums)
      line_count <- line_count - 1
      module_groups <- rep.int(groups, line_count)

      # get modules
      module_lines <- grep("+", prj, fixed = TRUE)
      module_str <- prj[module_lines]

      # trim leading space and # from all lines
      module_str <- str_trim(module_str, side = "left")
      module_str <- str_remove(module_str, fixed("+"))

      # now spilt
      modules <- str_split_fixed(module_str, " ", 3)
      modules <- data.frame(modules, stringsAsFactors = FALSE)
      names(modules) <- c("module", "type", "date")
      modules <- cbind(module_groups, modules)
      names(modules)[1] <- "group"

    }

  } else {
    # single group
    modules <- str_split_fixed(prj, " ", 3)
    modules <- data.frame(modules, stringsAsFactors = FALSE)
    names(modules) <- c("module", "type", "date")
  }

  return(modules)
}
