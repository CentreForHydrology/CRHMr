#' Reads all parameters for all HRUs
#'
#' @param moduleName Required. A text pattern of the module name. For the Albedo
#' Richards module it would be \option{albedo_Richard}.
#' @param prjFile Required. Name of .prj file.
#'
#' @return A data frame with the names of the parameters, and the values
#' of the parameters for each HRU.
#' @author Ezequiel Toum
#' @export
#'
#' @examples \dontrun{
#' fileName <- "Vuriloche.prj"
#' shared_parameters <- readPrjModuleParams(moduleName = 'Shared',
#' prjFile = fileName)}

readPrjModuleParams <- function(moduleName, prjFile) {
  # check parameter values
  if (prjFile == '' | is.null(prjFile)) {
    cat('Missing CRHM input .prj file name\n')
    return(FALSE)
  }

  if (moduleName == '' | is.null(moduleName)) {
    cat('Missing CRHM module name\n')
    return(FALSE)
  } else {
    moduleName <- paste(moduleName, "*", sep = "")
  }

  # read in .prj file
  prj <- readPrj(prjFile)

  # get number of HRUs
  dims <- prjDimensions(prj)
  numHRU <- dims[1]

  # get rid of lines before "Parameters:"
  param_start_line <- grep("Parameters:", prj, fixed = TRUE) + 1
  prj <- prj[-(1:param_start_line)]

  # get rid of lines after parameters
  param_end_line <- min(grep("#", prj, fixed = TRUE)) - 1
  prj <- prj[1:param_end_line]

  index   <- grep(pattern = moduleName, x = prj, ignore.case = FALSE)
  Naux    <- c(diff(index) - 1, 1)
  distr   <- if (sum(Naux) > length(index)) {TRUE} else {FALSE}
  rm(index, Naux)

  if (distr) {
    # inicio parámetros distribuidos en capas
    if (moduleName != 'Shared*' | moduleName != 'Ayers*') {
      index   <- grep(pattern = moduleName, x = prj, ignore.case = FALSE)
      Naux    <- c(diff(index) - 1, 1) # number of rows to fill per iteration
      N       <- sum(Naux)             # total rows in matrix
      numit   <- length(index)         # number of iterartions

      matriz  <- matrix(NA, nrow = N, ncol = numHRU)
      Parname <- rep('NA', N)

      for (i in 1:numit) {
        j <- index[i]
        nombre <- strsplit(prj[j], '<')[[1]][1]
        mpar   <- matrix(data = readPrjParameters(prjFile = prjFile,
                                                  paramName = nombre),
                         nrow = Naux[i], ncol = numHRU, byrow = TRUE)

        if (i == 1) {
          from <- 1
          to   <- from + (Naux[i] - 1)
        } else {
          from <- to + 1
          to   <- from + (Naux[i] - 1)
        }

        matriz[from:to,  ] <- readPrjParameters(prjFile = prjFile,
                                                paramName = nombre)
        Parname[from:to] <- nombre

      }
      colnames(matriz) <- paste('HRU', seq(1, numHRU), sep = '')
      df <- data.frame(Parname, matriz)
      return(df)

    } else {
      index   <- grep(pattern = moduleName, x = prj, ignore.case = FALSE)
      Naux    <- c(diff(index) - 1, 1) # number of rows to fill per iteration
      N       <- sum(Naux) # total rows in matrix
      numit   <- length(index) # number of iterations

      matriz  <- matrix(NA, nrow = N, ncol = numHRU)
      Parname <- rep('NA', N)

      for (i in 1:numit) {
        j <- index[i]
        nombre <- strsplit(prj[j], '<')[[1]][1]
        mpar   <- matrix(data = readPrjParameters(prjFile = prjFile,
                                                  paramName = nombre),
                         nrow = Naux[i], ncol = numHRU, byrow = TRUE)

        if (i == 1) {
          from <- 1
          to   <- from + (Naux[i] - 1)
        } else {
          from <- to + 1
          to   <- from + (Naux[i] - 1)
        }

        matriz[from:to,  ] <- readPrjParameters(prjFile = prjFile,
                                                paramName = nombre)
        Parname[from:to] <- nombre

      }
      colnames(matriz) <- paste('HRU', seq(1, numHRU), sep = '')
      df <- data.frame(Parname, matriz)
      return(df)
    }
  } else {
    #inicio parámetros con interlineado simple
    if (moduleName != 'Shared*') {
      index   <- grep(pattern = moduleName, x = prj, ignore.case = F)
      N       <- length(index)
      matriz  <- matrix(NA, nrow = N, ncol = numHRU)
      Parname <- rep('NA', N)
      for (i in 1:N) {
        j <- index[i]
        nombre <- strsplit(prj[j], '<')[[1]][1]
        matriz[i,  ] <- readPrjParameters(prjFile = prjFile, paramName = nombre)
        Parname[i] <- nombre

      }
      colnames(matriz) <- paste('HRU', seq(1, numHRU), sep = '')
      df <- data.frame(Parname, matriz)
      return(df)

    } else {
      index   <- grep(pattern = moduleName, x = prj, ignore.case = FALSE)
      N       <- length(index)
      matriz  <- matrix(NA, nrow = N, ncol = numHRU)
      Parname <- rep('NA', N)
      for (i in 1:N) {
        j <- index[i]
        nombre <- strsplit(prj[j], '<')[[1]][1]
        if (i == 1) {
          matriz[i, 1] <- readPrjParameters(prjFile = prjFile,
                                            paramName = nombre)
        } else{
          matriz[i,  ] <- readPrjParameters(prjFile = prjFile,
                                            paramName = nombre)
        }
        Parname[i] <- nombre
      }
      colnames(matriz) <- paste('HRU', seq(1, numHRU), sep = '')
      df <- data.frame(Parname, matriz)
      return(df)
    }
  }
}
