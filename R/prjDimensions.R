prjDimensions <- function(prj){
  # returns # of HRUs, layers, obs
  dim_line_num <- grep('Dimensions:', prj, fixed=TRUE) 
  nhru_line_num <- dim_line_num + 2
  nlay_line_num <- nhru_line_num + 1
  nobs_line_num <- nlay_line_num + 1
  
  nhru_line <- prj[nhru_line_num]
  nlay_line <- prj[nlay_line_num]
  nobs_line <- prj[nobs_line_num]
  
  # parse
  nhru <- suppressWarnings(parseNums(nhru_line))
  nlay <- suppressWarnings(parseNums(nlay_line))
  nobs <- suppressWarnings(parseNums(nobs_line))
  
  ret <- c(nhru[2], nlay[2], nobs[2])
  return(ret)
}