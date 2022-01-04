readPrj <- function(prjFile){
  con <- file(prjFile, "r", blocking = FALSE, encoding="ISO_8859-2")
  prj <- readLines(con)
  close(con)
  return(prj)
}
