writePrj <- function(prj, prjFile){
  eol_val <- win.eol()
  con <- file(prjFile, "w", blocking = FALSE)
  writeLines(prj, con, sep=eol_val)
  close(con)
}
