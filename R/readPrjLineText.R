readPrjLineText <- function(prj, searchString){
  # reads in a line from CRHM .prj file
  # searches for a specified string and reads in the line two lines below and parses it
  linenum <- grep(searchString, prj, fixed=TRUE)
  values <- parseText(prj[linenum + 2])
  return(values)
}
