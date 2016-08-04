readPrjLine <- function(prj, searchString){
  # reads in a line from CRHM .prj file
  # searches for a specified string and reads in the following line and parses it
  linenum <- grep(searchString, prj, fixed=TRUE)
  values <- parseNums(prj[linenum + 1])
  return(values)
}