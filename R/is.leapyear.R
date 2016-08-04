is.leapyear <- function(year){
  #http://en.wikipedia.org/wiki/Leap_year
  #http://quantitative-ecology.blogspot.ca/2009/10/leap-years.html
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}