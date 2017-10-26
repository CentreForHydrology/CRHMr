fakeDatetime <- function(datetime, fakeYear=2000){
  fake_datetime <- format(datetime, format='%m-%d %H:%S')
  fake_datetime <- paste(fakeYear, '-', fake_datetime, sep='')
  fake_datetime <- as.POSIXct(fake_datetime, format='%Y-%m-%d %H:%M', tzone='')
  return(fake_datetime)
}
