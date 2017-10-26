fakeDatetimeHydroyear <- function(datetime, fakeYear=2000, startMonth=10){
  fakeYear1 <-  fakeYear-1
  fakeYear2 <- fakeYear
  month <- as.numeric(format(datetime, format='%m'))

  hYear <- rep(fakeYear1, length(datetime))
  hYear[month < startMonth] <- fakeYear2

  fake_datetime <- format(datetime, format='%m-%d %H:%S')
  fake_datetime <- paste(hYear, '-', fake_datetime, sep='')
  fake_datetime <- as.POSIXct(fake_datetime, format='%Y-%m-%d %H:%M', tzone='')
  return(fake_datetime)
}
