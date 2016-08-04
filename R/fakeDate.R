fakeDate <- function(dates, fakeYear=2000){
  fake_dates <- format(dates, format='%m-%d')
  fake_dates <- paste(fakeYear,'-',fake_dates, sep='')
  fake_dates <- as.Date(fake_dates, format='%Y-%m-%d')
  return(fake_dates)
}