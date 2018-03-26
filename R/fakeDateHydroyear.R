fakeDateHydroyear <- function(dates, fakeYear=2000, startMonth=10) {
  fakeYear1 <- fakeYear - 1
  fakeYear2 <- fakeYear
  month <- as.numeric(format(dates, format = "%m"))

  hYear <- rep(fakeYear1, length(dates))
  hYear[month < startMonth] <- fakeYear2

  fake_dates <- format(dates, format = "%m-%d")
  fake_dates <- paste(hYear, "-", fake_dates, sep = "")
  fake_dates <- as.Date(fake_dates, format = "%Y-%m-%d")
  return(fake_dates)
}
