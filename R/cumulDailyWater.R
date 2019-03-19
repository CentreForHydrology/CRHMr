#' Cumulative water fluxes
#' @description Accumulates water fluxes by year or water year. The values are cumulative for each day.
#' @param dailyWaterVals Required. CRHM daily water fluxes as calculated by \code{simpleDailyWater}.
#' @param waterYear Optional. If \code{FALSE} the fluxes are accumlated by calendar year. If \code{TRUE} the fluxes
#' are accumulated by the water year, based on the first month of the model output.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If successful, returns a data frame containing the date and the cumulative values of each variable.
#' If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @note The output from this function can be plotted using \code{simpleRibbonPlot}.
#' @seealso \code{\link{simpleDailyWater}} \code{\link{simpleRibbonPlot}}
#' @export
#'
#' @examples \dontrun{
#' waterYearCumul <- cumulDailyWater(CRHM_daily, waterYear=TRUE)
#' }
cumulDailyWater <- function(dailyWaterVals, waterYear=FALSE, logfile=''){

  # check parameters
  if(is.null(dailyWaterVals)){
    cat('Error: missing water data frame\n')
    return(FALSE)
  }
  dailyWaterValsName <- deparse(substitute(dailyWaterVals))


  year <- as.numeric(format(dailyWaterVals$date, format='%Y'))
  month <- as.numeric(format(dailyWaterVals$date, format='%m'))
  data_vars <- names(dailyWaterVals)[-1]
  num_cols <- ncol(dailyWaterVals) - 1
  data_cols <- seq(2,num_cols+1)

  if (!waterYear){
    # accumulate by calendar year
    all_years <- unique(year)
    dailyWaterVals$year <- as.numeric(format(dailyWaterVals$date, format='%Y'))
    dailyWaterVals$daynum <- as.numeric(format(dailyWaterVals$date, format='%j'))

    # now aggregate
    for (year in all_years){
      yearly <- dailyWaterVals[dailyWaterVals$year == year,]
      yearly_vals <- yearly[,data_cols]
      cumul <- cumsumDataframe(yearly_vals)
      cumul <- data.frame(yearly$date, cumul)

      if(year == all_years[1])
        all_agg <- cumul
      else
        all_agg <- rbind(all_agg, cumul)
    }
    names(all_agg)[1] <- 'date'

  }
  else{
    # accumulate by water year
    startMonth <-  as.numeric(format(dailyWaterVals$date[1], format='%m'))
    hYear <- year
    month <- as.numeric(format(dailyWaterVals$date, format='%m'))
    hYear[month < startMonth] <- hYear[month < startMonth] -1
    dailyWaterVals$hyear <- hYear

    # now calculate day number
    dailyWaterVals$daynum <- as.numeric(format(dailyWaterVals$date, format='%j'))

    # get day 1
    day1 <- paste(year,'-',startMonth,'-01', sep='')
    day1 <- as.Date(day1, format='%Y-%m-%d')
    day1_num <- as.numeric(format(day1, format='%j'))

    # get last day of year
    last_day <- paste(year,'-12-31', sep='')
    last_day <- as.Date(last_day, format='%Y-%m-%d')
    last_day_num <- as.numeric(format(last_day, format='%j'))

    # figure out day number
    # first, set day number for days after start day
    dailyWaterVals$hdaynum <- dailyWaterVals$daynum
    days_after_day1 <- (dailyWaterVals$hdaynum >= day1_num)

    dailyWaterVals$hdaynum[days_after_day1] <-
      (dailyWaterVals$hdaynum[days_after_day1] - day1_num[days_after_day1]) + 1

    # second, set day number of days before start day
    # find the days to be changed
    days_after_lastday <- (dailyWaterVals$hdaynum == dailyWaterVals$daynum)

    # now find out how much to add

    dailyWaterVals$hdaynum[days_after_lastday] <- dailyWaterVals$hdaynum[days_after_lastday] +
      (last_day_num[days_after_lastday] - day1_num[days_after_lastday]) + 1


    # now aggregate
    all_hyears <- unique(dailyWaterVals$hyear)

    for (hyear in all_hyears){
      yearly <- dailyWaterVals[dailyWaterVals$hyear == hyear,]
      yearly_vals <- yearly[,data_cols]
      cumul <- cumsumDataframe(yearly_vals)
      cumul <- data.frame(yearly$date, cumul)
      if(hyear == all_hyears[1])
        all_agg <- cumul
      else
        all_agg <- rbind(all_agg, cumul)
    }
    names(all_agg)[1] <- c('date')

  }

  comment <- paste('cumulDailyWater dailyWaterVals:', dailyWaterValsName,
                   'waterYear:',
                   waterYear, sep='')
  result <- logAction(comment, logfile)
  if (result)
    return (all_agg)
  else
    return(result)

}
