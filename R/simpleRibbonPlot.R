#' Simple plots of CRHM output max, min and mean values
#'
#' @description Creates \pkg{ggplot2} ribbon plots of the daily minimum, maximum and mean values for all selected variables over a year, for models \emph{without} sub-basins. The shaded area indicates the range of values in the data frame for each day.
#' @param CRHMoutput Required. A data frame containing either instantaneous or cumulative values. The data may be either daily or hourly.
#' @param columns The columns to be aggregated, not including the datetime. The default is the \option{all}, but can be a vector, i.e. c(1,2,3). Note that all variables must have the same units.
#' @param waterYear Optional. If \code{True}, the statistics are calculated and plotted by water year, if \code{FALSE} (the default) the statistics are calculated and plotted by calendar year. If you are plotting cumulative values using \code{cumulDailyWater}, this parameter must be set to be the same as that function.
#' @param ylabel Optional. Label to be used for the y-axis.
#' @param skipLeapDay Optional. If \code{TRUE} (the default) then values for Feb. 29 will be omitted from the analyses and plots. If \code{FALSE}, then these values will be included, which may result in strange-looking plots.
#' @param facet Optional. If \code{FALSE} (the default), then all variables will be on a single plot. If \code{TRUE}, then each variable will be on a separate facet.
#' @param facetColour Optional. The colour to be used for shading the ribbon plot in each facet, if selected.
#' @param facetCols Optional. The number of columns to be used for the facets, if selected.
#'
#' @return If unsuccessful, returns \code{FALSE}. If successful, returns a \pkg{ggplot2} object.
#' @author Kevin Shook
#' @seealso \code{\link{cumulDailyWater}}
#' @export
#'
#' @examples \dontrun{
#' p <- ribbonPlot(waterYearCumul, waterYear=TRUE, units='mm', facet=TRUE, facetCols=2}
simpleRibbonPlot <- function(CRHMoutput, columns='all', waterYear=FALSE,
                       ylabel='Basin total (mm)', skipLeapDay=TRUE, facet=FALSE,
                       facetColour='blue', facetCols=3){
  # declare ggplot variables
  variable <- NULL

  # check parameters
  if(is.null(CRHMoutput)){
    cat('Error: missing CRHM output\n')
    return(FALSE)
  }

  # subset dataframe
  if (length(columns) == 1){
    if (columns != 'all'){
      var_cols <- columns + 1
      CRHMoutput <- CRHMoutput[,c(1,var_cols)]
    }
    else{
      # have to figure out which are the columns to plot
      var_cols <- seq(2, ncol(CRHMoutput))
    }
  }
  else{
    var_cols <- columns + 1
    CRHMoutput <- CRHMoutput[,c(1,var_cols)]
  }

  # check for type of dataframe, i.e. daily/datetime, hydroyear etc.

  col_names <- names(CRHMoutput)
  if (col_names[1] == 'datetime')
    datetime <- TRUE
  else
    datetime <- FALSE

  if (!datetime & !lubridate::is.Date(CRHMoutput$date)){
    # not a date, so convert from datetime to a real date
    CRHMoutput <- datetimeToDate(CRHMoutput)
  }

  # create variable dataframe
  variable_name <- names(CRHMoutput)[-1]
  CRHM_values <- CRHMoutput[,-1]
  variable_count <- ncol(CRHM_values)

  if (skipLeapDay)
    fakeYear <- 2002
  else
    fakeYear <- 2000


  # get fake dates
  if (!waterYear){
    if (datetime)
      fakeCRHMDatetimes <- fakeDatetime(CRHMoutput$datetime, fakeYear)
    else
      fakeCRHMDates <- fakeDate(CRHMoutput$date, fakeYear)
  }
  else{
    startMonth <- as.numeric(format(CRHMoutput[1,1], format='%m'))
    if (datetime)
      fakeCRHMDatetimes <- fakeDatetimeHydroyear(CRHMoutput$datetime,
                                                 fakeYear=fakeYear,
                                                 startMonth = startMonth)
    else
      fakeCRHMDates <- fakeDateHydroyear(CRHMoutput$date, fakeYear=fakeYear,
                                         startMonth = startMonth)
  }

  # find min, max and mean values and plot

  if (!datetime){
    # aggregate by date


    min_vals <- aggregate(CRHM_values, by=list(fakeCRHMDates), FUN='min')
    min_vals <- na.omit(min_vals)
    names(min_vals)[1] <- 'date'
    min_melted <- reshape2::melt(min_vals, id.vars='date')
    names(min_melted)[3] <- 'min'

    mean_vals <- aggregate(CRHM_values, by=list(fakeCRHMDates), FUN='mean')
    mean_vals <- na.omit(mean_vals)
    names(mean_vals)[1] <- 'date'
    mean_melted <- reshape2::melt(mean_vals, id.vars='date')
    names(mean_melted)[3] <- 'mean'

    max_vals <- aggregate(CRHM_values, by=list(fakeCRHMDates), FUN='max')
    max_vals <- na.omit(max_vals)
    names(max_vals)[1] <- 'date'
    max_melted <- reshape2::melt(max_vals, id.vars='date')
    names(max_melted)[3] <- 'max'

    all_cols <- cbind(min_melted, mean_melted$mean, max_melted$max)
    names(all_cols)[4:5] <- c('mean', 'max')

    # now plot

    if (!facet){
      p <-  ggplot2::ggplot(all_cols, ggplot2::aes(x = date, y = mean,
                                                   colour = variable)) +
        ggplot2::geom_line()

      p <- p + ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = min,
                                                           ymax = max, fill = variable),
                                    alpha=0.2)
      p <-  p + ggplot2::scale_x_date(date_labels = "%b %d")
    }
    else{
      p <-  ggplot2::ggplot(all_cols, ggplot2::aes(x = date,
                                                   y = mean, group = variable)) +
        ggplot2::geom_line()
      p <- p + ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = min, ymax = max),
                                    fill=facetColour, alpha=0.4) +
        ggplot2::facet_wrap(~variable, scales='free_y', ncol=facetCols) +
        ggplot2::scale_x_date(date_labels = "%b")
    }

    # set axes
    p <-  p + ggplot2::xlab('')  + ggplot2::ylab(ylabel)

  }

 else{
   # plot against datetime
   # aggregate
   min_vals <- aggregate(CRHM_values, by=list(fakeCRHMDatetimes), FUN='min')
   min_vals <- na.omit(min_vals)
   names(min_vals)[1] <- 'date'
   min_melted <- reshape2::melt(min_vals, id.vars='date')
   names(min_melted)[3] <- 'min'

   mean_vals <- aggregate(CRHM_values, by=list(fakeCRHMDatetimes), FUN='mean')
   mean_vals <- na.omit(mean_vals)
   names(mean_vals)[1] <- 'date'
   mean_melted <- reshape2::melt(mean_vals, id.vars='date')
   names(mean_melted)[3] <- 'mean'

   max_vals <- aggregate(CRHM_values, by=list(fakeCRHMDatetimes), FUN='max')
   max_vals <- na.omit(max_vals)
   names(max_vals)[1] <- 'date'
   max_melted <- reshape2::melt(max_vals, id.vars='date')
   names(max_melted)[3] <- 'max'

   all_cols <- cbind(min_melted, mean_melted$mean, max_melted$max)
   names(all_cols)[4:5] <- c('mean', 'max')

   # now plot

   if (!facet){
     p <-  ggplot2::ggplot(all_cols, ggplot2::aes(x = datetime, y = mean,
                                                  colour = variable)) +
       ggplot2::geom_line()

     p <- p + ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = min,
                                                          ymax = max, fill = variable),
                                   alpha=0.2)
     p <-  p + ggplot2::scale_x_date(date_labels = "%b %d")
   }
   else{
     p <-  ggplot2::ggplot(all_cols, ggplot2::aes(x = datetime,
                                                  y = mean, group = variable)) +
       ggplot2::geom_line()
     p <- p + ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = min, ymax = max),
                                   fill=facetColour, alpha=0.4) +
       ggplot2::facet_wrap(~variable, scales='free_y', ncol=facetCols) +
       ggplot2::scale_x_date(date_labels = "%b")
   }

   # set axes
   p <-  p + ggplot2::xlab('')  + ggplot2::ylab(ylabel)

 }

  return(p)
}
