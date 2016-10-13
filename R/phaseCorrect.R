#' Corrects precipitation for undercatch and calculates phase
#'
#' @description Applies an iterative solution to calculate the psychrometric hydrometeor temperature. The hydrometeor temperature is then used to partition the raw precipitation into rainfall and snowfall. The snowfall is adjusted for gauge undercatch, if selected.
#' @param obs Required. A \pkg{CRHMr} data frame of observations.
#' @param Tcol Required. Column number containing the air temperature (not including the datetime). Default is column 1. The values must be in  \eqn{^\circ}{ }C.
#' @param RHcol Required. Column number containing the RH (not including the datetime). Default is column 2. The values must be as percentages.
#' @param Ucol Required. Column number containing the wind speeds (not including the datetime). Default is column 3. The values must be in m/s.
#' @param Pcol Required. Column number containing the precipitaion (not including the datetime). Default is column 4. The values must be in mm.
#' @param RH_type Optional. Set to \code{1} if RH is relative to water, any othe value if relative to ice. Default is \code{1}.
#' @param shield Optional. Set to \code{1} to use Alter shield undercatch correction from MacDonald and Pomeroy (2007). Set to \code{2} to use Alter shield undercatch correction from Smith. Set to any other value for unadjusted precipitation. Default is \code{1}.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If unsuccessful returns \code{FALSE}. If successful, returns the original dataframe with additional columns for the rain, snow, total precipitation and rain ratio.
#' @export
#' @author Phillip Harder
#' @references \cite{Harder, P., and J. Pomeroy (2013), \dQuote{Estimating precipitation phase using a psychrometric energy balance method}, Hydrol. Process., 27(13), 1901-1914, doi:10.1002/hyp.9799.} \cr\cr \cite{Macdonald, J., and J. Pomeroy (2007), \dQuote{Gauge Undercatch of Two Common Snowfall Gauges in a Prairie Environment}, Proc. 64th East. Snow Conf. St. John's, Canada., 119-126.}
#'
#' @examples
#' corrected <- phaseCorrect(BadLake7376, Tcol=1, RHcol=2, Ucol=3, Pcol=5)
phaseCorrect <- function(obs, Tcol=1, RHcol=2, Ucol=3, Pcol=4, RH_type=1, shield=1, quiet=TRUE, logfile=''){
  # check inputs
  if (nrow(obs) == 0){
    cat('Error: missing obs data frame\n')
    return(FALSE)
  }

  obsName <- deparse(substitute(obs))

  # get time interval & set step
  dt <- timestep.hours(obs[1,1], obs[2,1])
  if (dt < 1)
    step <- 1
  else if((dt >= 1) & (dt <= 12))
    step <- 2
  else
    step <- 3

  #Constants
  mw <- 18.01528                    # Molar mass of water [gmol-1]
  Ru <- 0.00831441                  # Universal gas constant [m3kPag-1mol-1K-1]

  #b and c coefficients from Harder and Pomeroy 2013
  b_ini <- c(2.630006,2.50286,2.799856)
  c_ini <- c(0.09336,0.125006,0.293292)

  # get locations of p > 0
  end <- nrow(obs)
  k <- which(obs[,(Pcol+1)] > 0)
  Ta <- obs[k, (Tcol+1)]
  RH <- obs[k, (RHcol+1)]
  u <- obs[k, (Ucol+1)]
  P <- obs[k, (Pcol+1)]

  # Latent Heat corrected to be with respect to Ta [J kg-1]
  L <- 1000*(2501-(2.361*Ta))
  L[which(Ta<0)] <- 1000*(2834.1-0.29*Ta[which(Ta<0)]-0.004*Ta[which(Ta<0)]^2)


  Ti <- matrix(ncol=1,nrow=length(Ta),NA)

  # RH correction to ice switch
  if(RH_type == 1){
    RH[which(Ta < 0)] <- buck(RH[which(Ta < 0)], Ta[which(Ta < 0)])
  }


  #Ti Iterative Solution
  for(i in 1:length(Ta)){
    Tai <- Ta[i]
    RHi <- RH[i]
    Li <- L[i]
    Ti1 <- Tai-5.0001            #Initial guess of Ti
    crit <- 99999                #Initial critical value for while loop
    while(crit > 0.000001){
      Ti2 <- Ti_calc(Tai, Ti1, Li, mw, Ru, RHi)
      crit <- abs(Ti1-Ti2)
      Ti1 <- Ti2
    }
    Ti[i] <- Ti1
  }

  #Timestep switch
  b <- b_ini[step]
  c <- c_ini[step]

  if (!quiet){
    cat('Time interval: ', dt, ' hours\n', sep='')
    cat('Model time step: ', step,'\n', sep='')
    cat('b: ', b, '\n', sep='')
    cat('c: ', c, '\n', sep='')
  }

  #round to 3 decimal places
  ratio_rain <- round(1/(1+b*c^Ti),3)
  Rain <- ratio_rain * P

  #undercatch switch
  #geonor-alter shield undercatch according to MacDonald Pomeroy 2007
  if(shield==1){
    Snow <- abs(ratio_rain-1)*P/(1.01*exp(-0.09*u))
  }
  #geonor-alter shield undercatch according to Smith
  else if(shield==2){
    Snow <- abs(ratio_rain-1)*P/(1.18*exp(-0.18*u))
  }
  #no underatch
  else{
    Snow<-abs(ratio_rain-1)*P
  }
  total_precip <- Rain + Snow

  obs$phaseCorrectRain <- 0
  obs$phaseCorrectRain[k] <- Rain

  obs$phaseCorrectSnow <- 0
  obs$phaseCorrectSnow[k] <- Snow

  obs$phaseCorrectTotalPrecip <- 0
  obs$phaseCorrectTotalPrecip[k] <- total_precip

  obs$phaseCorrectRainRatio <- NA_real_
  obs$phaseCorrectRainRatio[k] <- ratio_rain

  # output info to screen and write to log file
  obs.info <- CRHM_summary(obs)
  if (!quiet)
    print(obs.info)

  comment <- paste('phaseCorrect dataframe:', obsName, sep='')
  result <- logAction(comment, logfile)
  if (result)
    return(obs)
  else
    return(result)

}
