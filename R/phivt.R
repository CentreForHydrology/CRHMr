#saturated water vapour density (ASHRAE,1993)[gm-3]
phivt <- function(mw,Tai,Ru){
  val <- mw*(0.611*exp((17.3*Tai)/(237.3+Tai)))/(Ru*(Tai+273.15))/1000
  return(val)
}
