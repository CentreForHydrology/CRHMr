buck <- function(RH,Ta){
  buck_RH <- abs(100*((RH/100)*0.61121*exp((17.502*Ta)/(240.97 + Ta)))/(-0.61115*exp((22.452*Ta)/(272.55+Ta))))
  return(buck_RH)
}
