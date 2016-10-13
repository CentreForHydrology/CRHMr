#Diffusivity of water vapour in air [m2 s-1]
Dv <- function(Tai){
  Dv <- 2.06*10^-5*((Tai+273.15)/273.15)^1.75
  return(Dv)
}
