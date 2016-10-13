Ti_calc <- function(Tai, Ti1, Li, mw, Ru, RHi){
  Ti2 <- Ti1 - ff(Tai,Ti1, Li, mw, Ru, RHi) / fp(Tai,Ti1, Li, mw, Ru, RHi)
  return(Ti2)
}
