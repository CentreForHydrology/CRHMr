#Newton-Raphston Iteration Function
fp <- function(Tai, Ti1, Li, mw, Ru, RHi){
  T1 <- Ti1 + 0.001*Ti1
  T2 <- Ti1 - 0.001*Ti1
  fp_val <- (ff(Tai,T1, Li, mw, Ru, RHi)-ff(Tai,T2, Li, mw, Ru, RHi))/(0.002*Ti1)
  return(fp_val)
}
